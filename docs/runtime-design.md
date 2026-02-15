# Jet Runtime Design Document

## Overview

This document specifies the runtime system for Jet, a statically-typed, compiled programming language with Rust-level safety, garbage collection, structured concurrency, and an effects system.

## 1. Memory Management

### 1.1 Garbage Collector Selection: Immix

**Decision**: Immix with on-the-fly reference counting for short-lived objects

**Rationale**:
- **Boehm GC**: Conservative collection causes memory fragmentation and prevents precise heap layout control
- **Reference Counting + Cycle Detection**: Predictable latency but high overhead for mutation-heavy workloads
- **Immix**: Modern mark-region collector with:
  - Line-granularity allocation (128 bytes per line)
  - Opportunistic evacuation during collection
  - ~10% fragmentation vs ~30% for Boehm
  - Predictable pause times (sub-millisecond for typical workloads)

**Hybrid Approach**:
```
Object Lifetime Classification:
- Nursery (Immix): Objects < 256KB, < 10ms lifetime
- Mature (Immix): Long-lived objects, evacuated to contiguous regions
- Immortal: Static data, never collected
- RC-fastpath: Objects with single owner, no cycles possible
```

### 1.2 Heap Layout

```
┌─────────────────────────────────────────────────────────────┐
│                     Immortal Space                          │
│         (Static data, interned strings, type descriptors)    │
├─────────────────────────────────────────────────────────────┤
│                     Nursery Space                           │
│    ┌─────────┬─────────┬─────────┬─────────┬─────────┐     │
│    │ Block 0 │ Block 1 │ Block 2 │   ...   │ Block N │     │
│    │ 32KB    │ 32KB    │ 32KB    │         │ 32KB    │     │
│    └─────────┴─────────┴─────────┴─────────┴─────────┘     │
├─────────────────────────────────────────────────────────────┤
│                     Mature Space                            │
│    ┌─────────┬─────────┬─────────┬─────────┬─────────┐     │
│    │ Region  │ Region  │ Region  │   ...   │ Region  │     │
│    │ 256KB   │ 256KB   │ 256KB   │         │ 256KB   │     │
│    └─────────┴─────────┴─────────┴─────────┴─────────┘     │
├─────────────────────────────────────────────────────────────┤
│                     Large Object Space                      │
│              (Objects > 256KB, segregated)                  │
└─────────────────────────────────────────────────────────────┘
```

**Block Structure (32KB)**:
```c
typedef struct {
    uint8_t lines[256];           // 128 bytes per line, mark byte per line
    uint16_t cursor;              // Next free line
    uint16_t free_lines;          // Count of completely free lines
    uint8_t mark_bits[32];        // One bit per line for marking
    uint8_t evacuate_candidates;  // Fragmentation threshold indicator
} ImmixBlock;
```

### 1.3 Object Header Format

Every heap-allocated object has a 16-byte header:

```c
typedef struct {
    // Word 0: Type and GC information
    union {
        struct {
            uint32_t type_id : 24;      // Type descriptor index
            uint32_t gc_bits : 4;       // GC state (white/grey/black/pinned)
            uint32_t rc_bits : 4;       // Refcount fastpath hints
        };
        uint32_t header_word;
    };

    // Word 1: Size and alignment
    uint32_t size;                      // Object size (excluding header)
    uint16_t alignment;                 // Required alignment
    uint16_t flags;                     // Object-specific flags

    // Words 2-3: GC metadata
    union {
        struct {
            void* forwarding;           // Evacuation forwarding pointer
        } gc;
        struct {
            uint32_t strong_refs;       // Strong reference count
            uint32_t weak_refs;         // Weak reference count
        } rc;
    };
} JetObjectHeader;
```

**GC State Machine**:
```
WHITE: Object unreachable in current collection cycle
GREY:  Object reachable, children not yet scanned
BLACK: Object reachable, children scanned
PINNED: Object cannot be moved (e.g., for FFI)
```

### 1.4 Stack Frames

Jet uses a split stack model for M:N threading:

```
┌─────────────────────────────────────┐
│  Return Address                     │
├─────────────────────────────────────┤
│  Saved Frame Pointer                │
├─────────────────────────────────────┤
│  Local Variables (fixed size)       │
│  ┌─────────────────────────────┐   │
│  │ Slot 0: Tagged pointer/value│   │
│  │ Slot 1: Tagged pointer/value│   │
│  │ ...                         │   │
│  └─────────────────────────────┘   │
├─────────────────────────────────────┤
│  Spill Area (for register pressure) │
├─────────────────────────────────────┤
│  Outgoing Arguments                 │
├─────────────────────────────────────┤
│  Stack Map Index                    │  ← For precise GC
└─────────────────────────────────────┘
```

**Stack Map Format**:
```c
typedef struct {
    uint32_t return_address;        // Return address this map applies to
    uint16_t num_entries;           // Number of live pointer slots
    uint16_t frame_size;            // Total frame size
    // Variable-length array of pointer locations
    uint16_t pointer_offsets[];     // Offsets from frame pointer
} StackMapEntry;
```

Stack maps are generated at compile time and stored in a separate section. The runtime uses binary search to find the correct map for a given return address.

### 1.5 Root Set Identification

Roots come from multiple sources:

1. **Registers**: Saved register state during collection
2. **Stack**: Walk each task's stack using stack maps
3. **Global/Static**: Immutable immortal space, scanned once
4. **Thread-Local Storage**: Per-task roots
5. **Conservative Roots**: For FFI boundaries (optional Boehm fallback)

**Root Scanning Algorithm**:
```c
void collect_roots(RootSet* roots) {
    // 1. Stop all mutator threads (STW for root scan)
    suspend_all_tasks();

    // 2. Scan each task's stack
    for (Task* task = all_tasks; task; task = task->next) {
        scan_stack(task, roots);
        scan_registers(task, roots);
    }

    // 3. Scan thread-local handles
    scan_thread_locals(roots);

    // 4. Scan global roots (cached, rarely changes)
    if (globals_dirty) {
        scan_globals(roots);
    }

    resume_all_tasks();  // Mutators can resume after root scan
}
```

### 1.6 Write Barriers

For generational collection, Jet uses a **deferred reference counting** write barrier:

```c
// Inline fast path (no function call)
#define WRITE_BARRIER(obj, field, new_value) do { \
    void* old = (obj)->field; \
    if (IS_HEAP_POINTER(old)) { \
        remember_old_value(old); \
    } \
    (obj)->field = (new_value); \
    if (IS_HEAP_POINTER(new_value)) { \
        remember_new_value(new_value); \
    } \
} while(0)
```

**Remembered Set Implementation**:
- Per-thread remembered set buffers (4KB each)
- Lock-free insertion using atomic operations
- Processed during GC pause or incrementally

### 1.7 Finalizers

Jet supports finalizers with strict ordering guarantees:

```c
typedef struct {
    JetObjectHeader header;
    void (*finalizer)(void* obj);
    uint32_t priority;              // Finalization order (lower = earlier)
    bool resurrect;                 // Allow object resurrection
} FinalizableHeader;
```

**Finalizer Rules**:
1. Finalizers run in a dedicated thread pool (not GC thread)
2. No guarantee about ordering relative to other objects
3. Object is unreachable when finalizer runs (unless resurrected)
4. Finalizer can only run once per object
5. No access to other finalizable objects (isolated)

## 2. Object Representation

### 2.1 Primitive Boxing

Small primitives are immediate values (no allocation):

```c
// Tagged pointer representation (64-bit)
// Bits 0-2: Tag
// Bits 3-63: Payload

#define TAG_MASK     0x7
#define TAG_INT      0x0   // 61-bit signed integer
#define TAG_BOOL     0x1   // true/false
#define TAG_NIL      0x2   // null/nil
#define TAG_FLOAT32  0x3   // 32-bit float (boxed in 61 bits)
#define TAG_CHAR     0x4   // Unicode codepoint
#define TAG_SYMBOL   0x5   // Interned symbol
#define TAG_PTR      0x7   // Heap pointer (8-byte aligned)

// Unboxed 64-bit floats use pointer tagging:
// If low 3 bits are 0b111 and high 16 bits match special pattern,
// it's a boxed float (separate allocation)
```

**Boxing Strategy**:
| Type | Size | Representation |
|------|------|----------------|
| `int` | 61-bit | Immediate, TAG_INT |
| `float32` | 32-bit | Immediate, TAG_FLOAT32 |
| `float64` | 64-bit | Boxed on heap |
| `bool` | 1-bit | Immediate, TAG_BOOL |
| `char` | 32-bit | Immediate, TAG_CHAR |
| `nil` | - | Immediate, TAG_NIL |

### 2.2 String Representation

UTF-8 with small string optimization:

```c
// Small string (≤23 bytes): embedded in pointer
// Large string: heap allocated

typedef struct {
    JetObjectHeader header;
    uint32_t length;            // UTF-8 byte length
    uint32_t char_count;        // Unicode codepoint count (cached)
    uint32_t hash;              // Cached hash (0 = not computed)
    union {
        struct {
            char data[];        // Inline UTF-8 bytes
        } inline_str;
        struct {
            char* data;         // Pointer to external buffer
            size_t capacity;    // Allocated capacity
        } heap_str;
    };
} JetString;

// Small string optimization using pointer tagging:
// If TAG_SYMBOL: length in upper bits, data in pointer
// Can store up to 7 bytes on 64-bit systems with special encoding
```

**String Interning**:
- Compile-time string literals → immortal space
- Runtime interning via hash table for frequently compared strings
- `Symbol` type for guaranteed-interned identifiers

### 2.3 Array Layout

```c
typedef struct {
    JetObjectHeader header;
    uint32_t length;
    uint32_t capacity;
    // Elements follow header
    // Alignment: max(8, element_alignment)
    uint8_t data[];
} JetArray;

// For primitive arrays, elements stored directly
// For reference arrays, elements are pointers

// Array access (bounds checked):
//   base + sizeof(JetArray) + index * element_size
```

**Slice Representation**:
```c
typedef struct {
    JetArray* array;        // Underlying array (RC incremented)
    uint32_t offset;        // Start offset
    uint32_t length;        // Slice length
} JetSlice;
```

### 2.4 Struct Layout

Jet uses C-compatible layout with Rust-like optimizations:

```c
// Layout algorithm:
// 1. Sort fields by alignment (descending)
// 2. Pack fields to minimize padding
// 3. Align total size to max field alignment

typedef struct {
    JetObjectHeader header;     // 16 bytes
    // Fields packed by compiler
} JetStruct;

// Example: struct { a: int64, b: int8, c: int32 }
// Layout: [header:16][a:8][padding:4][c:4][b:1][padding:7]
// Total: 40 bytes (aligned to 8)
```

**Field Reordering**:
- Enabled by default for performance
- Can be disabled per-struct for C interop: `#[repr(C)]`
- Explicit layout: `#[repr(packed)]`, `#[repr(align(N))]`

## 3. Type Information

### 3.1 Runtime Type Identification

Every heap object references a Type Descriptor:

```c
typedef struct {
    uint32_t type_id;               // Unique type identifier
    uint32_t size;                  // Instance size
    uint16_t alignment;             // Required alignment
    uint16_t num_fields;            // Number of reference fields
    uint32_t hash;                  // Type hash for fast comparison

    // Type name (for debugging/reflection)
    JetString* name;

    // GC traversal function
    void (*trace)(void* obj, void (*callback)(void** ptr));

    // Drop/finalizer function
    void (*drop)(void* obj);

    // Method table (for dynamic dispatch)
    MethodTable* methods;

    // Interface implementations
    uint32_t num_interfaces;
    InterfaceImpl* interfaces[];
} TypeDescriptor;
```

### 3.2 Vtables for Dynamic Dispatch

```c
typedef struct {
    uint32_t num_methods;
    void* methods[];                // Function pointers
} MethodTable;

typedef struct {
    uint32_t interface_id;
    MethodTable* vtable;
} InterfaceImpl;

// Dynamic dispatch:
// 1. Load TypeDescriptor from object header
// 2. Binary search interfaces array for interface_id
// 3. Cache result in inline cache (IC) for subsequent calls
```

**Inline Cache (IC) for Monomorphic Calls**:
```c
// Call site structure:
typedef struct {
    TypeDescriptor* cached_type;    // Expected type (NULL = uninitialized)
    void* cached_method;            // Cached method pointer
    uint32_t interface_slot;        // Slot in interface vtable
} CallSiteCache;

// Fast path: 2 comparisons + indirect call
// if (obj->type == cached_type) {
//     cached_method(obj, args...);
// } else {
//     slow_path_lookup(obj, call_site);
// }
```

### 3.3 Trait Object Representation

```c
// Fat pointer representation
typedef struct {
    void* data;                     // Pointer to object
    MethodTable* vtable;            // Trait-specific vtable
} TraitObject;

// Trait vtable layout:
typedef struct {
    uint32_t type_id;               // Concrete type (for downcasting)
    uint32_t size;                  // Size of concrete type
    uint32_t alignment;             // Alignment of concrete type
    void (*drop)(void* data);       // Destructor
    void* methods[];                // Trait methods
} TraitVtable;
```

### 3.4 Type Descriptors for Reflection

```c
typedef struct {
    uint32_t num_fields;
    FieldInfo fields[];
} StructInfo;

typedef struct {
    JetString* name;
    TypeDescriptor* type;
    uint32_t offset;                // Byte offset in struct
    uint32_t flags;                 // const, mut, etc.
} FieldInfo;

// Reflection API (optional, can be stripped):
// - jet_reflect_typeof(obj) -> TypeDescriptor*
// - jet_reflect_fields(type) -> FieldInfo[]
// - jet_reflect_call(method, obj, args)
```

## 4. Concurrency Runtime

### 4.1 M:N Threading Model

Jet uses an M:N (green threads) model:
- M user tasks multiplexed onto N OS threads
- N = number of CPU cores (default)
- Work-stealing scheduler

```
┌─────────────────────────────────────────────────────────┐
│                    OS Thread Pool                       │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐    │
│  │Thread 0 │  │Thread 1 │  │Thread 2 │  │Thread N │    │
│  │┌───────┐│  │┌───────┐│  │┌───────┐│  │┌───────┐│    │
│  ││LocalQ ││  ││LocalQ ││  ││LocalQ ││  ││LocalQ ││    │
│  │└───────┘│  │└───────┘│  │└───────┘│  │└───────┘│    │
│  └─────────┘  └─────────┘  └─────────┘  └─────────┘    │
│       │            │            │            │          │
│       └────────────┴────────────┴────────────┘          │
│                    Global Queue                         │
│              ┌─────────────────┐                        │
│              │  Task Queue     │                        │
│              │  (lock-free)    │                        │
│              └─────────────────┘                        │
└─────────────────────────────────────────────────────────┘
```

### 4.2 Scheduler Design

```c
typedef struct {
    TaskQueue local_queue;          // LIFO for cache locality
    TaskQueue steal_queue;          // FIFO for work stealing
    TimerQueue timers;              // Delayed task scheduling
    uint32_t thread_id;
    bool is_parking;                // About to sleep
} Scheduler;

// Task states:
// RUNNING: Currently executing
// READY:   In queue, waiting for CPU
// BLOCKED: Waiting for I/O, channel, or timer
// PARKED:  Suspended, not in any queue
// DEAD:    Finished execution
```

**Scheduling Algorithm**:
```c
void scheduler_loop(Scheduler* sched) {
    while (runtime_active) {
        Task* task = NULL;

        // 1. Check local queue (LIFO - hot cache)
        task = pop_local(sched);

        // 2. Check global queue
        if (!task) {
            task = pop_global();
        }

        // 3. Steal from other schedulers
        if (!task) {
            task = steal_from_random(sched);
        }

        // 4. Check for I/O readiness (epoll/kqueue/IOCP)
        if (!task) {
            task = poll_io(sched, timeout_ms);
        }

        if (task) {
            execute_task(task);
        } else {
            park_thread();  // No work available
        }
    }
}
```

### 4.3 Work Stealing

```c
// Chase-Lev work-stealing deque
// Owner: push/pop from bottom (no synchronization needed)
// Thieves: steal from top (requires CAS)

typedef struct {
    Task** buffer;                  // Circular buffer
    atomic_size_t top;              // Steal index
    atomic_size_t bottom;           // Owner index
    size_t capacity;
} WorkStealingDeque;

// Owner operations (fast, no atomics for bottom):
void push(WorkStealingDeque* deque, Task* task);
Task* pop(WorkStealingDeque* deque);

// Thief operations (CAS on top):
Task* steal(WorkStealingDeque* deque);  // Returns NULL if empty/contention
```

### 4.4 Task Switching Mechanism

Jet uses stackful coroutines (fibers) for tasks:

```c
typedef struct {
    void* stack_base;               // Bottom of stack
    void* stack_limit;              // Stack overflow check
    size_t stack_size;              // Current stack size
    void* saved_rsp;                // Saved stack pointer
    void* saved_rbp;                // Saved frame pointer
    Context* context;               // Additional saved state
    TaskState state;
    Scheduler* owner;
    // ...
} Task;

// Context switch (assembly):
// 1. Save current registers to current_task
// 2. Load registers from next_task
// 3. Switch stack pointer
// 4. Resume execution

// Stack growth: Dynamic stack allocation
// - Start with small stack (64KB)
// - Grow on overflow (copy to larger stack)
// - Or use segmented stacks (simpler, slight overhead)
```

### 4.5 Stack Management for Tasks

**Segmented Stacks** (chosen for simplicity):
```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│ Stack Seg 0 │────→│ Stack Seg 1 │────→│ Stack Seg 2 │
│   (64KB)    │     │   (128KB)   │     │   (256KB)   │
└─────────────┘     └─────────────┘     └─────────────┘
       │
       ↓
   ┌───────┐
   │ Guard │  ← Protected page for overflow detection
   └───────┘
```

**Stack Overflow Check**:
```c
// Compiler inserts check at function entry:
// if (unlikely(sp - frame_size < task->stack_limit)) {
//     grow_stack_or_overflow();
// }
//
// Cost: 2-3 instructions per function entry
```

## 5. Channel Implementation

### 5.1 Design: Lock-Free for Unbuffered, Mutex for Buffered

**Unbuffered Channel** (synchronous):
- Lock-free matching between sender and receiver
- Direct value transfer, no intermediate buffer

**Buffered Channel** (asynchronous):
- Circular buffer with mutex
- Condition variables for blocking
- Optimized for batch operations

### 5.2 Unbuffered Channel

```c
typedef struct {
    atomic_uintptr_t state;         // Combined sender/receiver waiters
    atomic_uintptr_t send_ptr;      // Waiting sender task
    atomic_uintptr_t recv_ptr;      // Waiting receiver task
} UnbufferedChannel;

// State machine:
// EMPTY: No waiters
// SENDING: Sender waiting for receiver
// RECEIVING: Receiver waiting for sender
// CLOSED: Channel closed

bool channel_send_unbuffered(Channel* ch, void* value) {
    // Fast path: try to match with waiting receiver
    Task* receiver = try_match_receiver(ch);
    if (receiver) {
        direct_transfer(value, receiver);
        resume_task(receiver);
        return true;
    }

    // Slow path: park and wait
    park_current_task(SENDING);
    // Receiver will wake us and transfer value
    return !ch->closed;
}
```

### 5.3 Buffered Channel

```c
typedef struct {
    Mutex mutex;
    CondVar not_full;
    CondVar not_empty;

    uint32_t capacity;
    uint32_t head;                  // Read position
    uint32_t tail;                  // Write position
    uint32_t count;                 // Current items
    bool closed;

    void* buffer[];                 // Circular buffer of pointers
} BufferedChannel;

bool channel_send_buffered(Channel* ch, void* value) {
    lock(&ch->mutex);

    while (ch->count == ch->capacity && !ch->closed) {
        wait(&ch->not_full, &ch->mutex);
    }

    if (ch->closed) {
        unlock(&ch->mutex);
        return false;
    }

    ch->buffer[ch->tail] = value;
    ch->tail = (ch->tail + 1) % ch->capacity;
    ch->count++;

    signal(&ch->not_empty);
    unlock(&ch->mutex);
    return true;
}
```

### 5.4 Select Statement Implementation

```c
// Select uses a "polling" approach with randomized order:

typedef struct {
    Channel* channel;
    SelectOp op;                    // SEND or RECEIVE
    void* value;                    // For send: value to send
                                    // For recv: pointer to store result
} SelectCase;

int select(SelectCase* cases, int num_cases, bool has_default) {
    // 1. Randomize case order (fairness)
    shuffle(cases, num_cases);

    // 2. Try non-blocking operations in order
    for (int i = 0; i < num_cases; i++) {
        if (try_operation(&cases[i])) {
            return i;
        }
    }

    if (has_default) {
        return num_cases;  // Default case
    }

    // 3. Register as waiter on all channels
    SelectWaiter waiter;
    for (int i = 0; i < num_cases; i++) {
        register_waiter(&cases[i], &waiter);
    }

    // 4. Park until one operation succeeds
    park_until_ready(&waiter);

    // 5. Unregister from remaining channels
    int ready_case = waiter.ready_index;
    for (int i = 0; i < num_cases; i++) {
        if (i != ready_case) {
            unregister_waiter(&cases[i], &waiter);
        }
    }

    return ready_case;
}
```

## 6. Error Handling Runtime

### 6.1 Effect Propagation Mechanism

Jet uses a structured effect system. Effects are propagated through a stack of handlers:

```c
typedef struct EffectFrame {
    struct EffectFrame* parent;     // Previous handler
    EffectHandler* handler;         // Current handler
    void* resume_continuation;      // Where to resume after handling
    void* stack_marker;             // For stack unwinding
} EffectFrame;

// Thread-local effect stack
_Thread_local EffectFrame* current_effect_stack = NULL;

// Perform an effect:
void effect_perform(Effect* effect) {
    EffectFrame* frame = current_effect_stack;

    // Search up the stack for a matching handler
    while (frame) {
        if (frame->handler->handles(effect->type)) {
            // Found handler - transfer control
            frame->handler->handle(effect, frame->resume_continuation);
            __builtin_unreachable();  // Handler must not return normally
        }
        frame = frame->parent;
    }

    // No handler found - panic
    panic("Unhandled effect: %s", effect->type->name);
}
```

### 6.2 Stack Unwinding

Two-phase unwinding (destructors then handlers):

```c
typedef struct UnwindFrame {
    struct UnwindFrame* next;
    void* landing_pad;              // Where to resume after cleanup
    void* stack_pointer;
    void (*cleanup_fn)(void*);      // Destructor to run
    void* cleanup_data;
} UnwindFrame;

void unwind_stack(Effect* effect) {
    UnwindFrame* frame = current_unwind_frame;

    while (frame) {
        // Save next frame (cleanup might modify list)
        UnwindFrame* next = frame->next;

        if (frame->cleanup_fn) {
            // Run destructor
            frame->cleanup_fn(frame->cleanup_data);
        }

        // Check if this frame has an effect handler
        if (is_effect_handler_frame(frame)) {
            EffectHandler* handler = get_handler(frame);
            if (handler->handles(effect->type)) {
                // Found handler - resume here
                resume_at_handler(frame, effect);
                return;
            }
        }

        frame = next;
    }

    // No handler found - continue unwinding to panic
    panic_unhandled_effect(effect);
}
```

### 6.3 Cleanup During Unwinding

**RAII Integration**:
```c
// Every scope with destructors pushes an UnwindFrame
// Compiler-generated code:
//
// void foo() {
//     Resource r = acquire();
//     PUSH_UNWIND_FRAME(r.destructor, &r);
//     // ...
//     POP_UNWIND_FRAME();  // Normal exit
// }

// If unwinding occurs, destructor is called automatically
```

**Destruction Order**:
1. Local variables in reverse declaration order
2. Function parameters
3. Temporaries from innermost expression

## 7. FFI and C Interop

### 7.1 Calling C Functions

```c
// FFI call wrapper
typedef struct {
    void* c_function;
    ffi_cif* cif;                   // libffi call interface
    void* arg_types[];
} FFICallInfo;

// Marshaling rules:
// Jet Type      → C Type
// int           → int64_t
// float32       → float
// float64       → double
// bool          → uint8_t (0/1)
// String        → const char* (null-terminated, copied)
// Array<T>      → T* + length parameter
// Struct        → C-compatible struct (#[repr(C)])
// Function      → Function pointer (trampoline)

void* marshal_to_c(void* jet_value, TypeDescriptor* jet_type, ffi_type* c_type);
void* marshal_from_c(void* c_value, ffi_type* c_type, TypeDescriptor* jet_type);
```

### 7.2 Exposing Jet Functions to C

```c
// Trampoline for C-to-Jet calls
typedef struct {
    void* jet_function;
    TypeDescriptor* ret_type;
    TypeDescriptor** arg_types;
    int num_args;
} JetTrampoline;

// Generated trampoline (per function signature):
void* jet_trampoline(void* ret, void** args, JetTrampoline* info) {
    // 1. Marshal C args to Jet values
    void* jet_args[MAX_ARGS];
    for (int i = 0; i < info->num_args; i++) {
        jet_args[i] = marshal_from_c(args[i], c_arg_types[i], info->arg_types[i]);
    }

    // 2. Call Jet function (may suspend, allocate, etc.)
    void* result = call_jet_function(info->jet_function, jet_args);

    // 3. Marshal result to C
    return marshal_to_c(result, info->ret_type, c_ret_type);
}
```

### 7.3 Memory Ownership Across Boundary

**Ownership Rules**:
```
Jet → C:
  - Primitive values: copied
  - Strings: copied to C buffer (caller must free)
  - Arrays: copied or borrowed (explicit annotation)
  - Structs: copied (if #[repr(C)])

C → Jet:
  - Primitive values: copied
  - Pointers: wrapped in opaque handle (no dereference)
  - Strings: copied to Jet String
  - Buffers: copied or wrapped in custom type
```

**Borrow Checker Integration**:
```c
// #[borrow] annotation for zero-copy passing:
// fn process(data: #[borrow] Array<u8>)  // No copy, lifetime checked

// Runtime borrow tracking for FFI:
typedef struct {
    void* data;
    TypeDescriptor* type;
    BorrowId id;                    // For dynamic borrow checking
    bool mutable;
} BorrowedRef;
```

### 7.4 Marshaling Rules

| Jet Type | C Representation | Notes |
|----------|-----------------|-------|
| `int` | `int64_t` | Sign-extended |
| `float32` | `float` | IEEE 754 |
| `float64` | `double` | IEEE 754 |
| `bool` | `uint8_t` | 0 or 1 |
| `String` | `char*` | Null-terminated, UTF-8 |
| `Array<T>` | `struct { T* data; size_t len; }` | By value or pointer |
| `Option<T>` | `T*` | NULL for None |
| `Result<T,E>` | `struct { bool ok; union { T ok; E err; }; }` | Discriminated union |
| `fn` | Function pointer | With context pointer for closures |

## 8. Panic/Recovery

### 8.1 Panic Mechanism

```c
typedef struct {
    JetString* message;
    StackTrace* stack_trace;
    void* payload;                  // Optional typed payload
    TypeDescriptor* payload_type;
} PanicInfo;

// Panic is implemented as an unhandled effect
// Can be caught with `recover` blocks

void jet_panic(JetString* message) {
    PanicInfo info = {
        .message = message,
        .stack_trace = capture_stack_trace(),
        .payload = NULL,
        .payload_type = NULL
    };

    // Perform panic effect
    effect_perform(&panic_effect, &info);
    __builtin_unreachable();
}
```

### 8.2 Stack Unwinding vs Abort

**Unwinding** (default for recoverable panics):
- Runs destructors (RAII cleanup)
- Searches for `recover` handler
- Slower but safe

**Abort** (for unrecoverable errors):
- Immediate process termination
- No cleanup
- Used for: double panic, stack overflow, memory corruption

```c
typedef enum {
    UNWIND_MODE_UNWIND,     // Default: run destructors, find handler
    UNWIND_MODE_ABORT,      // Immediate abort
} UnwindMode;

void panic_with_mode(PanicInfo* info, UnwindMode mode) {
    if (mode == UNWIND_MODE_ABORT || is_panicking()) {
        // Double panic - abort immediately
        abort();
    }

    set_panicking(true);

    if (mode == UNWIND_MODE_UNWIND) {
        unwind_and_find_handler(info);
    }
}
```

### 8.3 Recovery Boundaries

```c
// recover block runtime support:
typedef struct {
    jmp_buf jump_buffer;
    PanicInfo* caught_panic;
    bool caught;
} RecoveryContext;

bool enter_recover_block(RecoveryContext* ctx) {
    if (setjmp(ctx->jump_buffer) == 0) {
        // Normal entry
        push_recovery_handler(ctx);
        return true;  // Execute protected block
    } else {
        // Recovered from panic
        pop_recovery_handler();
        return false;  // Execute recovery block
    }
}

void recover_from_panic(PanicInfo* info) {
    RecoveryContext* ctx = current_recovery_handler();
    if (ctx) {
        ctx->caught_panic = info;
        ctx->caught = true;
        longjmp(ctx->jump_buffer, 1);
    }
    // No recovery handler - continue unwinding
}
```

## 9. Startup and Shutdown

### 9.1 Runtime Initialization

```c
typedef struct {
    int argc;
    char** argv;
    char** envp;

    // Configuration
    size_t heap_size;
    size_t stack_size;
    int num_worker_threads;
    GCConfig gc_config;
} RuntimeConfig;

int jet_main(int argc, char** argv, char** envp) {
    // 1. Early init (no allocation)
    parse_early_args(argc, argv);

    // 2. Initialize memory subsystem
    init_heap(config.heap_size);
    init_gc(&config.gc_config);

    // 3. Initialize type system
    init_type_registry();
    register_builtin_types();

    // 4. Initialize threading
    init_scheduler(config.num_worker_threads);

    // 5. Initialize I/O subsystem
    init_io();

    // 6. Initialize effect system
    init_effect_handlers();

    // 7. Run static constructors
    run_static_constructors();

    // 8. Call user main
    int result = jet_user_main(argc, argv);

    // 9. Shutdown
    jet_shutdown();

    return result;
}
```

### 9.2 Command Line Argument Handling

```c
// Jet runtime arguments (consumed, not passed to program):
// --jet-heap-size=SIZE       Initial heap size
// --jet-stack-size=SIZE      Default task stack size
// --jet-threads=N            Number of worker threads
// --jet-gc-log               Enable GC logging
// --jet-gc-parallel          Parallel GC marking
// --jit-profile=FILE         JIT profiling output

// Parsed before user main() sees arguments
```

### 9.3 Atexit Handlers

```c
typedef struct AtExitHandler {
    struct AtExitHandler* next;
    void (*fn)(void);
    int priority;                   // Lower = earlier
} AtExitHandler;

static AtExitHandler* atexit_handlers = NULL;

void jet_atexit(void (*fn)(void), int priority) {
    AtExitHandler* handler = malloc(sizeof(AtExitHandler));
    handler->fn = fn;
    handler->priority = priority;
    // Insert in priority order
    // ...
}

void run_atexit_handlers() {
    // Run in reverse priority order
    for (AtExitHandler* h = atexit_handlers; h; h = h->next) {
        h->fn();
    }
}
```

### 9.4 Graceful Shutdown

```c
void jet_shutdown() {
    // 1. Stop accepting new tasks
    disable_scheduler();

    // 2. Wait for all tasks to complete (with timeout)
    wait_for_tasks(SHUTDOWN_TIMEOUT_MS);

    // 3. Cancel remaining tasks (if any)
    cancel_all_tasks();

    // 4. Run atexit handlers
    run_atexit_handlers();

    // 5. Flush I/O buffers
    flush_all_io();

    // 6. Final GC (collect everything)
    gc_collect_all();

    // 7. Verify no leaks (debug builds)
    #ifdef DEBUG
    verify_no_leaks();
    #endif

    // 8. Release resources
    destroy_scheduler();
    destroy_heap();
}
```

## 10. Performance Considerations

### 10.1 Cache-Friendly Layouts

**Struct Layout Optimization**:
```c
// Hot/cold splitting:
// Frequently accessed fields first (cache line 0)
// Rarely accessed fields later

typedef struct {
    // Hot fields (cache line 0)
    uint32_t refcount;
    uint32_t state;
    void* data;

    // Cold fields (cache line 1+)
    JetString* debug_name;
    void* user_data;
    Statistics* stats;
} OptimizedStruct;
```

**Array of Structs vs Struct of Arrays**:
```c
// AoS (default for objects):
// [obj0: {a,b,c}][obj1: {a,b,c}][obj2: {a,b,c}]

// SoA (for numeric arrays, SIMD):
// [a0,a1,a2,...][b0,b1,b2,...][c0,c1,c2,...]
// Use #[repr(soa)] for automatic transformation
```

### 10.2 Prefetch Hints

```c
// GC marking with prefetching:
void mark_with_prefetch(void* obj) {
    mark_object(obj);

    // Prefetch children
    TypeDescriptor* type = get_type(obj);
    for (int i = 0; i < type->num_fields; i++) {
        void** field = get_field_ptr(obj, type->fields[i].offset);
        if (*field) {
            __builtin_prefetch(*field, 0, 3);  // Read, high locality
        }
    }
}
```

### 10.3 SIMD Support

```c
// Vector types:
// Vec4f, Vec8f, Vec4i, Vec8i, etc.

// Automatic vectorization hints:
// #[vectorize] annotation for loops
// Compiler generates SIMD versions with runtime dispatch

// Explicit SIMD:
Vec4f add_vectors(Vec4f a, Vec4f b) {
    return _mm_add_ps(a, b);  // SSE
}
```

### 10.4 Vectorization Opportunities

**Loop Vectorization**:
```c
// Recognized patterns:
// - Map: arr.map(|x| x * 2)
// - Reduce: arr.reduce(0, |a,b| a + b)
// - Filter: arr.filter(|x| x > 0)
// - Zip: zip(a, b).map(|(x,y)| x + y)

// Runtime dispatch based on CPU features:
// - AVX-512 (512-bit)
// - AVX2 (256-bit)
// - SSE4.2 (128-bit)
// - Scalar fallback
```

## 11. Platform Abstraction

### 11.1 OS-Specific Code Isolation

```
runtime/
├── platform/
│   ├── unix/
│   │   ├── thread.c          pthreads
││   ├── mmap.c               memory mapping
│   │   ├── signal.c          signal handling
│   │   └── io.c              epoll/kqueue
│   ├── windows/
│   │   ├── thread.c          Windows threads
│   │   ├── virtual_memory.c  VirtualAlloc
│   │   ├── exception.c       SEH
│   │   └── io.c              IOCP
│   └── common/
│       └── ...               Platform-agnostic code
```

### 11.2 Thread-Local Storage

```c
// Fast TLS using __thread (compiler-supported)
// Or platform-specific for maximum performance

_Thread_local Scheduler* tls_current_scheduler;
_Thread_local Task* tls_current_task;
_Thread_local EffectFrame* tls_effect_stack;
_Thread_local UnwindFrame* tls_unwind_frame;
_Thread_local GCThreadState* tls_gc_state;

// Access macros for potential optimization:
#define CURRENT_SCHEDULER (__builtin_thread_pointer()->scheduler)
```

### 11.3 Signal Handling

```c
// Unix signal handling:
void init_signal_handlers() {
    struct sigaction sa;

    // Segfault handler (for stack overflow detection)
    sa.sa_handler = segfault_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_SIGINFO;
    sigaction(SIGSEGV, &sa, NULL);

    // User interrupt
    sa.sa_handler = interrupt_handler;
    sigaction(SIGINT, &sa, NULL);

    // GC-friendly signals
    sa.sa_handler = gc_signal_handler;
    sigaction(SIGUSR1, &sa, NULL);  // Request GC
    sigaction(SIGUSR2, &sa, NULL);  // Stop for GC
}

// Stack overflow detection:
void segfault_handler(int sig, siginfo_t* info, void* context) {
    if (is_stack_overflow(info->si_addr)) {
        // Convert to panic with stack trace
        panic("Stack overflow");
    }
    // Otherwise, real segfault - abort
    abort();
}
```

### 11.4 System Call Interface

```c
// Direct syscalls where beneficial (Linux):
// - read/write (buffered I/O)
// - mmap/munmap (memory management)
// - futex (synchronization primitives)
// - epoll (async I/O)

// Wrapped for portability:
static inline long jet_syscall3(long n, long a1, long a2, long a3) {
    unsigned long ret;
    __asm__ volatile ("syscall"
        : "=a"(ret)
        : "a"(n), "D"(a1), "S"(a2), "d"(a3)
        : "rcx", "r11", "memory");
    return ret;
}

// Or use libc wrappers for compatibility
```

## Appendix A: Data Structure Summary

| Component | Primary Data Structure | Key Algorithm |
|-----------|----------------------|---------------|
| GC Heap | Immix blocks | Mark-region with opportunistic evacuation |
| Object Headers | 16-byte struct | Tagged pointers for small values |
| Scheduler | Work-stealing deques | Chase-Lev algorithm |
| Channels | Lock-free matching (unbuffered), mutex + condvar (buffered) | Randomized select |
| Type System | Type descriptor table | Binary search for interface dispatch |
| Stack Maps | Sorted array | Binary search by return address |
| Effects | Linked list of handlers | Linear search up stack |

## Appendix B: Configuration Options

| Option | Default | Description |
|--------|---------|-------------|
| `heap.initial_size` | 256MB | Initial heap reservation |
| `heap.max_size` | 0 (unlimited) | Maximum heap size |
| `gc.nursery_size` | 32MB | Nursery space per thread |
| `gc.collection_threshold` | 70% | Start GC at this heap usage |
| `scheduler.threads` | num_cpus | Worker thread count |
| `scheduler.stack_size` | 64KB | Initial task stack size |
| `scheduler.max_stack_size` | 8MB | Maximum task stack size |

## Appendix C: References

1. **Immix GC**: Blackburn, S. M., & McKinley, K. S. (2008). Immix: A mark-region garbage collector with space efficiency, fast collection, and mutator performance.
2. **Work-Stealing**: Blumofe, R. D., & Leiserson, C. E. (1999). Scheduling multithreaded computations by work stealing.
3. **Go Runtime**: https://github.com/golang/go/tree/master/src/runtime
4. **Rust Runtime**: https://github.com/rust-lang/rust/tree/master/library/std/src
5. **Effect Handlers**: Plotkin, G., & Pretnar, M. (2009). Handlers of algebraic effects.
