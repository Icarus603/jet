// AArch64 (ARM64) context switching for Jet scheduler
// Arguments:
//   x0 - pointer to current Context (to save)
//   x1 - pointer to next Context (to restore)

.text
.globl _jet_context_switch
.align 4
_jet_context_switch:
    // Save current context (first argument - x0)
    str x19, [x0, #0]       // Save x19 (callee-saved)
    str x20, [x0, #8]       // Save x20
    str x21, [x0, #16]      // Save x21
    str x22, [x0, #24]      // Save x22
    str x23, [x0, #32]      // Save x23
    str x24, [x0, #40]      // Save x24
    str x25, [x0, #48]      // Save x25
    str x26, [x0, #56]      // Save x26
    str x27, [x0, #64]      // Save x27
    str x28, [x0, #72]      // Save x28
    str x29, [x0, #80]      // Save x29 (frame pointer)
    mov x2, sp
    str x2, [x0, #88]       // Save stack pointer
    str x30, [x0, #96]      // Save x30 (link register / return address)

    // Restore next context (second argument - x1)
    ldr x19, [x1, #0]       // Restore x19
    ldr x20, [x1, #8]       // Restore x20
    ldr x21, [x1, #16]      // Restore x21
    ldr x22, [x1, #24]      // Restore x22
    ldr x23, [x1, #32]      // Restore x23
    ldr x24, [x1, #40]      // Restore x24
    ldr x25, [x1, #48]      // Restore x25
    ldr x26, [x1, #56]      // Restore x26
    ldr x27, [x1, #64]      // Restore x27
    ldr x28, [x1, #72]      // Restore x28
    ldr x29, [x1, #80]      // Restore x29
    ldr x2, [x1, #88]       // Load stack pointer
    mov sp, x2
    ldr x30, [x1, #96]      // Restore x30 (link register)

    ret

// Initialize a new context for a task
// Arguments:
//   x0 - pointer to Context to initialize
//   x1 - stack pointer (top of stack)
//   x2 - entry point function
.globl _jet_context_init
.align 4
_jet_context_init:
    // Clear all callee-saved registers
    str xzr, [x0, #0]       // Clear x19
    str xzr, [x0, #8]       // Clear x20
    str xzr, [x0, #16]      // Clear x21
    str xzr, [x0, #24]      // Clear x22
    str xzr, [x0, #32]      // Clear x23
    str xzr, [x0, #40]      // Clear x24
    str xzr, [x0, #48]      // Clear x25
    str xzr, [x0, #56]      // Clear x26
    str xzr, [x0, #64]      // Clear x27
    str xzr, [x0, #72]      // Clear x28
    str xzr, [x0, #80]      // Clear x29
    str x1, [x0, #88]       // Set stack pointer
    str x2, [x0, #96]       // Set link register to entry point
    ret

// Get the current stack pointer
.globl _jet_get_stack_pointer
.align 4
_jet_get_stack_pointer:
    mov x0, sp
    ret
