//! Jet Runtime Garbage Collector
//!
//! This crate provides the Immix garbage collector for the Jet programming language.
//!
//! # Immix GC
//!
//! Immix is a mark-region garbage collector with the following characteristics:
//!
//! - **Block-based**: Memory is organized into 32KB blocks
//! - **Line-granular**: Each block has 256 lines of 128 bytes each
//! - **Bump allocation**: Fast allocation within contiguous free lines
//! - **Hole punching**: Can allocate into fragmented blocks
//! - **Generational**: Nursery for new objects, mature space for survivors
//! - **Evacuation**: Fragmented blocks can be evacuated during full collections
//!
//! # Usage
//!
//! ```rust
//! use jet_rt_gc::{ImmixHeap, TypeId};
//!
//! let mut heap = ImmixHeap::new();
//!
//! // Allocate an object
//! let ptr = heap.allocate_object(100, 8, TypeId::new(1));
//! assert!(!ptr.is_null());
//!
//! // Collect garbage
//! heap.collect(&[]);
//! ```

// Module declarations
pub mod allocator;
pub mod block;
pub mod collect;
pub mod header;
pub mod immix;

// GC integration modules
pub mod safe_point;
pub mod scheduler_integration;
pub mod stack_map;
pub mod thread_roots;
pub mod write_barrier;

// Re-export main types
pub use block::{
    Block, BlockList, BlockState, LineMetadata, LineState, BLOCK_SIZE, LINES_PER_BLOCK, LINE_SIZE,
};

pub use header::{align_ptr_up, align_up, init_header, GcFlags, ObjectHeader, TypeId};

pub use allocator::{
    Allocator, LargeObjectSpace, DEFAULT_NURSERY_THRESHOLD, LARGE_OBJECT_THRESHOLD,
};

pub use collect::{CollectionStats, Collector, MarkStack};

pub use immix::{HeapStats, ImmixConfig, ImmixHeap, ThreadLocalBuffer};

// Re-export integration types
pub use stack_map::{
    GcRootIterator, StackMapBuilder, StackMapEntry, StackMapHeader, StackMapRegistry,
    StackMapSection, STACK_MAP_MAGIC, STACK_MAP_VERSION,
};

pub use write_barrier::{
    array_write_barrier, bulk_write_barrier, write_barrier, GlobalRememberedSet, RememberedEntry,
    RememberedSetBuffer, ThreadRememberedSet, WriteBarrierState, REMEMBERED_SET_BUFFER_SIZE,
};

pub use safe_point::{
    is_gc_in_progress, is_safe_point_requested, release_safe_point, request_safe_point,
    SafePointCoordinator, SafePointState, ThreadSafePoint, ThreadSafePointState, PARKED_THREADS,
    SAFE_POINT_STATE, TOTAL_THREADS,
};

pub use thread_roots::{
    conservative_scan_stack, GcRoot, GlobalRootScanner, RootSetCollector, SavedRegisters,
    ThreadLocalRoots, ThreadRootScanner,
};

pub use scheduler_integration::{
    CollectionResult, CollectionSummary, GcSchedulerIntegration, GcSchedulerState,
    GcWorkStealingQueue, ThreadLocalAllocBuffer, ThreadLocalGcState,
};

/// Version of the GC crate
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Initialize the garbage collector
///
/// This should be called once at runtime startup
pub fn init() {
    // Initialization logic if needed
}

/// Shutdown the garbage collector
///
/// This should be called at runtime shutdown to ensure proper cleanup
pub fn shutdown() {
    // Cleanup logic if needed
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert!(!VERSION.is_empty());
    }

    #[test]
    fn test_basic_allocation() {
        let mut heap = ImmixHeap::new();
        let ptr = heap.allocate(100, 8);
        assert!(!ptr.is_null());
    }

    #[test]
    fn test_object_lifecycle() {
        let mut heap = ImmixHeap::new();

        // Allocate some objects
        let ptr1 = heap.allocate_object(64, 8, TypeId::new(1));
        let ptr2 = heap.allocate_object(128, 8, TypeId::new(2));

        assert!(!ptr1.is_null());
        assert!(!ptr2.is_null());

        // Verify headers
        unsafe {
            let header1 = heap.header_from_payload(ptr1);
            let header2 = heap.header_from_payload(ptr2);

            assert_eq!((*header1).type_id().raw(), 1);
            assert_eq!((*header2).type_id().raw(), 2);
            assert_eq!((*header1).object_size(), 64);
            assert_eq!((*header2).object_size(), 128);
        }
    }

    #[test]
    fn test_collection() {
        let mut heap = ImmixHeap::new();

        // Allocate objects
        for i in 0..100 {
            let _ = heap.allocate_object(64, 8, TypeId::new(i as u32));
        }

        let _before = heap.total_allocated();

        // Collect
        let stats = heap.collect_nursery(&[]);

        // After collection of unreachable objects, should have reclaimed memory
        // (though with no roots, everything is unreachable)
        assert!(stats.objects_marked == 0); // No roots = nothing marked
    }

    #[test]
    fn test_stress_allocation() {
        let mut heap = ImmixHeap::new();
        let mut ptrs = Vec::new();

        // Allocate many objects
        for i in 0..1000 {
            let ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
            if !ptr.is_null() {
                ptrs.push(ptr);
            }
        }

        // Verify we got some allocations
        assert!(!ptrs.is_empty());

        // Keep some "roots"
        let roots: Vec<*mut ObjectHeader> = ptrs
            .iter()
            .take(10)
            .map(|p| unsafe { heap.header_from_payload(*p) })
            .collect();

        // Collect with roots
        let _stats = heap.collect_nursery(&roots);

        // Objects reachable from roots should survive
        // (in nursery collection, they get promoted to mature)
    }

    #[test]
    fn test_large_object_space() {
        let mut heap = ImmixHeap::new();

        // Allocate a large object
        let large_size = LARGE_OBJECT_THRESHOLD + 1024;
        let ptr = heap.allocate_object(large_size, 8, TypeId::new(1));

        assert!(!ptr.is_null());
        assert!(heap.los_bytes() > 0);
    }

    #[test]
    fn test_heap_contains() {
        let mut heap = ImmixHeap::new();
        let ptr = heap.allocate_object(100, 8, TypeId::new(1));

        assert!(!ptr.is_null());
        assert!(heap.contains(ptr));
        assert!(!heap.contains(std::ptr::null()));
    }

    #[test]
    fn test_multiple_collections() {
        let mut heap = ImmixHeap::new();

        for _ in 0..5 {
            // Allocate some objects
            for i in 0..50 {
                let _ = heap.allocate_object(64, 8, TypeId::new(i as u32));
            }

            // Collect
            let _ = heap.collect(&[]);
        }

        assert!(heap.collection_count() >= 5);
    }
}
