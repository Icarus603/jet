//! Comprehensive garbage collector tests for Jet runtime
//!
//! Tests allocation, collection, stress scenarios, and edge cases.

use jet_rt_gc::{
    align_ptr_up, align_up, Block, BlockList, BlockState, CollectionStats, Collector, ImmixConfig,
    ImmixHeap, LineState, ObjectHeader, TypeId, BLOCK_SIZE, LARGE_OBJECT_THRESHOLD,
    LINES_PER_BLOCK,
};

// ============================================================================
// Block Tests
// ============================================================================

#[test]
fn test_block_creation() {
    let block = Block::new().expect("Failed to create block");
    let block_ref = unsafe { block.as_ptr().as_ref().unwrap() };
    assert_eq!(block_ref.state, BlockState::Empty);
    assert_eq!(block_ref.free_lines as usize, LINES_PER_BLOCK);
}

#[test]
fn test_block_line_metadata() {
    let block = Block::new().expect("Failed to create block");
    let block_ref = unsafe { block.as_ptr().as_ref().unwrap() };

    // Initially all lines should be free
    for i in 0..LINES_PER_BLOCK {
        assert_eq!(block_ref.line_metadata[i].state, LineState::Free);
    }

    // Mark some lines as allocated via object allocation
    let block_mut = unsafe { block.as_ptr().as_mut().unwrap() };
    let ptr1 = block_mut.allocate(100, 8).expect("Allocation failed");
    assert!(!ptr1.is_null());

    // After allocation, lines should be marked as Live
    assert_eq!(block_mut.line_metadata[0].state, LineState::Live);
}

#[test]
fn test_block_allocation() {
    let block = Block::new().expect("Failed to create block");
    let block_mut = unsafe { block.as_ptr().as_mut().unwrap() };
    block_mut.is_nursery = true;

    // Allocate some memory
    let ptr1 = block_mut.allocate(100, 8).expect("Allocation failed");
    assert!(!ptr1.is_null());

    // Allocate more
    let ptr2 = block_mut
        .allocate(200, 8)
        .expect("Second allocation failed");
    assert!(!ptr2.is_null());
    assert_ne!(ptr1, ptr2);
}

#[test]
fn test_block_full() {
    let block = Block::new().expect("Failed to create block");
    let block_mut = unsafe { block.as_ptr().as_mut().unwrap() };

    // Try to allocate more than block size
    let result = block_mut.allocate(BLOCK_SIZE + 1, 8);
    assert!(
        result.is_none(),
        "Should fail to allocate more than block size"
    );
}

// ============================================================================
// Object Header Tests
// ============================================================================

#[test]
fn test_object_header_creation() {
    let header = ObjectHeader::new(TypeId::new(1), 100);

    assert_eq!(header.object_size(), 100);
    assert_eq!(header.type_id().raw(), 1);
    assert!(!header.is_marked());
}

#[test]
fn test_object_header_marking() {
    let header = ObjectHeader::new(TypeId::new(1), 64);

    assert!(!header.is_marked());
    header.set_marked(true);
    assert!(header.is_marked());
    header.set_marked(false);
    assert!(!header.is_marked());
}

#[test]
fn test_object_header_size_rounding() {
    // ObjectHeader stores size as provided, alignment is handled separately
    let header1 = ObjectHeader::new(TypeId::new(1), 8);
    assert_eq!(header1.object_size(), 8);

    let header2 = ObjectHeader::new(TypeId::new(1), 16);
    assert_eq!(header2.object_size(), 16);
}

// ============================================================================
// Alignment Tests
// ============================================================================

#[test]
fn test_align_up_function() {
    assert_eq!(align_up(0, 8), 0);
    assert_eq!(align_up(1, 8), 8);
    assert_eq!(align_up(7, 8), 8);
    assert_eq!(align_up(8, 8), 8);
    assert_eq!(align_up(9, 8), 16);
    assert_eq!(align_up(100, 16), 112);
}

#[test]
fn test_align_ptr_up_function() {
    let ptr = 0x1001 as *mut u8;
    let aligned = align_ptr_up(ptr, 8);
    assert_eq!(aligned as usize % 8, 0);
    assert!(aligned as usize >= ptr as usize);
}

// ============================================================================
// Heap Allocation Tests
// ============================================================================

#[test]
fn test_heap_creation() {
    let heap = ImmixHeap::new();
    assert_eq!(heap.total_allocated(), 0);
    assert_eq!(heap.collection_count(), 0);
}

#[test]
fn test_heap_basic_allocation() {
    let mut heap = ImmixHeap::new();

    let ptr = heap.allocate(64, 8);
    assert!(!ptr.is_null());
    assert!(heap.total_allocated() >= 64);
}

#[test]
fn test_heap_object_allocation() {
    let mut heap = ImmixHeap::new();

    let ptr = heap.allocate_object(100, 8, TypeId::new(42));
    assert!(!ptr.is_null());

    // Verify header
    unsafe {
        let header = heap.header_from_payload(ptr);
        assert_eq!((*header).type_id().raw(), 42);
        assert_eq!((*header).object_size(), 100);
    }
}

#[test]
fn test_heap_multiple_allocations() {
    let mut heap = ImmixHeap::new();
    let mut ptrs = Vec::new();

    for i in 0..100 {
        let ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
        assert!(!ptr.is_null());
        ptrs.push(ptr);
    }

    // Verify all pointers are unique and valid
    let unique_count = ptrs.iter().collect::<std::collections::HashSet<_>>().len();
    assert_eq!(unique_count, 100);
}

#[test]
fn test_heap_contains() {
    let mut heap = ImmixHeap::new();
    let ptr = heap.allocate_object(64, 8, TypeId::new(1));

    assert!(heap.contains(ptr));
    assert!(!heap.contains(std::ptr::null_mut()));
    assert!(!heap.contains(std::ptr::dangling_mut::<u8>()));
}

// ============================================================================
// Large Object Space Tests
// ============================================================================

#[test]
fn test_large_object_allocation() {
    let mut heap = ImmixHeap::new();

    let large_size = LARGE_OBJECT_THRESHOLD + 1024;
    let ptr = heap.allocate_object(large_size, 8, TypeId::new(1));

    assert!(!ptr.is_null());
    assert!(heap.los_bytes() > 0);
}

#[test]
fn test_large_object_collection() {
    let mut heap = ImmixHeap::new();

    // Allocate some large objects
    let ptrs: Vec<_> = (0..10)
        .map(|i| heap.allocate_object(LARGE_OBJECT_THRESHOLD + 100, 8, TypeId::new(i as u32)))
        .collect();

    // All should be allocated
    assert!(ptrs.iter().all(|p| !p.is_null()));

    // Collect with no roots
    let stats = heap.collect(&[]);

    // Large objects should be collected
    assert_eq!(stats.objects_marked, 0);
}

// ============================================================================
// Collection Tests
// ============================================================================

#[test]
fn test_nursery_collection() {
    let mut heap = ImmixHeap::new();

    // Allocate objects in nursery
    let ptrs: Vec<_> = (0..50)
        .map(|i| heap.allocate_object(64, 8, TypeId::new(i as u32)))
        .collect();

    // Create roots from first 10
    let roots: Vec<*mut ObjectHeader> = ptrs
        .iter()
        .take(10)
        .map(|p| unsafe { heap.header_from_payload(*p) })
        .collect();

    // Collect nursery
    let stats = heap.collect_nursery(&roots);

    // Objects reachable from roots should survive (get promoted)
    assert!(stats.objects_marked >= 10);
}

#[test]
fn test_full_collection() {
    let mut heap = ImmixHeap::new();

    // Allocate many objects
    let mut ptrs = Vec::new();
    for i in 0..100 {
        let ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
        ptrs.push(ptr);
    }

    let before = heap.total_allocated();

    // Collect with no roots - everything should be reclaimed
    let stats = heap.collect(&[]);

    assert_eq!(stats.objects_marked, 0);
    assert!(heap.total_allocated() < before || heap.total_allocated() == 0);
}

#[test]
fn test_collection_with_roots() {
    let mut heap = ImmixHeap::new();

    // Allocate objects
    let ptr1 = heap.allocate_object(64, 8, TypeId::new(1));
    let ptr2 = heap.allocate_object(64, 8, TypeId::new(2));
    let _ptr3 = heap.allocate_object(64, 8, TypeId::new(3));

    // Keep ptr1 and ptr2 as roots
    let roots = vec![unsafe { heap.header_from_payload(ptr1) }, unsafe {
        heap.header_from_payload(ptr2)
    }];

    let stats = heap.collect(&roots);

    assert_eq!(stats.objects_marked, 2);
}

#[test]
fn test_multiple_collections() {
    let mut heap = ImmixHeap::new();

    for cycle in 0..10 {
        // Allocate some objects
        for i in 0..50 {
            let _ = heap.allocate_object(64, 8, TypeId::new(i as u32));
        }

        // Collect
        let _ = heap.collect(&[]);

        // Verify heap is still functional
        let ptr = heap.allocate_object(64, 8, TypeId::new(cycle as u32));
        assert!(!ptr.is_null());
    }

    assert!(heap.collection_count() >= 10);
}

// ============================================================================
// Stress Tests
// ============================================================================

#[test]
fn test_stress_allocation() {
    let mut heap = ImmixHeap::new();
    let mut ptrs = Vec::new();

    // Allocate many small objects
    for i in 0..10000 {
        let ptr = heap.allocate_object(32, 8, TypeId::new((i % 100) as u32));
        if !ptr.is_null() {
            ptrs.push(ptr);
        }
    }

    // Should have allocated most objects
    assert!(ptrs.len() > 9000);

    // Collect with some roots
    let roots: Vec<_> = ptrs
        .iter()
        .step_by(10)
        .map(|p| unsafe { heap.header_from_payload(*p) })
        .collect();

    let stats = heap.collect(&roots);
    assert!(stats.objects_marked >= roots.len());
}

#[test]
fn test_fragmentation() {
    let mut heap = ImmixHeap::new();
    let mut ptrs = Vec::new();

    // Allocate objects of varying sizes
    for i in 0..1000 {
        let size = 32 + (i % 10) * 32; // 32 to 320 bytes
        let ptr = heap.allocate_object(size, 8, TypeId::new((i % 50) as u32));
        if !ptr.is_null() {
            ptrs.push(ptr);
        }
    }

    // Free every other object (conceptually - in reality we just don't root them)
    let _to_free: Vec<_> = ptrs.iter().step_by(2).copied().collect();
    // In a real GC, we'd drop references here

    // Collect
    let roots: Vec<_> = ptrs
        .iter()
        .skip(1)
        .step_by(2)
        .map(|p| unsafe { heap.header_from_payload(*p) })
        .collect();

    let _stats = heap.collect(&roots);

    // Should still be able to allocate
    let new_ptr = heap.allocate_object(100, 8, TypeId::new(999));
    assert!(!new_ptr.is_null());
}

#[test]
fn test_allocation_patterns() {
    let mut heap = ImmixHeap::new();

    // Pattern 1: Allocate and immediately drop
    for _ in 0..100 {
        let _ = heap.allocate_object(64, 8, TypeId::new(1));
    }

    // Pattern 2: Allocate and keep
    let mut keepers = Vec::new();
    for i in 0..100 {
        let ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
        keepers.push(ptr);
    }

    // Pattern 3: Allocate varying sizes
    for i in 0..100 {
        let size = 1 << (i % 10 + 5); // Powers of 2 from 32 to 16384
        let _ = heap.allocate_object(size, 8, TypeId::new(i as u32));
    }

    // Collect and verify keepers are still valid (as roots)
    let roots: Vec<_> = keepers
        .iter()
        .map(|p| unsafe { heap.header_from_payload(*p) })
        .collect();

    let stats = heap.collect(&roots);
    assert!(stats.objects_marked >= 100);
}

// ============================================================================
// Configuration Tests
// ============================================================================

#[test]
fn test_custom_config() {
    let config = ImmixConfig {
        nursery_threshold: 64 * 1024,     // 64 KB
        mature_threshold: 512 * 1024,     // 512 KB
        large_object_threshold: 8 * 1024, // 8 KB
        enable_evacuation: true,
        evacuation_threshold: 0.7,
    };

    let mut heap = ImmixHeap::with_config(config);
    let ptr = heap.allocate_object(100, 8, TypeId::new(1));
    assert!(!ptr.is_null());
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_zero_size_allocation() {
    let mut heap = ImmixHeap::new();

    // Zero-size allocation should still return a valid pointer
    let ptr = heap.allocate(0, 8);
    // Behavior may vary, but shouldn't crash
    let _ = ptr;
}

#[test]
fn test_very_small_allocation() {
    let mut heap = ImmixHeap::new();

    let ptr = heap.allocate(1, 8);
    assert!(!ptr.is_null());
}

#[test]
fn test_large_alignment() {
    let mut heap = ImmixHeap::new();

    // Request large alignment
    let ptr = heap.allocate(64, 64);
    assert!(!ptr.is_null());
    assert_eq!(ptr as usize % 64, 0);
}

#[test]
fn test_allocation_after_collection() {
    let mut heap = ImmixHeap::new();

    // Allocate and collect multiple times
    for _ in 0..5 {
        for i in 0..100 {
            let _ = heap.allocate_object(64, 8, TypeId::new(i as u32));
        }
        let _ = heap.collect(&[]);
    }

    // Should still be able to allocate
    let ptr = heap.allocate_object(1000, 8, TypeId::new(999));
    assert!(!ptr.is_null());
}

#[test]
fn test_empty_collection() {
    let mut heap = ImmixHeap::new();

    // Collect on empty heap
    let stats = heap.collect(&[]);
    assert_eq!(stats.objects_marked, 0);

    // Collect again
    let stats2 = heap.collect(&[]);
    assert_eq!(stats2.objects_marked, 0);
}

// ============================================================================
// Statistics Tests
// ============================================================================

#[test]
fn test_collection_stats() {
    let mut heap = ImmixHeap::new();

    // Allocate some objects
    for i in 0..100 {
        let _ = heap.allocate_object(64, 8, TypeId::new(i as u32));
    }

    let stats = heap.collect(&[]);

    assert_eq!(stats.objects_marked, 0);
    // bytes_reclaimed may vary based on implementation
}

#[test]
fn test_heap_stats() {
    let mut heap = ImmixHeap::new();

    let stats_before = heap.stats();
    assert_eq!(stats_before.total_bytes, 0);

    // Allocate objects
    for i in 0..10 {
        let _ = heap.allocate_object(100, 8, TypeId::new(i as u32));
    }

    let stats_after = heap.stats();
    assert!(stats_after.total_bytes >= 1000);
}

// ============================================================================
// Block List Tests
// ============================================================================

#[test]
fn test_block_list_operations() {
    let mut list = BlockList::new();
    assert!(list.is_empty());

    let block = Block::new().expect("Failed to allocate block");
    list.push(block);
    assert_eq!(list.len(), 1);

    let popped = list.pop();
    assert!(popped.is_some());
    assert!(list.is_empty());
}

#[test]
fn test_block_list_iteration() {
    let mut list = BlockList::new();

    // Add several blocks
    for _ in 0..5 {
        let block = Block::new().expect("Failed to allocate block");
        list.push(block);
    }

    // Count via iteration
    let count = list.iter().count();
    assert_eq!(count, 5);
}

// ============================================================================
// Collector Tests
// ============================================================================

#[test]
fn test_collector_creation() {
    let collector = Collector::new();
    // Collector starts empty
    let stats = collector.stats();
    assert_eq!(stats.objects_marked, 0);
}

#[test]
fn test_collection_stats_structure() {
    let stats = CollectionStats {
        objects_marked: 10,
        objects_evacuated: 5,
        bytes_reclaimed: 1024,
        mark_time_us: 100,
        sweep_time_us: 50,
    };

    assert_eq!(stats.objects_marked, 10);
    assert_eq!(stats.objects_evacuated, 5);
    assert_eq!(stats.bytes_reclaimed, 1024);
}
