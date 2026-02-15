//! Stress tests for the Immix garbage collector
//!
//! These tests are designed to verify GC stability under extreme conditions:
//! - High allocation rates (millions of objects per second)
//! - Memory pressure (near-OOM scenarios)
//! - Fragmentation stress
//! - Concurrent multi-threaded allocation
//! - Long-running stability simulation

use jet_rt_gc::{
    CollectionStats, ImmixConfig, ImmixHeap, TypeId, BLOCK_SIZE, LARGE_OBJECT_THRESHOLD,
};
use std::collections::HashSet;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Barrier};
use std::thread;
use std::time::{Duration, Instant};

// ============================================================================
// High Allocation Rate Tests
// ============================================================================

#[test]
fn test_million_allocations_stress() {
    let mut heap = ImmixHeap::new();
    let start = Instant::now();
    let num_allocations = 1_000_000;

    for i in 0..num_allocations {
        let ptr = heap.allocate_object(32, 8, TypeId::new((i % 100) as u32));
        assert!(!ptr.is_null(), "Allocation {} failed", i);
    }

    let elapsed = start.elapsed();
    let rate = num_allocations as f64 / elapsed.as_secs_f64();

    println!(
        "Allocated {} objects in {:?} ({:.0} allocs/sec)",
        num_allocations, elapsed, rate
    );

    // Should sustain at least 100K allocations/sec
    assert!(
        rate > 100_000.0,
        "Allocation rate too slow: {:.0} allocs/sec",
        rate
    );
}

#[test]
fn test_sustained_high_throughput() {
    let mut heap = ImmixHeap::new();
    let batch_size = 100_000;
    let num_batches = 10;
    let mut rates = Vec::new();

    for batch in 0..num_batches {
        let start = Instant::now();

        for i in 0..batch_size {
            let _ = heap.allocate_object(64, 8, TypeId::new((i % 50) as u32));
        }

        let elapsed = start.elapsed();
        let rate = batch_size as f64 / elapsed.as_secs_f64();
        rates.push(rate);

        println!("Batch {}: {:.0} allocs/sec", batch, rate);

        // Periodic collection to prevent OOM
        if batch % 3 == 0 {
            heap.collect(&[]);
        }
    }

    // Calculate average and check for performance degradation
    let avg_rate: f64 = rates.iter().sum::<f64>() / rates.len() as f64;
    let last_rate = rates.last().unwrap();

    println!("Average rate: {:.0} allocs/sec", avg_rate);
    println!("Last batch rate: {:.0} allocs/sec", last_rate);

    // Throughput can vary significantly on shared CI hosts. Ensure we keep a
    // healthy absolute throughput floor and avoid only catastrophic collapse.
    assert!(
        *last_rate > avg_rate * 0.25 && *last_rate > 100_000.0,
        "Performance degraded too far: last={:.0}, avg={:.0}",
        last_rate,
        avg_rate
    );
}

// ============================================================================
// Memory Pressure Tests
// ============================================================================

#[test]
fn test_near_oom_recovery() {
    // Configure small heap to trigger frequent collections
    let config = ImmixConfig {
        nursery_threshold: 64 * 1024, // 64 KB nursery
        mature_threshold: 256 * 1024, // 256 KB mature
        large_object_threshold: LARGE_OBJECT_THRESHOLD,
        enable_evacuation: true,
        evacuation_threshold: 0.7,
    };

    let mut heap = ImmixHeap::with_config(config);
    let mut total_allocated = 0;
    let mut successful_collections = 0;

    // Try to allocate more than total heap size multiple times
    for cycle in 0..100 {
        for i in 0..1000 {
            let ptr = heap.allocate_object(512, 8, TypeId::new((i % 10) as u32));
            if ptr.is_null() {
                break;
            }
            total_allocated += 512;
        }

        // Collect and verify we can continue allocating
        let stats = heap.collect(&[]);
        successful_collections += 1;

        if cycle % 20 == 0 {
            println!(
                "Cycle {}: allocated {} bytes, collections: {}, last reclaimed: {} bytes",
                cycle, total_allocated, successful_collections, stats.bytes_reclaimed
            );
        }
    }

    // Should have completed all cycles
    assert_eq!(successful_collections, 100);
}

#[test]
fn test_large_object_pressure() {
    let mut heap = ImmixHeap::new();
    let mut large_objects: Vec<*mut u8> = Vec::new();

    // Allocate many large objects
    for i in 0..100 {
        let size = LARGE_OBJECT_THRESHOLD + (i * 1024);
        let ptr = heap.allocate_object(size, 8, TypeId::new(i as u32));

        if !ptr.is_null() {
            large_objects.push(ptr);
        }

        // Periodically collect and free half
        if i > 0 && i % 10 == 0 {
            // Keep only half as roots
            let roots: Vec<_> = large_objects
                .iter()
                .step_by(2)
                .map(|p| unsafe { heap.header_from_payload(*p) })
                .collect();

            heap.collect(&roots);

            // Update our tracking
            large_objects.retain(|_| rand::random::<bool>());
        }
    }

    // Verify heap is still functional
    let ptr = heap.allocate_object(1000, 8, TypeId::new(999));
    assert!(!ptr.is_null());
}

// ============================================================================
// Fragmentation Stress Tests
// ============================================================================

#[test]
fn test_fragmentation_resistance() {
    let mut heap = ImmixHeap::new();
    let mut ptrs: Vec<Option<*mut u8>> = Vec::new();

    // Phase 1: Allocate many objects
    for i in 0..1000 {
        let size = 32 + (i % 8) * 32; // 32 to 256 bytes
        let ptr = heap.allocate_object(size, 8, TypeId::new((i % 50) as u32));
        ptrs.push(Some(ptr));
    }

    // Phase 2: Free every other object (creates fragmentation)
    for i in (0..ptrs.len()).step_by(2) {
        ptrs[i] = None;
    }

    // Phase 3: Collect with remaining roots
    let roots: Vec<_> = ptrs
        .iter()
        .flatten()
        .map(|p| unsafe { heap.header_from_payload(*p) })
        .collect();

    let stats_before = heap.stats();
    heap.collect(&roots);
    let stats_after = heap.stats();

    println!(
        "Before collection: {} bytes, After: {} bytes",
        stats_before.total_bytes, stats_after.total_bytes
    );

    // Phase 4: Try to allocate larger objects in fragmented space
    let mut new_allocations = 0;
    for i in 0..500 {
        let size = 128 + (i % 4) * 64; // 128 to 320 bytes
        let ptr = heap.allocate_object(size, 8, TypeId::new(100 + (i % 10) as u32));
        if !ptr.is_null() {
            new_allocations += 1;
        }
    }

    // Should be able to allocate most objects despite fragmentation
    assert!(
        new_allocations > 400,
        "Too many allocation failures due to fragmentation: {}/500",
        new_allocations
    );
}

#[test]
fn test_hole_punching() {
    let mut heap = ImmixHeap::new();
    let block_count = 10;

    // Allocate blocks worth of small objects
    let objects_per_block = BLOCK_SIZE / 128;
    let mut ptrs = Vec::new();

    for block in 0..block_count {
        for i in 0..objects_per_block {
            let ptr = heap.allocate_object(64, 8, TypeId::new((block * 100 + i) as u32));
            if !ptr.is_null() {
                ptrs.push((block, i, ptr));
            }
        }
    }

    // Free specific pattern to create holes
    let mut surviving = Vec::new();
    for (block, i, ptr) in ptrs {
        // Keep only objects where (block + i) % 3 == 0
        if (block + i) % 3 == 0 {
            surviving.push(ptr);
        }
    }

    // Collect
    let roots: Vec<_> = surviving
        .iter()
        .map(|p| unsafe { heap.header_from_payload(*p) })
        .collect();
    heap.collect(&roots);

    // Try to fill holes with new allocations
    let mut filled = 0;
    for i in 0..surviving.len() * 2 {
        let ptr = heap.allocate_object(64, 8, TypeId::new((i % 100) as u32));
        if !ptr.is_null() {
            filled += 1;
        }
    }

    println!(
        "Filled {} holes out of {} attempts",
        filled,
        surviving.len() * 2
    );
    assert!(filled > surviving.len(), "Hole punching not effective");
}

// ============================================================================
// Concurrent Allocation Tests
// ============================================================================

#[test]
fn test_concurrent_allocations() {
    let num_threads = 8;
    let allocations_per_thread = 10_000;
    let barrier = Arc::new(Barrier::new(num_threads));
    let mut handles = Vec::new();

    let start = Instant::now();

    for thread_id in 0..num_threads {
        let barrier_clone = Arc::clone(&barrier);

        let handle = thread::spawn(move || {
            barrier_clone.wait();

            let mut heap = ImmixHeap::new();
            let mut local_ptrs = Vec::new();

            for i in 0..allocations_per_thread {
                let ptr = heap.allocate_object(64, 8, TypeId::new((thread_id * 1000 + i) as u32));
                if !ptr.is_null() {
                    local_ptrs.push(ptr);
                }
            }

            // Collect and verify heap still works
            heap.collect(&[]);

            // Try allocating after collection
            let ptr = heap.allocate_object(64, 8, TypeId::new(999));
            assert!(!ptr.is_null(), "Heap should work after collection");

            local_ptrs.len()
        });

        handles.push(handle);
    }

    let mut total_successful = 0;
    for handle in handles {
        total_successful += handle.join().unwrap();
    }

    let elapsed = start.elapsed();
    let rate = (num_threads * allocations_per_thread) as f64 / elapsed.as_secs_f64();

    println!(
        "Concurrent: {} allocations in {:?} ({:.0} allocs/sec)",
        total_successful, elapsed, rate
    );

    // Should have high success rate
    let expected = num_threads * allocations_per_thread;
    assert!(
        total_successful > expected * 95 / 100,
        "Too many allocation failures: {}/{}",
        total_successful,
        expected
    );
}

#[test]
fn test_concurrent_independent_heaps() {
    let num_threads = 4;
    let stop_flag = Arc::new(AtomicUsize::new(0));
    let barrier = Arc::new(Barrier::new(num_threads));

    let mut handles = Vec::new();

    // Each thread gets its own heap - no synchronization needed
    for thread_id in 0..num_threads {
        let stop_clone = Arc::clone(&stop_flag);
        let barrier_clone = Arc::clone(&barrier);

        let handle = thread::spawn(move || {
            barrier_clone.wait();

            let mut heap = ImmixHeap::new();
            let mut allocated = 0;
            let mut collections = 0;

            while stop_clone.load(Ordering::Relaxed) == 0 {
                // Allocate some objects
                for _ in 0..100 {
                    let ptr = heap.allocate_object(128, 8, TypeId::new(thread_id as u32));
                    if !ptr.is_null() {
                        allocated += 1;
                    }
                }

                // Periodic collection
                heap.collect(&[]);
                collections += 1;

                if collections % 10 == 0 {
                    thread::yield_now();
                }
            }

            (allocated, collections)
        });

        handles.push(handle);
    }

    // Let them run for a short time
    thread::sleep(Duration::from_millis(500));
    stop_flag.store(1, Ordering::Relaxed);

    let mut total_allocated = 0;
    let mut total_collections = 0;

    for handle in handles {
        let (alloc, coll) = handle.join().unwrap();
        total_allocated += alloc;
        total_collections += coll;
    }

    println!(
        "Concurrent independent heaps: {} allocations, {} collections",
        total_allocated, total_collections
    );

    // Should have made progress
    assert!(total_allocated > 1000, "Allocation starvation");
    assert!(total_collections > 10, "Collection starvation");
}

// ============================================================================
// Long-Running Stability Simulation
// ============================================================================

#[test]
fn test_long_running_simulation() {
    let mut heap = ImmixHeap::new();
    let mut roots: Vec<*mut u8> = Vec::new();
    let mut stats_history: Vec<CollectionStats> = Vec::new();

    // Simulate various allocation patterns over many cycles
    let cycles = 1000;

    for cycle in 0..cycles {
        // Pattern 1: Allocate short-lived objects
        for _ in 0..100 {
            let _ = heap.allocate_object(64, 8, TypeId::new(1));
        }

        // Pattern 2: Allocate medium-lived objects
        if cycle % 10 == 0 {
            for i in 0..50 {
                let ptr = heap.allocate_object(128, 8, TypeId::new((i + 100) as u32));
                if !ptr.is_null() {
                    roots.push(ptr);
                }
            }
        }

        // Pattern 3: Allocate long-lived objects
        if cycle % 50 == 0 {
            for i in 0..10 {
                let ptr = heap.allocate_object(256, 8, TypeId::new((i + 200) as u32));
                if !ptr.is_null() {
                    roots.push(ptr);
                }
            }
        }

        // Pattern 4: Randomly drop some roots
        if !roots.is_empty() && cycle % 5 == 0 {
            let to_remove = roots.len() / 10;
            for _ in 0..to_remove {
                if let Some(idx) = (0..roots.len()).next() {
                    roots.remove(idx);
                }
            }
        }

        // Periodic collection
        if cycle % 20 == 0 {
            let root_headers: Vec<_> = roots
                .iter()
                .map(|p| unsafe { heap.header_from_payload(*p) })
                .collect();
            let stats = heap.collect(&root_headers);
            stats_history.push(stats);
        }

        // Verify heap health periodically
        if cycle % 100 == 0 {
            let heap_stats = heap.stats();
            println!(
                "Cycle {}: {} bytes, {} collections",
                cycle, heap_stats.total_bytes, heap_stats.collection_count
            );

            // Verify we can still allocate
            let test_ptr = heap.allocate_object(100, 8, TypeId::new(999));
            assert!(!test_ptr.is_null(), "Heap unhealthy at cycle {}", cycle);
        }
    }

    // Final collection
    let root_headers: Vec<_> = roots
        .iter()
        .map(|p| unsafe { heap.header_from_payload(*p) })
        .collect();
    heap.collect(&root_headers);

    let final_stats = heap.stats();
    println!(
        "Final: {} bytes, {} total collections",
        final_stats.total_bytes, final_stats.collection_count
    );

    // Should have reasonable memory usage
    assert!(
        final_stats.total_bytes < 100 * 1024 * 1024, // Less than 100MB
        "Memory usage too high: {} bytes",
        final_stats.total_bytes
    );
}

#[test]
fn test_memory_leak_detection() {
    let mut heap = ImmixHeap::new();
    let mut previous_total: Option<usize> = None;

    for cycle in 0..50 {
        // Allocate and immediately drop (should be collectible)
        for i in 0..1000 {
            let _ = heap.allocate_object(100, 8, TypeId::new((i % 100) as u32));
        }

        // Collect everything
        let stats = heap.collect(&[]);
        let current_total = heap.total_allocated();

        if cycle % 10 == 0 {
            println!(
                "Cycle {}: {} bytes after collection, reclaimed {} bytes",
                cycle, current_total, stats.bytes_reclaimed
            );
        }

        // After first cycle, memory should stabilize
        if let Some(prev) = previous_total {
            // Allow 20% growth between cycles (some legitimate growth possible)
            assert!(
                current_total <= prev * 12 / 10,
                "Memory leak detected: {} -> {} bytes",
                prev,
                current_total
            );
        }

        previous_total = Some(current_total);
    }
}

// ============================================================================
// Edge Cases and Error Recovery
// ============================================================================

#[test]
fn test_rapid_collection_cycles() {
    let mut heap = ImmixHeap::new();

    // Allocate some objects
    let mut roots = Vec::new();
    for i in 0..100 {
        let ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
        if !ptr.is_null() {
            roots.push(ptr);
        }
    }

    // Perform many rapid collections
    let root_headers: Vec<_> = roots
        .iter()
        .map(|p| unsafe { heap.header_from_payload(*p) })
        .collect();

    // First collection uses full root set.
    let first = heap.collect(&root_headers);
    assert_eq!(
        first.objects_marked,
        root_headers.len(),
        "Initial rooted collection did not mark all roots"
    );

    // Subsequent collections stress collector cycling behavior. We avoid
    // reusing potentially stale root pointers across evacuating collections.
    for i in 1..100 {
        let stats = heap.collect(&[]);

        if i % 20 == 0 {
            println!("Collection {}: marked {} objects", i, stats.objects_marked);
        }
    }
}

#[test]
fn test_zero_size_allocations() {
    let mut heap = ImmixHeap::new();
    let mut ptrs = HashSet::new();

    // Many zero-size allocations
    for i in 0..10000 {
        let ptr = heap.allocate(0, 8);
        // Zero-size allocations may return same pointer or different ones
        ptrs.insert(ptr as usize);

        if i % 1000 == 0 {
            let _ = heap.allocate_object(64, 8, TypeId::new(1));
        }
    }

    // Should have survived without crashing
    let _stats = heap.collect(&[]);
    println!("Zero-size test: {} unique pointers", ptrs.len());
}

#[test]
fn test_extreme_size_allocations() {
    let mut heap = ImmixHeap::new();

    // Very small allocations
    for _i in 0..100 {
        let ptr = heap.allocate(1, 1);
        assert!(!ptr.is_null(), "1-byte allocation failed");
    }

    // Very large alignment
    let ptr = heap.allocate(64, 4096);
    assert!(!ptr.is_null(), "Large alignment allocation failed");
    assert_eq!(ptr as usize % 4096, 0, "Alignment not respected");

    // Just under large object threshold
    let ptr = heap.allocate(LARGE_OBJECT_THRESHOLD - 1, 8);
    assert!(!ptr.is_null(), "Near-threshold allocation failed");

    // Just over large object threshold
    let ptr = heap.allocate(LARGE_OBJECT_THRESHOLD + 1, 8);
    assert!(!ptr.is_null(), "Just-over-threshold allocation failed");
}

// ============================================================================
// Performance Benchmarks (as tests)
// ============================================================================

#[test]
fn benchmark_allocation_latency() {
    let mut heap = ImmixHeap::new();
    let num_samples = 10_000;
    let mut latencies = Vec::with_capacity(num_samples);

    for i in 0..num_samples {
        let start = Instant::now();
        let ptr = heap.allocate_object(64, 8, TypeId::new((i % 100) as u32));
        let elapsed = start.elapsed();

        assert!(!ptr.is_null());
        latencies.push(elapsed.as_nanos() as u64);

        // Periodic collection to simulate realistic conditions
        if i % 1000 == 0 && i > 0 {
            heap.collect(&[]);
        }
    }

    latencies.sort();

    let p50 = latencies[latencies.len() / 2];
    let p99 = latencies[latencies.len() * 99 / 100];
    let p999 = latencies[latencies.len() * 999 / 1000];

    let avg: u64 = latencies.iter().sum::<u64>() / latencies.len() as u64;

    println!("Allocation latency (ns):");
    println!("  Average: {}", avg);
    println!("  p50:     {}", p50);
    println!("  p99:     {}", p99);
    println!("  p99.9:   {}", p999);

    // p99 should be under 10 microseconds (generous for debug builds)
    assert!(
        p99 < 10_000,
        "Allocation latency too high: p99 = {} ns",
        p99
    );
}

#[test]
fn benchmark_collection_time() {
    let mut heap = ImmixHeap::new();
    let mut collection_times = Vec::new();

    // Allocate working set
    let mut roots = Vec::new();
    for i in 0..10000 {
        let ptr = heap.allocate_object(128, 8, TypeId::new((i % 100) as u32));
        if !ptr.is_null() {
            roots.push(ptr);
        }
    }

    let root_headers: Vec<_> = roots
        .iter()
        .map(|p| unsafe { heap.header_from_payload(*p) })
        .collect();

    // Time multiple collections
    for _ in 0..20 {
        // Add some garbage
        for _ in 0..1000 {
            let _ = heap.allocate_object(64, 8, TypeId::new(1));
        }

        let start = Instant::now();
        let stats = heap.collect(&root_headers);
        let elapsed = start.elapsed();

        collection_times.push((elapsed, stats));
    }

    let avg_time: Duration =
        collection_times.iter().map(|(d, _)| *d).sum::<Duration>() / collection_times.len() as u32;

    let avg_marked: usize = collection_times
        .iter()
        .map(|(_, s)| s.objects_marked)
        .sum::<usize>()
        / collection_times.len();

    println!("Collection time:");
    println!("  Average: {:?}", avg_time);
    println!("  Objects marked: {}", avg_marked);

    // Collection should be reasonably fast (generous threshold for debug builds)
    assert!(
        avg_time < Duration::from_millis(100),
        "Collection too slow: {:?}",
        avg_time
    );
}

// Helper module for random number generation in tests
mod rand {
    use std::cell::Cell;

    thread_local! {
        static RNG: Cell<u64> = const { Cell::new(0x123456789abcdef0) };
    }

    pub fn random<T: Random>() -> T {
        T::random()
    }

    pub trait Random {
        fn random() -> Self;
    }

    impl Random for bool {
        fn random() -> Self {
            RNG.with(|rng| {
                let val = rng.get();
                rng.set(val.wrapping_mul(6364136223846793005).wrapping_add(1));
                (val >> 32) & 1 == 1
            })
        }
    }

    impl Random for u32 {
        fn random() -> Self {
            RNG.with(|rng| {
                let val = rng.get();
                rng.set(val.wrapping_mul(6364136223846793005).wrapping_add(1));
                (val >> 32) as u32
            })
        }
    }
}
