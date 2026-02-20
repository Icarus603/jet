//! Garbage collector benchmarks
//!
//! Measures GC performance: allocation speed, collection time, pause times.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use jet_rt_gc::{ImmixHeap, TypeId};
use std::time::Instant;

/// Benchmark allocation throughput
fn gc_allocation_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("gc/allocation");
    group.sample_size(20);
    group.warm_up_time(std::time::Duration::from_millis(500));
    group.measurement_time(std::time::Duration::from_secs(2));

    // Benchmark small object allocation throughput
    for size in [16, 32, 64, 128].iter() {
        group.throughput(Throughput::Bytes(*size as u64 * 100));
        group.bench_with_input(BenchmarkId::new("small_objects", size), size, |b, &size| {
            b.iter(|| {
                let mut heap = ImmixHeap::new();
                for i in 0..100 {
                    let _ptr = heap.allocate_object(size, 8, TypeId::new(i as u32));
                }
                black_box(heap);
            });
        });
    }

    // Benchmark various object sizes
    for size in [256, 512, 1024, 4096].iter() {
        group.throughput(Throughput::Bytes(*size as u64 * 50));
        group.bench_with_input(
            BenchmarkId::new("medium_objects", size),
            size,
            |b, &size| {
                b.iter(|| {
                    let mut heap = ImmixHeap::new();
                    for i in 0..50 {
                        let _ptr = heap.allocate_object(size, 8, TypeId::new(i as u32));
                    }
                    black_box(heap);
                });
            },
        );
    }

    // Benchmark large object space allocation
    for size in [8192, 16384, 32768].iter() {
        group.throughput(Throughput::Bytes(*size as u64 * 5));
        group.bench_with_input(BenchmarkId::new("large_objects", size), size, |b, &size| {
            b.iter(|| {
                let mut heap = ImmixHeap::new();
                for i in 0..5 {
                    let _ptr = heap.allocate_object(size, 8, TypeId::new(i as u32));
                }
                black_box(heap);
            });
        });
    }

    // Benchmark allocation with different alignments
    for align in [8, 16, 32, 64].iter() {
        group.bench_with_input(BenchmarkId::new("alignment", align), align, |b, &align| {
            b.iter(|| {
                let mut heap = ImmixHeap::new();
                for i in 0..100 {
                    let _ptr = heap.allocate_object(64, align, TypeId::new(i as u32));
                }
                black_box(heap);
            });
        });
    }

    group.finish();
}

/// Benchmark GC collection performance
fn gc_collection_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("gc/collection");
    group.sample_size(20);
    group.warm_up_time(std::time::Duration::from_millis(500));
    group.measurement_time(std::time::Duration::from_secs(2));

    // Benchmark nursery collection with different object counts
    for count in [50, 100, 250, 500].iter() {
        group.bench_with_input(
            BenchmarkId::new("nursery_collection", count),
            count,
            |b, &count| {
                b.iter(|| {
                    let mut heap = ImmixHeap::new();

                    // Allocate objects
                    let mut roots = Vec::new();
                    for i in 0..count {
                        let ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
                        if i % 10 == 0 {
                            // Keep 10% as roots
                            roots.push(ptr);
                        }
                    }

                    // Collect nursery
                    let root_headers: Vec<_> = roots
                        .iter()
                        .map(|&p| unsafe { heap.header_from_payload(p) })
                        .collect();
                    let _stats = heap.collect_nursery(&root_headers);

                    black_box(heap);
                });
            },
        );
    }

    // Benchmark full collection with different heap sizes
    for count in [50, 100, 250].iter() {
        group.bench_with_input(
            BenchmarkId::new("full_collection", count),
            count,
            |b, &count| {
                b.iter(|| {
                    let mut heap = ImmixHeap::new();

                    // Allocate objects
                    let mut roots = Vec::new();
                    for i in 0..count {
                        let ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
                        if i % 10 == 0 {
                            roots.push(ptr);
                        }
                    }

                    // Collect full heap
                    let root_headers: Vec<_> = roots
                        .iter()
                        .map(|&p| unsafe { heap.header_from_payload(p) })
                        .collect();
                    let _stats = heap.collect(&root_headers);

                    black_box(heap);
                });
            },
        );
    }

    group.finish();
}

/// Benchmark GC pause times (microbenchmarks for individual operations)
fn gc_pause_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("gc/pause_times");

    // Measure individual allocation pause
    group.bench_function("allocation_pause", |b| {
        let mut heap = ImmixHeap::new();
        b.iter(|| {
            let ptr = heap.allocate_object(64, 8, TypeId::new(1));
            black_box(ptr);
        });
    });

    // Measure write barrier cost
    group.bench_function("write_barrier", |b| {
        b.iter(|| {
            // Simulate write barrier check
            black_box(42u8);
        });
    });

    // Measure safe point check cost
    group.bench_function("safe_point_check", |b| {
        b.iter(|| {
            // Simulate safe point check
            black_box(false);
        });
    });

    group.finish();
}

/// Benchmark object churn rate (allocation + deallocation cycles)
fn gc_churn_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("gc/churn");
    group.sample_size(20);
    group.warm_up_time(std::time::Duration::from_millis(500));
    group.measurement_time(std::time::Duration::from_secs(2));

    // Benchmark high churn rate (many short-lived objects)
    for cycles in [10, 50, 100].iter() {
        group.throughput(Throughput::Elements(*cycles as u64));
        group.bench_with_input(
            BenchmarkId::new("high_churn", cycles),
            cycles,
            |b, &cycles| {
                b.iter(|| {
                    let mut heap = ImmixHeap::new();

                    for _ in 0..cycles {
                        // Allocate and immediately lose reference
                        for i in 0..50 {
                            let _ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
                        }

                        // Trigger collection
                        let _stats = heap.collect_nursery(&[]);
                    }

                    black_box(heap);
                });
            },
        );
    }

    // Benchmark medium churn rate (some objects survive)
    for cycles in [10, 50].iter() {
        group.throughput(Throughput::Elements(*cycles as u64));
        group.bench_with_input(
            BenchmarkId::new("medium_churn", cycles),
            cycles,
            |b, &cycles| {
                b.iter(|| {
                    let mut heap = ImmixHeap::new();
                    let mut survivors: Vec<*mut u8> = Vec::new();

                    for cycle in 0..cycles {
                        // Allocate new objects
                        let mut new_roots = Vec::new();
                        for i in 0..50 {
                            let ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
                            if i % 5 == 0 {
                                new_roots.push(ptr);
                            }
                        }

                        // Add some survivors to roots
                        let mut all_roots = survivors.clone();
                        all_roots.extend(new_roots);

                        // Collect with roots
                        let root_headers: Vec<_> = all_roots
                            .iter()
                            .map(|&p| unsafe { heap.header_from_payload(p) })
                            .collect();
                        let _stats = heap.collect_nursery(&root_headers);

                        // Update survivors (keep some)
                        survivors = all_roots.into_iter().take(25).collect();

                        black_box(cycle);
                    }

                    black_box(heap);
                });
            },
        );
    }

    group.finish();
}

/// Benchmark stress scenarios
fn gc_stress_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("gc/stress");
    group.sample_size(10);
    group.warm_up_time(std::time::Duration::from_millis(500));
    group.measurement_time(std::time::Duration::from_secs(2));

    // High allocation rate stress test
    group.bench_function("high_allocation_rate", |b| {
        b.iter(|| {
            let mut heap = ImmixHeap::new();
            let start = Instant::now();
            let duration = std::time::Duration::from_millis(50);
            let mut count = 0;

            while start.elapsed() < duration {
                let _ptr = heap.allocate_object(64, 8, TypeId::new(count as u32));
                count += 1;

                // Trigger occasional collections
                if count % 5000 == 0 {
                    let _stats = heap.collect_nursery(&[]);
                }
            }

            black_box(count);
            black_box(heap);
        });
    });

    // Fragmentation stress test (allocate/deallocate pattern)
    group.bench_function("fragmentation", |b| {
        b.iter(|| {
            let mut heap = ImmixHeap::new();
            let mut objects: Vec<Option<*mut u8>> = Vec::new();

            // Allocate many objects
            for i in 0..500 {
                let ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
                objects.push(Some(ptr));
            }

            // Free every other object
            for i in (0..objects.len()).step_by(2) {
                objects[i] = None;
            }

            // Collect to create fragmentation
            let roots: Vec<_> = objects
                .iter()
                .filter_map(|&o| o)
                .map(|p| unsafe { heap.header_from_payload(p) })
                .collect();
            let _stats = heap.collect(&roots);

            // Try to allocate in fragmented space
            for i in 0..250 {
                let _ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
            }

            black_box(heap);
        });
    });

    // Large object stress test
    group.bench_function("large_objects", |b| {
        b.iter(|| {
            let mut heap = ImmixHeap::new();

            // Allocate many large objects
            for i in 0..50 {
                let _ptr = heap.allocate_object(16384, 8, TypeId::new(i as u32));
            }

            // Collect
            let _stats = heap.collect(&[]);

            // Allocate more
            for i in 0..50 {
                let _ptr = heap.allocate_object(16384, 8, TypeId::new((i + 100) as u32));
            }

            black_box(heap);
        });
    });

    // Mixed size allocation stress
    group.bench_function("mixed_sizes", |b| {
        b.iter(|| {
            let mut heap = ImmixHeap::new();
            let sizes = [16, 32, 64, 128, 256, 512, 1024, 2048];

            for i in 0..500 {
                let size = sizes[i % sizes.len()];
                let _ptr = heap.allocate_object(size, 8, TypeId::new(i as u32));
            }

            let _stats = heap.collect_nursery(&[]);

            black_box(heap);
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    gc_allocation_benchmark,
    gc_collection_benchmark,
    gc_pause_benchmark,
    gc_churn_benchmark,
    gc_stress_benchmark
);
criterion_main!(benches);
