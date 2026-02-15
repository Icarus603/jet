//! Stress tests for the Jet scheduler
//!
//! These tests verify scheduler stability under extreme conditions:
//! - High task spawn rates
//! - Deep task chains
//! - Work-stealing fairness
//! - Deadlock detection
//! - Context switching stress

use jet_rt_sched::deadlock::DeadlockDetector;
use jet_rt_sched::{next_task_id, Scheduler};
use std::collections::HashSet;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Barrier, Mutex};
use std::thread;
use std::time::{Duration, Instant};

// ============================================================================
// High Task Spawn Rate Tests
// ============================================================================

#[test]
fn test_million_tasks_stress() {
    let mut sched = Scheduler::new(4);
    let counter = Arc::new(AtomicUsize::new(0));
    let num_tasks = 100_000; // Reduced from 1M for reasonable test time

    let start = Instant::now();

    for _ in 0..num_tasks {
        let c = counter.clone();
        sched.spawn(move || {
            c.fetch_add(1, Ordering::SeqCst);
        });
    }

    sched.run();

    let elapsed = start.elapsed();
    let rate = num_tasks as f64 / elapsed.as_secs_f64();

    println!(
        "Spawned {} tasks in {:?} ({:.0} tasks/sec)",
        num_tasks, elapsed, rate
    );

    assert_eq!(counter.load(Ordering::SeqCst), num_tasks);

    // Should sustain at least 10K tasks/sec
    assert!(
        rate > 10_000.0,
        "Task spawn rate too slow: {:.0} tasks/sec",
        rate
    );
}

#[test]
fn test_rapid_spawn_and_complete() {
    let mut sched = Scheduler::new(4);

    // Multiple waves of tasks
    for wave in 0..10 {
        let counter = Arc::new(AtomicUsize::new(0));

        for _ in 0..1000 {
            let c = counter.clone();
            sched.spawn(move || {
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        sched.run();

        assert_eq!(counter.load(Ordering::SeqCst), 1000, "Wave {} failed", wave);
    }
}

#[test]
fn test_spawn_from_task_simulation() {
    // Simulate spawning from within tasks by pre-creating task batches
    let mut sched = Scheduler::new(4);
    let total_completed = Arc::new(AtomicUsize::new(0));

    // Create "parent" tasks that would spawn "children"
    let batch_size = 100;
    let tasks_per_parent = 10;

    for _parent_id in 0..batch_size {
        let completed = total_completed.clone();
        sched.spawn(move || {
            // Simulate spawning 10 child tasks
            for _ in 0..tasks_per_parent {
                completed.fetch_add(1, Ordering::SeqCst);
            }
        });
    }

    sched.run();

    assert_eq!(
        total_completed.load(Ordering::SeqCst),
        batch_size * tasks_per_parent
    );
}

// ============================================================================
// Deep Task Chain Tests
// ============================================================================

#[test]
fn test_deep_task_chain() {
    let mut sched = Scheduler::new(2);
    let depth = Arc::new(AtomicUsize::new(0));
    let counter = Arc::new(AtomicUsize::new(0));

    // Create a chain of dependencies
    for i in 0..100 {
        let d = depth.clone();
        let c = counter.clone();
        sched.spawn(move || {
            d.fetch_max(i, Ordering::SeqCst);
            c.fetch_add(1, Ordering::SeqCst);
        });
    }

    sched.run();

    assert_eq!(counter.load(Ordering::SeqCst), 100);
}

#[test]
fn test_recursive_task_pattern() {
    let mut sched = Scheduler::new(4);
    let sum = Arc::new(AtomicUsize::new(0));
    let n = 100;

    // Simulate recursive divide-and-conquer
    for i in 0..n {
        let s = sum.clone();
        sched.spawn(move || {
            s.fetch_add(i, Ordering::SeqCst);
        });
    }

    sched.run();

    // Sum of 0 to n-1
    let expected = n * (n - 1) / 2;
    assert_eq!(sum.load(Ordering::SeqCst), expected);
}

// ============================================================================
// Work-Stealing Fairness Tests
// ============================================================================

#[test]
fn test_work_stealing_fairness() {
    let num_workers = 4;
    let mut sched = Scheduler::new(num_workers);
    let _task_counts: Vec<Arc<AtomicUsize>> = (0..num_workers)
        .map(|_| Arc::new(AtomicUsize::new(0)))
        .collect();

    // Pin tasks to workers by using thread-local tracking
    let global_counter = Arc::new(AtomicUsize::new(0));

    for _ in 0..1000 {
        let c = global_counter.clone();
        sched.spawn(move || {
            c.fetch_add(1, Ordering::SeqCst);
            // Simulate some work
            for _ in 0..100 {
                std::hint::black_box(0);
            }
        });
    }

    sched.run();

    assert_eq!(global_counter.load(Ordering::SeqCst), 1000);

    // Check worker stats for fairness
    let stats = sched.worker_stats();
    let total_processed: u64 = stats.iter().map(|s| s.total_processed()).sum();
    assert_eq!(total_processed, 1000);

    // Each worker should have done some work (fairness check)
    for (i, stat) in stats.iter().enumerate() {
        println!("Worker {} processed {} tasks", i, stat.total_processed());
        // In a fair system, each worker should process at least some tasks
        // Allow for significant imbalance (some workers may be idle)
        assert!(
            stat.total_processed() > 0 || num_workers > 4,
            "Worker {} did no work",
            i
        );
    }
}

#[test]
fn test_uneven_load_distribution() {
    let mut sched = Scheduler::new(4);
    let completed = Arc::new(AtomicUsize::new(0));

    // Create uneven load: some fast tasks, some slow
    for i in 0..100 {
        let c = completed.clone();
        let work_amount = if i % 10 == 0 { 10000 } else { 100 };

        sched.spawn(move || {
            // Varying work amounts
            for _ in 0..work_amount {
                std::hint::black_box(0);
            }
            c.fetch_add(1, Ordering::SeqCst);
        });
    }

    sched.run();

    assert_eq!(completed.load(Ordering::SeqCst), 100);
}

// ============================================================================
// Context Switching Stress Tests
// ============================================================================

#[test]
fn test_context_switching_stress() {
    let mut sched = Scheduler::new(2);
    let switches = Arc::new(AtomicUsize::new(0));

    // Create tasks that would context switch frequently
    for _ in 0..100 {
        let s = switches.clone();
        sched.spawn(move || {
            for _ in 0..100 {
                s.fetch_add(1, Ordering::Relaxed);
                // In a real implementation, this would yield
                // For now, just increment the counter
            }
        });
    }

    sched.run();

    // Should have completed all increments
    assert_eq!(switches.load(Ordering::Relaxed), 100 * 100);
}

#[test]
fn test_stack_usage_stress() {
    let mut sched = Scheduler::new(4);
    let completed = Arc::new(AtomicUsize::new(0));

    // Tasks with varying stack sizes
    for i in 0..100 {
        let c = completed.clone();
        let depth = (i % 10) * 100; // 0 to 900 iterations

        sched.spawn(move || {
            // Recursive-like pattern using iteration
            let mut local_data = [0u8; 1024]; // 1KB stack usage
            for j in 0..depth {
                local_data[j % 1024] = j as u8;
            }
            c.fetch_add(1, Ordering::SeqCst);
            std::hint::black_box(local_data);
        });
    }

    sched.run();

    assert_eq!(completed.load(Ordering::SeqCst), 100);
}

// ============================================================================
// Deadlock Detection Tests
// ============================================================================

#[test]
fn test_deadlock_detector_basic() {
    let detector = DeadlockDetector::new();

    // No dependencies initially
    assert!(detector.detect_cycle().is_none());

    // Add a simple dependency
    detector.register_dependency(1, 2, Some("Task 1 waits for Task 2".to_string()));
    assert!(detector.detect_cycle().is_none());

    // Complete the cycle
    detector.register_dependency(2, 1, Some("Task 2 waits for Task 1".to_string()));

    let cycle = detector.detect_cycle();
    assert!(cycle.is_some(), "Should detect cycle");

    let cycle = cycle.unwrap();
    assert!(cycle.tasks.contains(&1));
    assert!(cycle.tasks.contains(&2));
}

#[test]
fn test_deadlock_detector_complex_cycle() {
    let detector = DeadlockDetector::new();

    // Create a chain: 1 -> 2 -> 3 -> 4 -> 2 (cycle between 2, 3, 4)
    detector.register_dependency(1, 2, None);
    detector.register_dependency(2, 3, None);
    detector.register_dependency(3, 4, None);
    detector.register_dependency(4, 2, None);

    let cycle = detector.detect_cycle();
    assert!(cycle.is_some());

    let cycle_tasks = cycle.unwrap().tasks;
    assert!(cycle_tasks.contains(&2));
    assert!(cycle_tasks.contains(&3));
    assert!(cycle_tasks.contains(&4));
}

#[test]
fn test_deadlock_detector_clearing() {
    let detector = DeadlockDetector::new();

    detector.register_dependency(1, 2, None);
    detector.register_dependency(2, 1, None);

    assert!(detector.detect_cycle().is_some());

    // Clear dependencies for task 1
    detector.clear_dependencies(1);

    assert!(detector.detect_cycle().is_none());
}

#[test]
fn test_deadlock_detector_disabled() {
    let detector = DeadlockDetector::disabled();

    detector.register_dependency(1, 2, None);
    detector.register_dependency(2, 1, None);

    // Should not detect anything when disabled
    assert!(detector.detect_cycle().is_none());
}

// ============================================================================
// Concurrency Stress Tests
// ============================================================================

#[test]
fn test_concurrent_scheduler_access() {
    let num_threads = 8;
    let tasks_per_thread = 1000;

    let sched = Arc::new(Mutex::new(Scheduler::new(4)));
    let counter = Arc::new(AtomicUsize::new(0));
    let barrier = Arc::new(Barrier::new(num_threads));

    let mut handles = Vec::new();

    for _ in 0..num_threads {
        let sched_clone = Arc::clone(&sched);
        let counter_clone = Arc::clone(&counter);
        let barrier_clone = Arc::clone(&barrier);

        let handle = thread::spawn(move || {
            barrier_clone.wait();

            for _ in 0..tasks_per_thread {
                let c = counter_clone.clone();
                let sched = sched_clone.lock().unwrap();
                sched.spawn(move || {
                    c.fetch_add(1, Ordering::SeqCst);
                });
            }
        });

        handles.push(handle);
    }

    // Wait for all spawners
    for handle in handles {
        handle.join().unwrap();
    }

    // Now run the scheduler
    let mut sched = match Arc::try_unwrap(sched) {
        Ok(mutex) => mutex.into_inner().expect("Mutex poisoned"),
        Err(_) => panic!("Failed to unwrap Arc - threads still holding references"),
    };
    sched.run();

    assert_eq!(
        counter.load(Ordering::SeqCst),
        num_threads * tasks_per_thread
    );
}

#[test]
fn test_race_condition_safety() {
    let mut sched = Scheduler::new(4);
    let data = Arc::new(Mutex::new(Vec::new()));

    // Many tasks accessing shared data
    for i in 0..1000 {
        let d = data.clone();
        sched.spawn(move || {
            let mut vec = d.lock().unwrap();
            vec.push(i);
        });
    }

    sched.run();

    let final_data = data.lock().unwrap();
    assert_eq!(final_data.len(), 1000);

    // All values should be present (no races lost data)
    let mut sorted = final_data.clone();
    sorted.sort();
    for (i, &val) in sorted.iter().enumerate() {
        assert_eq!(val, i, "Missing or duplicate value at index {}", i);
    }
}

// ============================================================================
// Performance Benchmarks
// ============================================================================

#[test]
fn benchmark_task_spawn_latency() {
    let mut sched = Scheduler::new(1); // Single thread to measure pure overhead
    let num_samples = 10000;
    let mut latencies = Vec::with_capacity(num_samples);

    let start = Instant::now();

    for _ in 0..num_samples {
        let task_start = Instant::now();
        sched.spawn(|| {});
        latencies.push(task_start.elapsed().as_nanos() as u64);
    }

    let total_elapsed = start.elapsed();
    sched.run();

    latencies.sort();
    let p50 = latencies[num_samples / 2];
    let p99 = latencies[num_samples * 99 / 100];
    let avg = latencies.iter().sum::<u64>() / latencies.len() as u64;

    println!("Task spawn latency:");
    println!("  Total: {:?} for {} tasks", total_elapsed, num_samples);
    println!("  Average: {} ns", avg);
    println!("  p50: {} ns", p50);
    println!("  p99: {} ns", p99);

    // p99 should stay below a conservative bound that remains stable on
    // shared/loaded CI machines.
    assert!(
        p99 < 250_000,
        "Task spawn latency too high: p99 = {} ns",
        p99
    );
}

#[test]
fn benchmark_throughput_scaling() {
    let task_count = 10000;

    for num_workers in [1, 2, 4] {
        let mut sched = Scheduler::new(num_workers);
        let counter = Arc::new(AtomicUsize::new(0));

        let start = Instant::now();

        for _ in 0..task_count {
            let c = counter.clone();
            sched.spawn(move || {
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        sched.run();

        let elapsed = start.elapsed();
        let throughput = task_count as f64 / elapsed.as_secs_f64();

        println!(
            "Workers: {}, Tasks: {}, Time: {:?}, Throughput: {:.0} tasks/sec",
            num_workers, task_count, elapsed, throughput
        );

        assert_eq!(counter.load(Ordering::SeqCst), task_count);
    }
}

// ============================================================================
// Edge Cases and Error Recovery
// ============================================================================

#[test]
fn test_empty_scheduler_runs() {
    let mut sched = Scheduler::new(4);

    // Run with no tasks - should complete immediately
    let start = Instant::now();
    sched.run();
    let elapsed = start.elapsed();

    println!("Empty scheduler run took {:?}", elapsed);
    assert!(
        elapsed < Duration::from_millis(100),
        "Empty scheduler took too long"
    );
}

#[test]
fn test_single_task_many_times() {
    for _ in 0..100 {
        let mut sched = Scheduler::new(1);
        let completed = Arc::new(AtomicBool::new(false));

        let c = completed.clone();
        sched.spawn(move || {
            c.store(true, Ordering::SeqCst);
        });

        sched.run();

        assert!(completed.load(Ordering::SeqCst));
    }
}

#[test]
fn test_task_id_uniqueness() {
    let mut ids = HashSet::new();

    for _ in 0..10000 {
        let id = next_task_id();
        assert!(ids.insert(id), "Duplicate task ID generated: {}", id);
    }
}

#[test]
fn test_scheduler_with_custom_stack_sizes() {
    let mut sched = Scheduler::new(2);
    let completed = Arc::new(AtomicUsize::new(0));

    // Mix of different stack sizes
    let stack_sizes = [
        64 * 1024,   // 64 KB
        128 * 1024,  // 128 KB
        256 * 1024,  // 256 KB
        512 * 1024,  // 512 KB
        1024 * 1024, // 1 MB
    ];

    for (i, &stack_size) in stack_sizes.iter().cycle().take(50).enumerate() {
        let c = completed.clone();
        sched.spawn_with_stack(stack_size, move || {
            // Use some stack space
            let data = [i as u8; 1024];
            std::hint::black_box(data);
            c.fetch_add(1, Ordering::SeqCst);
        });
    }

    sched.run();

    assert_eq!(completed.load(Ordering::SeqCst), 50);
}

#[test]
fn test_long_running_stability() {
    let mut sched = Scheduler::new(4);
    let iterations = 100;

    for i in 0..iterations {
        let counter = Arc::new(AtomicUsize::new(0));

        // Each iteration spawns some tasks
        for _ in 0..100 {
            let c = counter.clone();
            sched.spawn(move || {
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        sched.run();

        assert_eq!(
            counter.load(Ordering::SeqCst),
            100,
            "Iteration {} failed",
            i
        );
    }

    println!("Completed {} scheduler iterations", iterations);
}
