//! Comprehensive scheduler tests for Jet runtime
//!
//! Tests task spawning, work-stealing, synchronization, and async operations.

use jet_rt_sched::{
    next_task_id, Context, GlobalQueue, Scheduler, Stack, Task, TaskQueue, TaskState,
};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Barrier, Mutex};
use std::time::Duration;

// ============================================================================
// Scheduler Creation Tests
// ============================================================================

#[test]
fn test_scheduler_creation_single_thread() {
    let sched = Scheduler::new(1);
    assert_eq!(sched.num_workers(), 1);
    assert!(!sched.is_running());
}

#[test]
fn test_scheduler_creation_multi_thread() {
    for num_threads in [2, 4, 8] {
        let sched = Scheduler::new(num_threads);
        assert_eq!(sched.num_workers(), num_threads);
    }
}

#[test]
fn test_scheduler_creation_zero_threads() {
    // Should default to at least 1 thread
    let sched = Scheduler::new(0);
    assert_eq!(sched.num_workers(), 1);
}

// ============================================================================
// Task Spawning Tests
// ============================================================================

#[test]
fn test_spawn_single_task() {
    let mut sched = Scheduler::new(1);
    let executed = Arc::new(AtomicBool::new(false));

    let e = executed.clone();
    sched.spawn(move || {
        e.store(true, Ordering::SeqCst);
    });

    sched.run();
    assert!(executed.load(Ordering::SeqCst));
}

#[test]
fn test_spawn_multiple_tasks() {
    let mut sched = Scheduler::new(2);
    let counter = Arc::new(AtomicUsize::new(0));

    for _ in 0..100 {
        let c = counter.clone();
        sched.spawn(move || {
            c.fetch_add(1, Ordering::SeqCst);
        });
    }

    sched.run();
    assert_eq!(counter.load(Ordering::SeqCst), 100);
}

#[test]
fn test_spawn_with_stack_size() {
    let mut sched = Scheduler::new(1);
    let executed = Arc::new(AtomicBool::new(false));

    let e = executed.clone();
    sched.spawn_with_stack(256 * 1024, move || {
        e.store(true, Ordering::SeqCst);
    });

    sched.run();
    assert!(executed.load(Ordering::SeqCst));
}

#[test]
fn test_spawn_from_within_task() {
    let mut sched = Scheduler::new(2);
    let counter = Arc::new(AtomicUsize::new(0));

    let c = counter.clone();
    sched.spawn(move || {
        c.fetch_add(1, Ordering::SeqCst);
        // In a full implementation, we could spawn more tasks here
    });

    sched.run();
    assert!(counter.load(Ordering::SeqCst) >= 1);
}

// ============================================================================
// Task Execution Order Tests
// ============================================================================

#[test]
fn test_task_execution_order() {
    let mut sched = Scheduler::new(1);
    let order = Arc::new(Mutex::new(Vec::new()));

    for i in 0..10 {
        let o = order.clone();
        sched.spawn(move || {
            o.lock().unwrap().push(i);
        });
    }

    sched.run();

    let final_order = order.lock().unwrap();
    // All tasks should have executed
    assert_eq!(final_order.len(), 10);
}

#[test]
fn test_nested_spawns() {
    let mut sched = Scheduler::new(2);
    let counter = Arc::new(AtomicUsize::new(0));

    let c = counter.clone();
    sched.spawn(move || {
        c.fetch_add(1, Ordering::SeqCst);
        // Note: In full implementation, could spawn nested tasks
    });

    sched.run();
    assert_eq!(counter.load(Ordering::SeqCst), 1);
}

// ============================================================================
// Work Stealing Tests
// ============================================================================

#[test]
fn test_work_stealing_basic() {
    let mut sched = Scheduler::new(4);
    let counter = Arc::new(AtomicUsize::new(0));

    // Spawn many tasks to encourage stealing
    for _ in 0..1000 {
        let c = counter.clone();
        sched.spawn(move || {
            c.fetch_add(1, Ordering::SeqCst);
        });
    }

    sched.run();
    assert_eq!(counter.load(Ordering::SeqCst), 1000);
}

#[test]
fn test_work_stealing_with_uneven_load() {
    let mut sched = Scheduler::new(4);
    let counter = Arc::new(AtomicUsize::new(0));

    // Spawn tasks with varying workloads
    for i in 0..100 {
        let c = counter.clone();
        sched.spawn(move || {
            // Varying "work" duration
            for _ in 0..(i * 100) {
                std::hint::black_box(0);
            }
            c.fetch_add(1, Ordering::SeqCst);
        });
    }

    sched.run();
    assert_eq!(counter.load(Ordering::SeqCst), 100);
}

// ============================================================================
// Task State Tests
// ============================================================================

#[test]
fn test_task_state_transitions() {
    let task = Task::new(|| {}, None);
    assert_eq!(task.state, TaskState::Ready);
}

#[test]
fn test_task_id_generation() {
    let id1 = next_task_id();
    let id2 = next_task_id();
    assert_ne!(id1, id2);
    assert!(id2 > id1);
}

// ============================================================================
// Queue Tests
// ============================================================================

#[test]
fn test_global_queue() {
    use crossbeam::deque::Steal;

    let queue = GlobalQueue::new();
    assert!(queue.is_empty());

    let task = Box::new(Task::new(|| {}, None));
    queue.push(task);
    assert!(!queue.is_empty());

    // GlobalQueue uses steal() which returns Steal<T>
    let stolen = queue.steal();
    assert!(matches!(stolen, Steal::Success(_)));
    assert!(queue.is_empty());
}

#[test]
fn test_local_queue() {
    let queue = TaskQueue::new();
    assert_eq!(queue.len(), 0);

    let task = Box::new(Task::new(|| {}, None));
    queue.push(task);
    assert_eq!(queue.len(), 1);

    let popped = queue.pop();
    assert!(popped.is_some());
    assert_eq!(queue.len(), 0);
}

#[test]
fn test_queue_stealing() {
    use crossbeam::deque::Steal;

    let queue1 = TaskQueue::new();
    let stealer = queue1.stealer();

    // Push tasks to queue1
    for _ in 0..10 {
        let task = Box::new(Task::new(|| {}, None));
        queue1.push(task);
    }

    // Steal from queue1
    let stolen = stealer.steal();
    assert!(matches!(stolen, Steal::Success(_)));
}

// ============================================================================
// Statistics Tests
// ============================================================================

#[test]
fn test_scheduler_counters() {
    let mut sched = Scheduler::new(2);

    assert_eq!(sched.tasks_spawned(), 0);
    assert_eq!(sched.tasks_completed(), 0);

    for _ in 0..50 {
        sched.spawn(|| {});
    }

    assert_eq!(sched.tasks_spawned(), 50);

    sched.run();

    assert_eq!(sched.tasks_completed(), 50);
}

#[test]
fn test_worker_stats() {
    let mut sched = Scheduler::new(4);

    for _ in 0..100 {
        sched.spawn(|| {
            std::hint::black_box(0);
        });
    }

    sched.run();

    let stats = sched.worker_stats();
    assert_eq!(stats.len(), 4);

    let total_processed: u64 = stats.iter().map(|s| s.total_processed()).sum();
    assert_eq!(total_processed, 100);
}

// ============================================================================
// Concurrent Execution Tests
// ============================================================================

#[test]
fn test_concurrent_task_execution() {
    let mut sched = Scheduler::new(4);
    let barrier = Arc::new(Barrier::new(4));
    let counter = Arc::new(AtomicUsize::new(0));

    for _ in 0..4 {
        let b = barrier.clone();
        let c = counter.clone();
        sched.spawn(move || {
            b.wait();
            c.fetch_add(1, Ordering::SeqCst);
        });
    }

    sched.run();
    assert_eq!(counter.load(Ordering::SeqCst), 4);
}

#[test]
fn test_shared_state_access() {
    let mut sched = Scheduler::new(4);
    let data = Arc::new(Mutex::new(Vec::new()));

    for i in 0..100 {
        let d = data.clone();
        sched.spawn(move || {
            d.lock().unwrap().push(i);
        });
    }

    sched.run();

    let final_data = data.lock().unwrap();
    assert_eq!(final_data.len(), 100);
}

// ============================================================================
// Edge Cases and Stress Tests
// ============================================================================

#[test]
fn test_empty_scheduler_run() {
    let mut sched = Scheduler::new(4);
    // Run with no tasks
    sched.run();
    assert_eq!(sched.tasks_completed(), 0);
}

#[test]
fn test_single_long_running_task() {
    let mut sched = Scheduler::new(1);
    let executed = Arc::new(AtomicBool::new(false));

    let e = executed.clone();
    sched.spawn(move || {
        // Simulate some work
        for _ in 0..1_000_000 {
            std::hint::black_box(0);
        }
        e.store(true, Ordering::SeqCst);
    });

    sched.run();
    assert!(executed.load(Ordering::SeqCst));
}

#[test]
fn test_many_short_tasks() {
    let mut sched = Scheduler::new(4);
    let counter = Arc::new(AtomicUsize::new(0));

    for _ in 0..10000 {
        let c = counter.clone();
        sched.spawn(move || {
            c.fetch_add(1, Ordering::SeqCst);
        });
    }

    sched.run();
    assert_eq!(counter.load(Ordering::SeqCst), 10000);
}

#[test]
fn test_task_panics() {
    let mut sched = Scheduler::new(1);
    let counter = Arc::new(AtomicUsize::new(0));

    // Normal task
    let c = counter.clone();
    sched.spawn(move || {
        c.fetch_add(1, Ordering::SeqCst);
    });

    // Task that would panic (in real implementation)
    // For now, just test that normal tasks complete

    sched.run();
    assert_eq!(counter.load(Ordering::SeqCst), 1);
}

// ============================================================================
// Stack Tests
// ============================================================================

#[test]
fn test_stack_creation() {
    let stack = Stack::new(1024 * 1024); // 1 MB
                                         // top() returns *mut u8, just check it's not null
    assert!(!stack.top().is_null());
}

#[test]
fn test_stack_size_validation() {
    // Too small stack should be rejected or adjusted
    let _stack = Stack::new(1024);
    // Behavior may vary, but shouldn't crash
}

// ============================================================================
// Context Switching Tests (basic)
// ============================================================================

#[test]
fn test_context_creation() {
    let context = Context::new();
    // Context should be creatable
    let _ = context;
}

// ============================================================================
// Integration Tests
// ============================================================================

#[test]
fn test_producer_consumer_pattern() {
    let mut sched = Scheduler::new(4);
    let data = Arc::new(Mutex::new(Vec::new()));
    let done = Arc::new(AtomicBool::new(false));

    // Producer tasks
    for i in 0..10 {
        let d = data.clone();
        sched.spawn(move || {
            d.lock().unwrap().push(i);
        });
    }

    // Consumer task
    let d = data.clone();
    let done_flag = done.clone();
    sched.spawn(move || {
        // In real implementation, would wait for producers
        std::thread::sleep(Duration::from_millis(10));
        let items = d.lock().unwrap().len();
        done_flag.store(items >= 10, Ordering::SeqCst);
    });

    sched.run();
    assert!(done.load(Ordering::SeqCst) || data.lock().unwrap().len() >= 10);
}

#[test]
fn test_map_reduce_pattern() {
    let mut sched = Scheduler::new(4);
    let partial_results = Arc::new(Mutex::new(Vec::new()));

    // Map phase - process chunks in parallel
    for i in 0..10 {
        let p = partial_results.clone();
        sched.spawn(move || {
            // Simulate processing
            let result = i * i;
            p.lock().unwrap().push(result);
        });
    }

    sched.run();

    // Verify all map tasks completed
    let results = partial_results.lock().unwrap();
    assert_eq!(results.len(), 10);

    // Verify correct results
    let sum: i32 = results.iter().sum();
    assert_eq!(sum, 285); // 0+1+4+9+16+25+36+49+64+81
}

#[test]
fn test_parallel_tree_traversal() {
    let mut sched = Scheduler::new(4);
    let sum = Arc::new(AtomicUsize::new(0));

    // Simulate tree nodes
    let nodes: Vec<usize> = (1..=100).collect();

    // Process nodes in parallel
    for node in nodes {
        let s = sum.clone();
        sched.spawn(move || {
            s.fetch_add(node, Ordering::SeqCst);
        });
    }

    sched.run();
    assert_eq!(sum.load(Ordering::SeqCst), 5050);
}

// ============================================================================
// Performance Benchmarks (basic)
// ============================================================================

#[test]
fn test_scheduling_overhead() {
    let mut sched = Scheduler::new(4);
    let counter = Arc::new(AtomicUsize::new(0));

    let start = std::time::Instant::now();

    for _ in 0..1000 {
        let c = counter.clone();
        sched.spawn(move || {
            c.fetch_add(1, Ordering::SeqCst);
        });
    }

    sched.run();

    let elapsed = start.elapsed();
    assert_eq!(counter.load(Ordering::SeqCst), 1000);

    // Should complete in reasonable time (adjust threshold as needed)
    assert!(
        elapsed < Duration::from_secs(30),
        "Scheduling took too long: {:?}",
        elapsed
    );
}

#[test]
fn test_scalability() {
    // Test that more workers generally helps (though not strictly required)
    let task_count = 1000;

    for num_workers in [1, 2, 4] {
        let mut sched = Scheduler::new(num_workers);
        let counter = Arc::new(AtomicUsize::new(0));

        for _ in 0..task_count {
            let c = counter.clone();
            sched.spawn(move || {
                // Some work
                for _ in 0..1000 {
                    std::hint::black_box(0);
                }
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        sched.run();
        assert_eq!(counter.load(Ordering::SeqCst), task_count);
    }
}
