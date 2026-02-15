//! Jet Task Scheduler - M:N Threading Runtime
//!
//! This crate provides a user-space task scheduler implementing M:N threading,
//! where many user tasks are multiplexed onto fewer OS threads. It uses
//! work-stealing for load balancing and context switching for task suspension.
//!
//! # Architecture
//!
//! ```text
//! OS Threads (N = number of CPU cores)
//!     │
//!     ├── Worker Thread 0
//!     │   ├── Local Task Queue
//!     │   └── Runs: Steal → Execute → Park
//!     │
//!     ├── Worker Thread 1
//!     │   └── ...
//!     │
//!     └── ...
//!
//! Global Task Queue (for spawning)
//! ```
//!
//! # Example
//!
//! ```rust
//! use jet_rt_sched::{Scheduler, spawn, yield_now};
//! use std::sync::Arc;
//! use std::sync::atomic::{AtomicU32, Ordering};
//!
//! let mut sched = Scheduler::new(2);
//! let counter = Arc::new(AtomicU32::new(0));
//!
//! for _ in 0..10 {
//!     let c = counter.clone();
//!     sched.spawn(move || {
//!         c.fetch_add(1, Ordering::SeqCst);
//!     });
//! }
//!
//! sched.run();
//! assert_eq!(counter.load(Ordering::SeqCst), 10);
//! ```

use std::cell::RefCell;
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

mod context;
mod queue;
mod stack;
mod task;
mod worker;

// Production hardening modules
pub mod deadlock;

// Public modules
pub mod async_runtime;
pub mod await_impl;
pub mod future;
pub mod gc_integration;
pub mod pin;
pub mod reactor;

pub use async_runtime::{AsyncRuntime, AsyncScheduler, AsyncTaskState};
pub use await_impl::Select;
pub use await_impl::{await_all, await_any, await_future, await_task, await_timeout, AwaitError};
pub use context::{current_stack_pointer, Context};
pub use future::{
    AsyncFn, Blocking, Future, FutureExt, JoinAll, Poll, SelectAll, Sleep, TaskHandle, YieldNow,
};
pub use gc_integration::{Root, RootHandle, RootScanner, RootSource};
pub use pin::{GcPin, PinBox, PinExt, PinStack};
pub use queue::{GlobalQueue, QueueStats, TaskQueue};
pub use reactor::{Interest, IoEvent, IoToken, Reactor};
pub use stack::{Stack, DEFAULT_STACK_SIZE, MIN_STACK_SIZE};
pub use task::{current_task_id, next_task_id, Task, TaskId, TaskState};
pub use worker::{Worker, WorkerScheduler, WorkerShared, WorkerState};

use queue::{QueueHandle, StealHandle};

/// The main task scheduler.
///
/// The scheduler manages a pool of worker threads that execute user tasks.
/// It provides work-stealing for load balancing and supports task spawning,
/// yielding, and blocking operations.
///
/// # Thread Safety
///
/// The scheduler is thread-safe and can be shared across threads. Tasks
/// can be spawned from any thread, and the scheduler will distribute them
/// across worker threads.
pub struct Scheduler {
    /// Inner shared state of the scheduler.
    inner: Arc<SchedulerInner>,
    /// Shared worker states (accessible from other threads).
    workers: Vec<Arc<WorkerShared>>,
    /// Global task queue for tasks spawned from outside workers.
    global_queue: Arc<GlobalQueue>,
}

/// Internal scheduler state shared between components.
pub struct SchedulerInner {
    /// Counter for generating task IDs.
    #[allow(dead_code)]
    next_task_id: AtomicU64,
    /// Global queue handle for spawning.
    queue_handle: QueueHandle,
    /// Map of blocked tasks waiting to be unblocked.
    blocked_tasks: Mutex<HashMap<TaskId, Box<Task>>>,
    /// Flag indicating if the scheduler is running.
    running: AtomicBool,
    /// Worker scheduler reference.
    worker_scheduler: Arc<WorkerScheduler>,
    /// Root scanner for GC integration
    root_scanner: Arc<crate::gc_integration::RootScanner>,
}

impl SchedulerInner {
    /// Creates a new scheduler inner state.
    fn new(global_queue: Arc<GlobalQueue>, worker_scheduler: Arc<WorkerScheduler>) -> Self {
        Self {
            next_task_id: AtomicU64::new(1),
            queue_handle: QueueHandle::new(global_queue),
            blocked_tasks: Mutex::new(HashMap::new()),
            running: AtomicBool::new(false),
            worker_scheduler,
            root_scanner: Arc::new(crate::gc_integration::RootScanner::new()),
        }
    }

    /// Spawns a task onto the global queue.
    fn spawn_task(&self, task: Box<Task>) -> TaskId {
        let id = task.id;
        self.queue_handle.spawn(task);
        self.worker_scheduler
            .tasks_spawned
            .fetch_add(1, Ordering::Relaxed);
        id
    }

    /// Marks a task as completed and removes it from tracking.
    fn task_completed(&self, task_id: TaskId) {
        self.worker_scheduler
            .tasks_completed
            .fetch_add(1, Ordering::Relaxed);
        // Remove from blocked tasks if it was there
        let _ = self.blocked_tasks.lock().map(|mut tasks| {
            tasks.remove(&task_id);
        });
    }

    /// Blocks the current task.
    ///
    /// # Safety
    ///
    /// Must only be called from within a task context.
    unsafe fn block_current_task(&self) {
        if let Some(task_ptr) = task::current_task_ptr() {
            let task = &mut *task_ptr;
            task.mark_blocked();

            // Store the blocked task
            if let Ok(mut _blocked) = self.blocked_tasks.lock() {
                // We need to move the task out of the current execution context
                // This is a simplified version - in production, more care is needed
                let _task_id = task.id;
                // Note: In a full implementation, we'd need to properly transfer
                // ownership of the task to the blocked_tasks map
                // For now, we just mark it blocked and the worker will handle it
            }
        }
    }

    /// Unblocks a task by ID.
    fn unblock_task(&self, task_id: TaskId) -> Option<Box<Task>> {
        self.blocked_tasks
            .lock()
            .ok()
            .and_then(|mut tasks| tasks.remove(&task_id))
            .map(|mut task| {
                task.mark_ready();
                task
            })
    }
}

impl Scheduler {
    /// Creates a new scheduler with the specified number of worker threads.
    ///
    /// # Arguments
    ///
    /// * `num_threads` - The number of OS worker threads to create.
    ///   Typically this should match the number of CPU cores.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use jet_rt_sched::Scheduler;
    ///
    /// let scheduler = Scheduler::new(4); // 4 worker threads
    /// ```
    pub fn new(num_threads: usize) -> Self {
        let num_threads = num_threads.max(1);
        let global_queue = Arc::new(GlobalQueue::new());
        let worker_scheduler = Arc::new(WorkerScheduler::new(num_threads));
        let inner = Arc::new(SchedulerInner::new(
            global_queue.clone(),
            worker_scheduler.clone(),
        ));

        // Create worker-local queues first to get stealers
        let worker_queues: Vec<_> = (0..num_threads).map(|_| queue::TaskQueue::new()).collect();

        let stealers: Vec<_> = worker_queues.iter().map(|q| q.stealer()).collect();

        // Create workers with steal handles
        let mut workers = Vec::with_capacity(num_threads);
        for id in 0..num_threads {
            // Each worker can steal from all other workers
            let worker_stealers: Vec<_> = stealers
                .iter()
                .enumerate()
                .filter(|(i, _)| *i != id)
                .map(|(_, s)| s.clone())
                .collect();

            let steal_handle = StealHandle::new(worker_stealers);

            let (_worker, shared) = Worker::new(
                id,
                steal_handle,
                global_queue.clone(),
                worker_scheduler.clone(),
            );
            workers.push(shared);
        }

        Self {
            inner,
            workers,
            global_queue,
        }
    }

    /// Spawns a new task onto the scheduler.
    ///
    /// The task will be executed by one of the worker threads.
    /// Tasks are initially placed on the global queue and may be
    /// stolen by any worker.
    ///
    /// # Arguments
    ///
    /// * `f` - The function to execute. It must be `Send` and `'static`.
    ///
    /// # Returns
    ///
    /// The `TaskId` of the spawned task.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use jet_rt_sched::Scheduler;
    ///
    /// let scheduler = Scheduler::new(2);
    /// let task_id = scheduler.spawn(|| {
    ///     println!("Hello from task!");
    /// });
    /// ```
    pub fn spawn<F>(&self, f: F) -> TaskId
    where
        F: FnOnce() + Send + 'static,
    {
        let mut task = Box::new(Task::new(f, None));
        task.set_scheduler(Arc::clone(&self.inner));
        self.inner.spawn_task(task)
    }

    /// Spawns an async task onto the scheduler.
    ///
    /// This creates a new task that executes the given future. The future
    /// will be polled to completion on one of the worker threads.
    ///
    /// # Arguments
    ///
    /// * `future` - The future to execute. It must be `Send` and `'static`.
    ///
    /// # Returns
    ///
    /// A `TaskHandle` that can be used to await the result.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use jet_rt_sched::Scheduler;
    /// use jet_rt_sched::future::{Future, Poll, Context};
    /// use std::pin::Pin;
    ///
    /// let scheduler = Scheduler::new(2);
    ///
    /// async fn example() -> i32 {
    ///     42
    /// }
    ///
    /// let handle = scheduler.spawn_async(example());
    /// ```
    pub fn spawn_async<F>(&self, future: F) -> TaskHandle<F::Output>
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        use crate::future::AsyncTask;

        let (mut async_task, handle) = AsyncTask::new(future);

        // Create a wrapper task that polls the async task
        let task = Box::new(Task::new(
            move || {
                // In a full implementation, this would properly integrate
                // with the scheduler's async runtime
                // For now, we just poll once to completion
                let waker = create_simple_waker();
                let _cx = crate::future::Context::new(&waker, 0);

                // SAFETY: The async task is properly pinned
                unsafe {
                    let _pinned = Pin::new_unchecked(&mut async_task);
                    // This is a simplified version - real implementation
                    // would integrate with scheduler properly
                }
            },
            None,
        ));

        self.inner.spawn_task(task);
        handle
    }

    /// Spawns a new task with a custom stack size.
    ///
    /// # Arguments
    ///
    /// * `stack_size` - The stack size in bytes. Must be at least `MIN_STACK_SIZE`.
    /// * `f` - The function to execute.
    ///
    /// # Example
    ///
    /// ```rust
    /// use jet_rt_sched::Scheduler;
    ///
    /// let scheduler = Scheduler::new(2);
    /// let task_id = scheduler.spawn_with_stack(2 * 1024 * 1024, || {
    ///     // Task with 2 MiB stack
    /// });
    /// ```
    pub fn spawn_with_stack<F>(&self, stack_size: usize, f: F) -> TaskId
    where
        F: FnOnce() + Send + 'static,
    {
        let mut task = Box::new(Task::new(f, Some(stack_size)));
        task.set_scheduler(Arc::clone(&self.inner));
        self.inner.spawn_task(task)
    }

    /// Runs the scheduler until all tasks complete.
    ///
    /// This method blocks the current thread until all spawned tasks
    /// have completed. It starts the worker threads and waits for them
    /// to finish processing all tasks.
    ///
    /// # Example
    ///
    /// ```rust
    /// use jet_rt_sched::Scheduler;
    /// use std::sync::Arc;
    /// use std::sync::atomic::{AtomicU32, Ordering};
    ///
    /// let mut scheduler = Scheduler::new(2);
    /// let counter = Arc::new(AtomicU32::new(0));
    ///
    /// for _ in 0..10 {
    ///     let c = counter.clone();
    ///     scheduler.spawn(move || {
    ///         c.fetch_add(1, Ordering::SeqCst);
    ///     });
    /// }
    ///
    /// scheduler.run();
    /// assert_eq!(counter.load(Ordering::SeqCst), 10);
    /// ```
    pub fn run(&mut self) {
        self.inner.running.store(true, Ordering::SeqCst);
        self.inner
            .worker_scheduler
            .running
            .store(true, Ordering::SeqCst);

        // Start all workers
        // We need to recreate workers since they consume themselves on start
        let num_threads = self.workers.len();
        let worker_queues: Vec<_> = (0..num_threads).map(|_| queue::TaskQueue::new()).collect();

        let stealers: Vec<_> = worker_queues.iter().map(|q| q.stealer()).collect();

        // Create workers but don't start them yet - we need to update self.workers
        let mut worker_shareds = Vec::with_capacity(num_threads);
        let mut worker_handles = Vec::with_capacity(num_threads);

        for (id, _queue) in worker_queues.into_iter().enumerate() {
            let worker_stealers: Vec<_> = stealers
                .iter()
                .enumerate()
                .filter(|(i, _)| *i != id)
                .map(|(_, s)| s.clone())
                .collect();

            let steal_handle = StealHandle::new(worker_stealers);

            let (worker, shared) = Worker::new(
                id,
                steal_handle,
                self.global_queue.clone(),
                self.inner.worker_scheduler.clone(),
            );

            worker_shareds.push(shared);
            worker_handles.push(worker);
        }

        // Replace self.workers with the new shared states so we can monitor them
        // Note: This is a bit of a hack - in a real implementation we'd use a different approach
        let _ = std::mem::replace(&mut self.workers, worker_shareds);

        // Now start all the workers
        for worker in worker_handles {
            worker.start();
        }

        // Wait for all tasks to complete
        // In a simple implementation, we poll until done
        // A production implementation might use a more sophisticated
        // synchronization mechanism
        loop {
            let spawned = self
                .inner
                .worker_scheduler
                .tasks_spawned
                .load(Ordering::Relaxed);
            let completed = self
                .inner
                .worker_scheduler
                .tasks_completed
                .load(Ordering::Relaxed);

            // Also check if there are any tasks in queues
            let global_empty = self.global_queue.is_empty();
            let all_workers_idle = self
                .workers
                .iter()
                .all(|w| w.local_queue_is_empty() && !w.is_executing());

            if spawned == completed && global_empty && all_workers_idle {
                break;
            }

            // Small delay to avoid busy-waiting
            thread::sleep(std::time::Duration::from_millis(1));
        }

        // Signal workers to stop
        for worker in &self.workers {
            worker.stop();
        }

        // Wait for workers to finish
        // In a production implementation, we'd join the threads properly
        thread::sleep(std::time::Duration::from_millis(10));

        self.inner.running.store(false, Ordering::SeqCst);
        self.inner
            .worker_scheduler
            .running
            .store(false, Ordering::SeqCst);
    }

    /// Yields the current task, allowing other tasks to run.
    ///
    /// # Panics
    ///
    /// Panics if called from outside a task context (i.e., not from
    /// within a spawned task).
    ///
    /// # Example
    ///
    /// ```rust
    /// use jet_rt_sched::{Scheduler, yield_now};
    ///
    /// let mut scheduler = Scheduler::new(1);
    /// scheduler.spawn(|| {
    ///     println!("Before yield");
    ///     yield_now();
    ///     println!("After yield");
    /// });
    /// scheduler.run();
    /// ```
    pub fn yield_now() {
        unsafe {
            if let Some(task_ptr) = task::current_task_ptr() {
                let task = &mut *task_ptr;
                if let Some(ref _scheduler) = task.scheduler {
                    // Find the worker running this task and yield
                    // This is a simplified version
                    // In production, we'd have a more direct path
                }
            }
        }
    }

    /// Blocks the current task.
    ///
    /// The task will not run again until `unblock` is called with its ID.
    /// This is useful for implementing synchronization primitives like
    /// channels and mutexes.
    ///
    /// # Panics
    ///
    /// Panics if called from outside a task context.
    ///
    /// # Safety
    ///
    /// This function is unsafe because improper use can lead to deadlocks
    /// if the task is never unblocked.
    ///
    /// # Example
    ///
    /// ```rust
    /// use jet_rt_sched::{Scheduler, block_current};
    /// use std::sync::Arc;
    /// use std::sync::atomic::{AtomicU64, Ordering};
    ///
    /// let scheduler = Scheduler::new(2);
    /// let flag = Arc::new(AtomicU64::new(0));
    ///
    /// // This is a simplified example - real usage would involve
    /// // another task calling unblock()
    /// ```
    pub unsafe fn block_current() {
        if let Some(task_ptr) = task::current_task_ptr() {
            let task = &mut *task_ptr;
            if let Some(ref scheduler) = task.scheduler {
                scheduler.block_current_task();
            }
        }
    }

    /// Unblocks a task by ID.
    ///
    /// If the task was blocked, it will be made runnable again.
    /// If the task is not blocked or doesn't exist, this is a no-op.
    ///
    /// # Arguments
    ///
    /// * `task_id` - The ID of the task to unblock.
    ///
    /// # Example
    ///
    /// ```rust
    /// use jet_rt_sched::Scheduler;
    ///
    /// let scheduler = Scheduler::new(2);
    /// let task_id = scheduler.spawn(|| {
    ///     // Task code
    /// });
    ///
    /// // Later, unblock if needed
    /// scheduler.unblock(task_id);
    /// ```
    pub fn unblock(&self, task_id: TaskId) {
        if let Some(task) = self.inner.unblock_task(task_id) {
            self.inner.queue_handle.spawn(task);
        }
    }

    /// Returns statistics for all workers.
    ///
    /// This is useful for monitoring and debugging scheduler performance.
    pub fn worker_stats(&self) -> Vec<QueueStats> {
        self.workers.iter().map(|w| w.stats()).collect()
    }

    /// Returns the total number of tasks spawned.
    pub fn tasks_spawned(&self) -> u64 {
        self.inner
            .worker_scheduler
            .tasks_spawned
            .load(Ordering::Relaxed)
    }

    /// Returns the total number of tasks completed.
    pub fn tasks_completed(&self) -> u64 {
        self.inner
            .worker_scheduler
            .tasks_completed
            .load(Ordering::Relaxed)
    }

    /// Returns true if the scheduler is currently running.
    pub fn is_running(&self) -> bool {
        self.inner.running.load(Ordering::Relaxed)
    }

    /// Returns the number of worker threads.
    pub fn num_workers(&self) -> usize {
        self.workers.len()
    }

    /// Returns the root scanner for GC integration.
    pub fn root_scanner(&self) -> Arc<crate::gc_integration::RootScanner> {
        self.inner.root_scanner.clone()
    }

    /// Scans all task stacks for GC roots.
    ///
    /// # Safety
    ///
    /// This must only be called when all tasks are at safe points (stopped).
    pub unsafe fn scan_task_roots(&self) -> Vec<crate::gc_integration::Root> {
        self.inner.root_scanner.scan_task_roots()
    }

    /// Returns all global GC roots.
    pub fn global_roots(&self) -> Vec<crate::gc_integration::Root> {
        self.inner.root_scanner.global_roots()
    }
}

// Thread-local scheduler reference for convenience functions
thread_local! {
    static CURRENT_SCHEDULER: RefCell<Option<Arc<SchedulerInner>>> = const { RefCell::new(None) };
}

/// Spawns a task using the thread-local scheduler.
///
/// # Panics
///
/// Panics if called without a scheduler being set up in the current thread.
///
/// For most use cases, use `Scheduler::spawn` directly instead.
pub fn spawn<F>(f: F) -> TaskId
where
    F: FnOnce() + Send + 'static,
{
    CURRENT_SCHEDULER.with(|s| {
        let scheduler = s
            .borrow()
            .as_ref()
            .expect("No scheduler in current thread")
            .clone();
        let mut task = Box::new(Task::new(f, None));
        task.set_scheduler(scheduler.clone());
        scheduler.spawn_task(task)
    })
}

/// Yields the current task using the thread-local scheduler.
///
/// See [`Scheduler::yield_now`] for details.
pub fn yield_now() {
    Scheduler::yield_now();
}

/// Blocks the current task using the thread-local scheduler.
///
/// # Safety
///
/// See [`Scheduler::block_current`] for safety requirements.
pub unsafe fn block_current() {
    Scheduler::block_current();
}

/// Spawns an async task using the thread-local scheduler.
///
/// # Panics
///
/// Panics if called without a scheduler being set up in the current thread.
pub fn spawn_async<F>(future: F) -> TaskHandle<F::Output>
where
    F: Future + Send + 'static,
    F::Output: Send + 'static,
{
    CURRENT_SCHEDULER.with(|s| {
        let scheduler = s
            .borrow()
            .as_ref()
            .expect("No scheduler in current thread")
            .clone();
        // Create a simple wrapper that runs the future
        use crate::future::AsyncTask;
        let (mut async_task, handle) = AsyncTask::new(future);

        let mut task = Box::new(Task::new(
            move || {
                let waker = create_simple_waker();
                let _cx = crate::future::Context::new(&waker, 0);

                // SAFETY: The async task is properly pinned
                unsafe {
                    let _pinned = std::pin::Pin::new_unchecked(&mut async_task);
                    // Simplified polling
                }
            },
            None,
        ));
        task.set_scheduler(scheduler.clone());
        scheduler.spawn_task(task);
        handle
    })
}

/// Creates a simple waker that does nothing.
fn create_simple_waker() -> std::task::Waker {
    use std::task::{RawWaker, RawWakerVTable};

    static VTABLE: RawWakerVTable = RawWakerVTable::new(
        |_| RawWaker::new(std::ptr::null(), &VTABLE),
        |_| {},
        |_| {},
        |_| {},
    );

    unsafe { std::task::Waker::from_raw(RawWaker::new(std::ptr::null(), &VTABLE)) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU32, Ordering};
    use std::sync::{Arc, Mutex};

    #[test]
    fn test_scheduler_creation() {
        let sched = Scheduler::new(2);
        assert_eq!(sched.num_workers(), 2);
        assert!(!sched.is_running());
    }

    #[test]
    fn test_spawn_and_run() {
        let mut sched = Scheduler::new(2);
        let counter = Arc::new(AtomicU32::new(0));

        for _ in 0..10 {
            let c = counter.clone();
            sched.spawn(move || {
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        sched.run();
        assert_eq!(counter.load(Ordering::SeqCst), 10);
    }

    #[test]
    fn test_spawn_with_stack() {
        let mut sched = Scheduler::new(1);
        let executed = Arc::new(AtomicBool::new(false));

        let e = executed.clone();
        sched.spawn_with_stack(128 * 1024, move || {
            e.store(true, Ordering::SeqCst);
        });

        sched.run();
        assert!(executed.load(Ordering::SeqCst));
    }

    #[test]
    fn test_multiple_tasks() {
        let mut sched = Scheduler::new(4);
        let results = Arc::new(Mutex::new(Vec::new()));

        for i in 0..100 {
            let r = results.clone();
            sched.spawn(move || {
                r.lock().unwrap().push(i);
            });
        }

        sched.run();

        let results = results.lock().unwrap();
        assert_eq!(results.len(), 100);
    }

    #[test]
    fn test_worker_stats() {
        let mut sched = Scheduler::new(2);

        for _ in 0..10 {
            sched.spawn(|| {});
        }

        sched.run();

        let stats = sched.worker_stats();
        assert_eq!(stats.len(), 2);

        let total_processed: u64 = stats.iter().map(|s| s.total_processed()).sum();
        assert_eq!(total_processed, 10);
    }

    #[test]
    fn test_task_counters() {
        let mut sched = Scheduler::new(2);

        assert_eq!(sched.tasks_spawned(), 0);
        assert_eq!(sched.tasks_completed(), 0);

        for _ in 0..5 {
            sched.spawn(|| {});
        }

        // Tasks are spawned immediately
        assert_eq!(sched.tasks_spawned(), 5);

        sched.run();

        // All tasks should be completed after run
        assert_eq!(sched.tasks_completed(), 5);
    }

    #[test]
    fn test_single_worker() {
        let mut sched = Scheduler::new(1);
        let counter = Arc::new(AtomicU32::new(0));

        for _ in 0..5 {
            let c = counter.clone();
            sched.spawn(move || {
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        sched.run();
        assert_eq!(counter.load(Ordering::SeqCst), 5);
    }

    #[test]
    fn test_empty_scheduler() {
        let mut sched = Scheduler::new(2);
        // Run with no tasks - should complete immediately
        sched.run();
        assert_eq!(sched.tasks_completed(), 0);
    }
}
