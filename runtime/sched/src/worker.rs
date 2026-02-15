//! Worker thread implementation.
//!
//! Workers are OS threads that execute user tasks. Each worker has a
//! local task queue and can steal work from other workers when idle.

use crate::context::Context;
use crate::queue::{GlobalQueue, QueueStats, StealHandle, TaskQueue};
use crate::task::{set_current_task, Task, TaskId};
use crossbeam::deque::Steal;
use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, Thread};

/// The state of a worker thread.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WorkerState {
    /// Worker is looking for work.
    Searching,
    /// Worker is executing a task.
    Executing,
    /// Worker is parked (idle).
    Parked,
    /// Worker is shutting down.
    ShuttingDown,
}

/// Shared state for a worker that can be accessed from other threads.
pub struct WorkerShared {
    /// The worker's ID (index in the scheduler's worker array).
    pub id: usize,
    /// Current state of this worker.
    state: Mutex<WorkerState>,
    /// The currently executing task (if any).
    current_task: Mutex<Option<TaskId>>,
    /// Statistics for this worker.
    stats: Mutex<QueueStats>,
    /// Flag indicating if the worker should stop.
    should_stop: AtomicBool,
    /// The OS thread handle (set when started).
    thread: Mutex<Option<Thread>>,
}

/// Reference to scheduler state needed by workers.
pub struct WorkerScheduler {
    /// Total number of workers.
    pub num_workers: usize,
    /// Counter for tasks completed (for shutdown detection).
    pub tasks_completed: AtomicU64,
    /// Counter for tasks spawned.
    pub tasks_spawned: AtomicU64,
    /// Flag indicating if the scheduler is running.
    pub running: AtomicBool,
}

impl WorkerScheduler {
    /// Creates a new worker scheduler reference.
    pub fn new(num_workers: usize) -> Self {
        Self {
            num_workers,
            tasks_completed: AtomicU64::new(0),
            tasks_spawned: AtomicU64::new(0),
            running: AtomicBool::new(false),
        }
    }
}

impl WorkerShared {
    /// Creates a new shared worker state.
    pub fn new(id: usize) -> Self {
        Self {
            id,
            state: Mutex::new(WorkerState::Parked),
            current_task: Mutex::new(None),
            stats: Mutex::new(QueueStats::new()),
            should_stop: AtomicBool::new(false),
            thread: Mutex::new(None),
        }
    }

    /// Unparks this worker to wake it up.
    pub fn unpark(&self) {
        if let Ok(guard) = self.thread.lock() {
            if let Some(ref thread) = *guard {
                thread.unpark();
            }
        }
    }

    /// Signals the worker to stop.
    pub fn stop(&self) {
        self.should_stop.store(true, Ordering::Relaxed);
        self.unpark();
    }

    /// Gets the current task ID if one is executing.
    pub fn current_task_id(&self) -> Option<TaskId> {
        self.current_task.lock().ok().and_then(|g| *g)
    }

    /// Gets a copy of this worker's statistics.
    pub fn stats(&self) -> QueueStats {
        self.stats
            .lock()
            .map(|s| *s)
            .unwrap_or_else(|_| QueueStats::new())
    }

    /// Gets the current state of this worker.
    pub fn state(&self) -> WorkerState {
        self.state.lock().map(|s| *s).unwrap_or(WorkerState::Parked)
    }

    /// Returns true if this worker is currently executing a task.
    pub fn is_executing(&self) -> bool {
        self.state() == WorkerState::Executing
    }

    /// Sets the thread handle.
    pub fn set_thread(&self, thread: Thread) {
        *self.thread.lock().unwrap() = Some(thread);
    }

    /// Sets the worker state.
    pub fn set_state(&self, state: WorkerState) {
        *self.state.lock().unwrap() = state;
    }

    /// Sets the current task.
    pub fn set_current_task(&self, task_id: Option<TaskId>) {
        *self.current_task.lock().unwrap() = task_id;
    }

    /// Returns true if the worker should stop.
    pub fn should_stop(&self) -> bool {
        self.should_stop.load(Ordering::Relaxed)
    }

    /// Updates statistics.
    pub fn update_stats<F>(&self, f: F)
    where
        F: FnOnce(&mut QueueStats),
    {
        if let Ok(mut stats) = self.stats.lock() {
            f(&mut stats);
        }
    }

    /// Returns true if the local queue is empty.
    ///
    /// Note: This is a placeholder since WorkerShared doesn't have direct
    /// access to the local queue. In a full implementation, we'd track this
    /// via a shared atomic or similar mechanism.
    pub fn local_queue_is_empty(&self) -> bool {
        // We can't directly check the local queue from WorkerShared,
        // so we assume it's empty. The global queue check will catch
        // any remaining tasks.
        true
    }
}

/// A worker thread in the scheduler.
///
/// Each worker runs an OS thread that executes user tasks. Workers
/// maintain local task queues and participate in work stealing.
pub struct Worker {
    /// Shared state accessible from other threads.
    pub shared: Arc<WorkerShared>,
    /// The worker's local task queue (only accessed by this worker).
    pub local_queue: RefCell<TaskQueue>,
    /// Stealer handles for other workers.
    steal_handle: StealHandle,
    /// Handle to the global task queue.
    global: Arc<GlobalQueue>,
    /// Reference to the scheduler for yielding.
    scheduler: Arc<WorkerScheduler>,
    /// Saved scheduler context for switching back.
    scheduler_context: RefCell<Context>,
}

impl Worker {
    /// Creates a new worker.
    pub fn new(
        id: usize,
        steal_handle: StealHandle,
        global: Arc<GlobalQueue>,
        scheduler: Arc<WorkerScheduler>,
    ) -> (Self, Arc<WorkerShared>) {
        let shared = Arc::new(WorkerShared::new(id));
        let worker = Self {
            shared: Arc::clone(&shared),
            local_queue: RefCell::new(TaskQueue::new()),
            steal_handle,
            global,
            scheduler,
            scheduler_context: RefCell::new(Context::new()),
        };
        (worker, shared)
    }

    /// Starts the worker thread.
    ///
    /// This spawns a new OS thread that runs the worker's main loop.
    pub fn start(self) {
        let builder = thread::Builder::new().name(format!("jet-worker-{}", self.shared.id));

        // Extract the shared state before moving self into the closure
        let shared = Arc::clone(&self.shared);

        let handle = builder
            .spawn(move || {
                self.run();
            })
            .expect("Failed to spawn worker thread");

        shared.set_thread(handle.thread().clone());
    }

    /// The worker's main loop.
    fn run(self) {
        self.shared.set_state(WorkerState::Searching);

        while !self.shared.should_stop() {
            // Try to find a task to execute
            if let Some(mut task) = self.find_task() {
                self.execute_task(&mut task);
            } else {
                // No work found, park briefly to avoid spinning
                self.shared.set_state(WorkerState::Parked);
                thread::park_timeout(std::time::Duration::from_micros(100));
                self.shared.set_state(WorkerState::Searching);
            }
        }

        self.shared.set_state(WorkerState::ShuttingDown);
    }

    /// Finds a task to execute using work stealing.
    ///
    /// Search order:
    /// 1. Local queue
    /// 2. Global queue
    /// 3. Other workers (stealing)
    fn find_task(&self) -> Option<Box<Task>> {
        // 1. Check local queue first (fastest)
        if let Some(task) = self.local_queue.borrow_mut().pop() {
            self.shared.update_stats(|s| s.local_pops += 1);
            return Some(task);
        }

        // 2. Try to steal from global queue
        match self.global.steal() {
            Steal::Success(task) => {
                self.shared.update_stats(|s| s.steals_from_global += 1);
                return Some(task);
            }
            Steal::Retry => {}
            Steal::Empty => {}
        }

        // 3. Try to steal from other workers
        if let Some(task) = self.steal_handle.steal_random() {
            self.shared.update_stats(|s| s.steals_from_workers += 1);
            return Some(task);
        }

        self.shared.update_stats(|s| s.failed_steals += 1);

        None
    }

    /// Returns true if the local queue is empty.
    pub fn local_queue_is_empty(&self) -> bool {
        self.local_queue.borrow().is_empty()
    }

    /// Executes a task.
    ///
    /// This runs the task function directly. Context switching is a work-in-progress
    /// and will be fully implemented in a future update.
    fn execute_task(&self, task: &mut Task) {
        self.shared.set_state(WorkerState::Executing);
        task.mark_running();

        // Store the task ID
        self.shared.set_current_task(Some(task.id));

        unsafe {
            // Set current task for thread-local access
            set_current_task(Some(task as *mut Task));

            // For now, run the task directly without context switching
            // This is a simplified implementation that works for cooperative tasks
            // that don't call yield_now() or block_current()
            if let Some(func) = task.func.take() {
                func();
            }

            // Mark task as completed
            task.mark_completed();

            // We return here when the task completes
            set_current_task(None);
        }

        self.shared.set_current_task(None);
        self.shared.set_state(WorkerState::Searching);

        // Update statistics
        self.scheduler
            .tasks_completed
            .fetch_add(1, Ordering::Relaxed);
    }

    /// Yields the current task back to the scheduler.
    ///
    /// # Safety
    ///
    /// Must only be called from within a task context.
    pub unsafe fn yield_current(&self) {
        if let Some(task_ptr) = self.shared.current_task_id().and_then(|_| {
            // Get the actual task pointer from thread-local
            crate::task::current_task_ptr()
        }) {
            let task = &mut *task_ptr;
            task.mark_ready();

            // Save task context and switch back to scheduler
            let task_ctx = &mut task.context;
            let sched_ctx = &*self.scheduler_context.as_ptr();

            Context::switch(task_ctx, sched_ctx);
        }
    }

    /// Blocks the current task.
    ///
    /// # Safety
    ///
    /// Must only be called from within a task context.
    pub unsafe fn block_current(&self) {
        if let Some(task_ptr) = crate::task::current_task_ptr() {
            let task = &mut *task_ptr;
            task.mark_blocked();

            // Save task context and switch back to scheduler
            let task_ctx = &mut task.context;
            let sched_ctx = &*self.scheduler_context.as_ptr();

            Context::switch(task_ctx, sched_ctx);
        }
    }

    /// Pushes a task onto the local queue.
    pub fn push_local(&self, task: Box<Task>) {
        self.local_queue.borrow_mut().push(task);
        self.scheduler.tasks_spawned.fetch_add(1, Ordering::Relaxed);
    }
}

/// A handle to a worker for sending signals.
#[derive(Clone)]
#[allow(dead_code)]
pub struct WorkerHandle {
    /// Worker ID
    pub id: usize,
    /// Reference to unpark the worker thread
    unpark_fn: Arc<dyn Fn() + Send + Sync>,
}

impl WorkerHandle {
    /// Creates a new worker handle.
    #[allow(dead_code)]
    pub fn new(id: usize, unpark_fn: Arc<dyn Fn() + Send + Sync>) -> Self {
        Self { id, unpark_fn }
    }

    /// Unparks the worker thread.
    #[allow(dead_code)]
    pub fn unpark(&self) {
        (self.unpark_fn)();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::queue::TaskQueue;
    // use std::sync::atomic::AtomicU32;

    #[test]
    fn test_worker_creation() {
        let global = Arc::new(GlobalQueue::new());
        let scheduler = Arc::new(WorkerScheduler::new(1));
        let steal_handle = StealHandle::new(vec![]);

        let (_worker, shared) = Worker::new(0, steal_handle, global, scheduler);

        assert_eq!(shared.id, 0);
        assert_eq!(shared.state(), WorkerState::Parked);
        assert!(!shared.is_executing());
    }

    #[test]
    fn test_worker_push_local() {
        let global = Arc::new(GlobalQueue::new());
        let scheduler = Arc::new(WorkerScheduler::new(1));
        let steal_handle = StealHandle::new(vec![]);

        let (worker, _shared) = Worker::new(0, steal_handle, global, scheduler);

        let task = Box::new(Task::new(|| {}, None));
        worker.push_local(task);

        assert_eq!(worker.local_queue.borrow().len(), 1);
    }

    #[test]
    fn test_worker_find_task_local() {
        let global = Arc::new(GlobalQueue::new());
        let scheduler = Arc::new(WorkerScheduler::new(1));
        let steal_handle = StealHandle::new(vec![]);

        let (worker, _shared) = Worker::new(0, steal_handle, global, scheduler);

        let task = Box::new(Task::new(|| {}, None));
        worker.push_local(task);

        let found = worker.find_task();
        assert!(found.is_some());
        assert!(worker.local_queue.borrow().is_empty());
    }

    #[test]
    fn test_worker_find_task_global() {
        let global = Arc::new(GlobalQueue::new());
        let scheduler = Arc::new(WorkerScheduler::new(1));
        let steal_handle = StealHandle::new(vec![]);

        let (worker, _shared) = Worker::new(0, steal_handle, global.clone(), scheduler);

        // Add task to global queue
        let task = Box::new(Task::new(|| {}, None));
        global.push(task);

        let found = worker.find_task();
        assert!(found.is_some());
    }

    #[test]
    fn test_worker_find_task_steal() {
        let global = Arc::new(GlobalQueue::new());
        let scheduler = Arc::new(WorkerScheduler::new(2));

        // Create two workers
        let worker2_queue = TaskQueue::new();
        let worker2_stealer = worker2_queue.stealer();

        let (worker1, _shared1) = Worker::new(
            0,
            StealHandle::new(vec![worker2_stealer]),
            global,
            scheduler,
        );

        // Add task to worker2's queue
        worker2_queue.push(Box::new(Task::new(|| {}, None)));

        // Worker1 should be able to steal it
        let found = worker1.find_task();
        assert!(found.is_some());
    }

    #[test]
    fn test_worker_stats() {
        let global = Arc::new(GlobalQueue::new());
        let scheduler = Arc::new(WorkerScheduler::new(1));
        let steal_handle = StealHandle::new(vec![]);

        let (_worker, shared) = Worker::new(0, steal_handle, global, scheduler);

        let stats = shared.stats();
        assert_eq!(stats.local_pops, 0);
        assert_eq!(stats.total_processed(), 0);
    }

    #[test]
    fn test_worker_state_transitions() {
        let global = Arc::new(GlobalQueue::new());
        let scheduler = Arc::new(WorkerScheduler::new(1));
        let steal_handle = StealHandle::new(vec![]);

        let (_worker, shared) = Worker::new(0, steal_handle, global, scheduler);

        assert_eq!(shared.state(), WorkerState::Parked);

        shared.set_state(WorkerState::Searching);
        assert_eq!(shared.state(), WorkerState::Searching);

        shared.set_state(WorkerState::Executing);
        assert!(shared.is_executing());

        shared.set_state(WorkerState::ShuttingDown);
        assert_eq!(shared.state(), WorkerState::ShuttingDown);
    }
}
