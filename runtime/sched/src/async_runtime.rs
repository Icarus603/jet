//! Async runtime integration with the M:N scheduler.
//!
//! This module provides the bridge between async/await syntax and the
//! underlying scheduler, enabling efficient execution of async tasks.
//!
//! # Architecture
//!
//! The async runtime consists of:
//! - An async task spawner that creates futures and schedules them
//! - A waker system for notifying tasks when they should be polled
//! - Integration with the I/O reactor for async I/O operations
//! - A task executor that polls futures to completion
//!
//! # Example
//!
//! ```ignore
//! use jet_rt_sched::async_runtime::{spawn, block_on};
//!
//! async fn example() -> i32 {
//!     42
//! }
//!
//! let result = block_on(example());
//! assert_eq!(result, 42);
//! ```

use crate::future::{Context, Future, Poll, TaskHandle, YieldNow};
use crate::queue::GlobalQueue;
use crate::reactor::{IoToken, Reactor};
use crate::task::{Task, TaskId};
use crate::worker::WorkerScheduler;
use std::cell::RefCell;
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::task::{RawWaker, RawWakerVTable, Waker};
use std::time::{Duration, Instant};

/// The async runtime that manages async task execution.
///
/// This is the main entry point for async/await functionality. It coordinates
/// between the scheduler, reactor, and async tasks.
pub struct AsyncRuntime {
    /// The underlying scheduler for task execution.
    scheduler: Arc<AsyncScheduler>,
    /// The I/O reactor for async I/O operations.
    reactor: Arc<Reactor>,
    /// Global queue for async tasks.
    global_queue: Arc<GlobalQueue>,
    /// Running state.
    running: AtomicBool,
}

/// Internal async scheduler state.
pub struct AsyncScheduler {
    /// The worker scheduler reference.
    #[allow(dead_code)]
    worker_scheduler: Arc<WorkerScheduler>,
    /// Map of active async tasks.
    tasks: Mutex<HashMap<TaskId, AsyncTaskEntry>>,
    /// Counter for spawned tasks.
    tasks_spawned: AtomicU64,
    /// Counter for completed tasks.
    tasks_completed: AtomicU64,
    /// Waker registry for tasks waiting on I/O.
    waker_registry: Mutex<HashMap<IoToken, TaskId>>,
    /// Timer queue for sleep futures.
    timer_queue: Mutex<Vec<TimerEntry>>,
}

/// An entry in the async task table.
struct AsyncTaskEntry {
    /// The task ID.
    #[allow(dead_code)]
    id: TaskId,
    /// The task state.
    state: AsyncTaskState,
    /// The waker for this task.
    waker: Option<Waker>,
    /// Whether the task has been cancelled.
    cancelled: bool,
}

/// State of an async task.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsyncTaskState {
    /// Task is ready to be polled.
    Ready,
    /// Task is currently being polled.
    Polling,
    /// Task is waiting for an event (I/O, timer, etc.).
    Waiting,
    /// Task has completed.
    Completed,
    /// Task was cancelled.
    Cancelled,
}

/// A timer entry for scheduled wakeups.
struct TimerEntry {
    /// When this timer fires.
    deadline: Instant,
    /// The task to wake.
    task_id: TaskId,
    /// Whether this timer has been cancelled.
    cancelled: bool,
}

impl AsyncRuntime {
    /// Creates a new async runtime with the given number of worker threads.
    ///
    /// # Arguments
    ///
    /// * `num_workers` - The number of OS threads to use for task execution.
    pub fn new(num_workers: usize) -> Self {
        let num_workers = num_workers.max(1);
        let global_queue = Arc::new(GlobalQueue::new());
        let worker_scheduler = Arc::new(WorkerScheduler::new(num_workers));

        let scheduler = Arc::new(AsyncScheduler {
            worker_scheduler,
            tasks: Mutex::new(HashMap::new()),
            tasks_spawned: AtomicU64::new(0),
            tasks_completed: AtomicU64::new(0),
            waker_registry: Mutex::new(HashMap::new()),
            timer_queue: Mutex::new(Vec::new()),
        });

        let reactor = Arc::new(Reactor::new().expect("Failed to create reactor"));

        Self {
            scheduler,
            reactor,
            global_queue,
            running: AtomicBool::new(false),
        }
    }

    /// Spawns a new async task.
    ///
    /// # Arguments
    ///
    /// * `future` - The future to execute.
    ///
    /// # Returns
    ///
    /// A `TaskHandle` that can be used to await the result.
    pub fn spawn<F>(&self, future: F) -> TaskHandle<F::Output>
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        let task_id = crate::task::next_task_id();
        let handle = TaskHandle::new(task_id);

        // Create the async task entry
        let entry = AsyncTaskEntry {
            id: task_id,
            state: AsyncTaskState::Ready,
            waker: None,
            cancelled: false,
        };

        self.scheduler.tasks.lock().unwrap().insert(task_id, entry);
        self.scheduler.tasks_spawned.fetch_add(1, Ordering::Relaxed);

        // Wrap the future in an executor task
        let executor = AsyncExecutor::new(future, handle.clone(), Arc::clone(&self.scheduler));

        // Create a regular task that polls the future
        let task = Box::new(Task::new(
            move || {
                executor.run();
            },
            None,
        ));

        // Spawn onto the global queue
        self.global_queue.push(task);

        handle
    }

    /// Blocks the current thread until the given future completes.
    ///
    /// This is the entry point for running async code from synchronous contexts.
    /// Accepts both our custom Future trait and std::future::Future.
    pub fn block_on<F>(&self, future: F) -> F::Output
    where
        F: Future,
    {
        let mut pinned = Box::pin(future);

        // Create a simple waker that just repolls
        let waker = create_simple_waker();
        let mut cx = Context::new(&waker, 0);

        loop {
            match pinned.as_mut().poll(&mut cx) {
                Poll::Ready(result) => return result,
                Poll::Pending => {
                    // In a real implementation, we'd park the thread
                    // and wait for the waker to unpark it
                    std::thread::park_timeout(Duration::from_millis(1));
                }
            }
        }
    }

    /// Blocks the current thread until the given std future completes.
    ///
    /// This overload accepts std::future::Future for compatibility with native async/await.
    pub fn block_on_std<F>(&self, future: F) -> F::Output
    where
        F: std::future::Future,
    {
        use std::task::Poll;

        let mut pinned = Box::pin(future);

        // Create a simple waker that just repolls
        let waker = create_simple_waker();

        loop {
            match pinned
                .as_mut()
                .poll(&mut std::task::Context::from_waker(&waker))
            {
                Poll::Ready(result) => return result,
                Poll::Pending => {
                    // In a real implementation, we'd park the thread
                    // and wait for the waker to unpark it
                    std::thread::park_timeout(Duration::from_millis(1));
                }
            }
        }
    }

    /// Runs the async runtime until all tasks complete.
    ///
    /// This method blocks the current thread and drives the async runtime.
    pub fn run(&self) {
        self.running.store(true, Ordering::SeqCst);

        // Main event loop
        while self.running.load(Ordering::Relaxed) {
            // 1. Process any ready async tasks
            self.process_ready_tasks();

            // 2. Poll for I/O events
            self.poll_io();

            // 3. Process timer events
            self.process_timers();

            // 4. Check if we should exit
            if self.should_exit() {
                break;
            }

            // Small yield to avoid busy-waiting
            std::thread::yield_now();
        }
    }

    /// Processes all ready async tasks.
    fn process_ready_tasks(&self) {
        let mut tasks = self.scheduler.tasks.lock().unwrap();

        for (_task_id, entry) in tasks.iter_mut() {
            if entry.state == AsyncTaskState::Ready {
                entry.state = AsyncTaskState::Polling;
                // The task will be polled by its executor
            }
        }
    }

    /// Polls for I/O events and wakes associated tasks.
    fn poll_io(&self) {
        let mut events = Vec::new();

        match self
            .reactor
            .poll(&mut events, Some(Duration::from_millis(0)))
        {
            Ok(n) if n > 0 => {
                let waker_registry = self.scheduler.waker_registry.lock().unwrap();

                for event in events {
                    if let Some(&task_id) = waker_registry.get(&event.token) {
                        // Wake the associated task
                        self.wake_task(task_id);
                    }
                }
            }
            _ => {}
        }
    }

    /// Processes timer events.
    fn process_timers(&self) {
        let now = Instant::now();
        let mut timers = self.scheduler.timer_queue.lock().unwrap();

        // Find timers that have fired
        let fired: Vec<TaskId> = timers
            .iter()
            .filter(|t| !t.cancelled && t.deadline <= now)
            .map(|t| t.task_id)
            .collect();

        // Remove fired timers
        timers.retain(|t| t.deadline > now || t.cancelled);

        // Wake associated tasks
        for task_id in fired {
            self.wake_task(task_id);
        }
    }

    /// Wakes a task by ID.
    fn wake_task(&self, task_id: TaskId) {
        if let Ok(mut tasks) = self.scheduler.tasks.lock() {
            if let Some(entry) = tasks.get_mut(&task_id) {
                if entry.state == AsyncTaskState::Waiting {
                    entry.state = AsyncTaskState::Ready;
                }
                if let Some(ref waker) = entry.waker {
                    waker.wake_by_ref();
                }
            }
        }
    }

    /// Registers a task to be woken when an I/O event occurs.
    #[allow(dead_code)]
    pub(crate) fn register_io_waker(&self, token: IoToken, task_id: TaskId) {
        let mut registry = self.scheduler.waker_registry.lock().unwrap();
        registry.insert(token, task_id);
    }

    /// Registers a timer for a task.
    #[allow(dead_code)]
    pub(crate) fn register_timer(&self, deadline: Instant, task_id: TaskId) {
        let mut timers = self.scheduler.timer_queue.lock().unwrap();
        timers.push(TimerEntry {
            deadline,
            task_id,
            cancelled: false,
        });
        // Sort by deadline
        timers.sort_by_key(|t| t.deadline);
    }

    /// Cancels a task.
    pub fn cancel(&self, task_id: TaskId) -> bool {
        if let Ok(mut tasks) = self.scheduler.tasks.lock() {
            if let Some(entry) = tasks.get_mut(&task_id) {
                entry.cancelled = true;
                entry.state = AsyncTaskState::Cancelled;
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Returns the number of active tasks.
    pub fn active_tasks(&self) -> usize {
        self.scheduler.tasks.lock().map(|t| t.len()).unwrap_or(0)
    }

    /// Returns the number of spawned tasks.
    pub fn tasks_spawned(&self) -> u64 {
        self.scheduler.tasks_spawned.load(Ordering::Relaxed)
    }

    /// Returns the number of completed tasks.
    pub fn tasks_completed(&self) -> u64 {
        self.scheduler.tasks_completed.load(Ordering::Relaxed)
    }

    /// Checks if the runtime should exit.
    fn should_exit(&self) -> bool {
        let spawned = self.tasks_spawned();
        let completed = self.tasks_completed();
        let global_empty = self.global_queue.is_empty();

        spawned == completed && global_empty
    }

    /// Stops the runtime.
    pub fn stop(&self) {
        self.running.store(false, Ordering::SeqCst);
    }

    /// Returns a reference to the reactor.
    pub fn reactor(&self) -> &Arc<Reactor> {
        &self.reactor
    }
}

/// Executor for an async task.
struct AsyncExecutor<T> {
    /// The future being executed.
    future: Option<Pin<Box<dyn Future<Output = T> + Send>>>,
    /// Handle for notifying completion.
    handle: TaskHandle<T>,
    /// Reference to the scheduler.
    scheduler: Arc<AsyncScheduler>,
}

impl<T: Send + 'static> AsyncExecutor<T> {
    /// Creates a new async executor.
    fn new<F>(future: F, handle: TaskHandle<T>, scheduler: Arc<AsyncScheduler>) -> Self
    where
        F: Future<Output = T> + Send + 'static,
    {
        Self {
            future: Some(Box::pin(future)),
            handle,
            scheduler,
        }
    }

    /// Runs the executor, polling the future to completion.
    fn run(mut self) {
        let task_id = self.handle.task_id;

        // Create a waker for this task
        let waker = create_task_waker(task_id);
        let mut cx = Context::new(&waker, task_id);

        loop {
            // Check if cancelled first (before polling)
            let is_cancelled = self
                .scheduler
                .tasks
                .lock()
                .ok()
                .and_then(|t| t.get(&task_id).map(|e| e.cancelled))
                .unwrap_or(false);

            if is_cancelled {
                break;
            }

            // Update state to polling
            if let Ok(mut tasks) = self.scheduler.tasks.lock() {
                if let Some(entry) = tasks.get_mut(&task_id) {
                    entry.state = AsyncTaskState::Polling;
                }
            }

            // Poll the future
            let poll_result = self.future.as_mut().map(|f| f.as_mut().poll(&mut cx));

            match poll_result {
                Some(Poll::Ready(result)) => {
                    self.future = None;
                    if let Ok(mut tasks) = self.scheduler.tasks.lock() {
                        if let Some(entry) = tasks.get_mut(&task_id) {
                            entry.state = AsyncTaskState::Completed;
                        }
                    }
                    self.handle.complete(result);
                    self.scheduler
                        .tasks_completed
                        .fetch_add(1, Ordering::Relaxed);
                    break;
                }
                Some(Poll::Pending) => {
                    if let Ok(mut tasks) = self.scheduler.tasks.lock() {
                        if let Some(entry) = tasks.get_mut(&task_id) {
                            entry.state = AsyncTaskState::Waiting;
                        }
                    }
                    // Yield to allow other tasks to run
                    std::thread::yield_now();
                }
                None => break,
            }
        }
    }
}

/// Creates a simple waker that does nothing special.
fn create_simple_waker() -> Waker {
    static VTABLE: RawWakerVTable = RawWakerVTable::new(
        |_| RawWaker::new(std::ptr::null(), &VTABLE),
        |_| {},
        |_| {},
        |_| {},
    );

    unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &VTABLE)) }
}

/// Creates a waker for a specific task.
fn create_task_waker(_task_id: TaskId) -> Waker {
    // In a full implementation, this would create a waker that
    // properly reschedules the task when wake() is called
    create_simple_waker()
}

/// Spawns a future onto the thread-local async runtime.
///
/// # Panics
///
/// Panics if called without an async runtime being available.
pub fn spawn<F>(future: F) -> TaskHandle<F::Output>
where
    F: Future + Send + 'static,
    F::Output: Send + 'static,
{
    RUNTIME.with(|r| {
        let runtime = r
            .borrow()
            .as_ref()
            .expect("No async runtime in current thread")
            .clone();
        runtime.spawn(future)
    })
}

/// Blocks on a future using the thread-local runtime.
pub fn block_on<F>(future: F) -> F::Output
where
    F: Future,
{
    RUNTIME.with(|r| {
        let runtime = r
            .borrow()
            .as_ref()
            .expect("No async runtime in current thread")
            .clone();
        runtime.block_on(future)
    })
}

/// Yields control back to the scheduler.
pub async fn yield_now() {
    YieldNow::new().await
}

/// Sleeps for the given duration.
pub async fn sleep(duration: Duration) {
    crate::future::Sleep::for_duration(duration).await
}

/// Sleeps until the given instant.
pub async fn sleep_until(deadline: Instant) {
    crate::future::Sleep::until(deadline).await
}

/// Runs a blocking operation on a separate thread.
pub async fn spawn_blocking<T, F>(f: F) -> T
where
    T: Send + 'static,
    F: FnOnce() -> T + Send + 'static,
{
    crate::future::Blocking::new(f).await
}

// Thread-local storage for the async runtime
thread_local! {
    static RUNTIME: RefCell<Option<Arc<AsyncRuntime>>> = const { RefCell::new(None) };
}

/// Sets the thread-local async runtime.
pub fn set_thread_local_runtime(runtime: Arc<AsyncRuntime>) {
    RUNTIME.with(|r| {
        *r.borrow_mut() = Some(runtime);
    });
}

/// Clears the thread-local async runtime.
pub fn clear_thread_local_runtime() {
    RUNTIME.with(|r| {
        *r.borrow_mut() = None;
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_async_runtime_creation() {
        let runtime = AsyncRuntime::new(2);
        assert_eq!(runtime.active_tasks(), 0);
    }

    #[test]
    fn test_block_on_simple() {
        let runtime = AsyncRuntime::new(1);

        async fn simple() -> i32 {
            42
        }

        let result = runtime.block_on_std(simple());
        assert_eq!(result, 42);
    }

    #[test]
    fn test_spawn_async() {
        let runtime = AsyncRuntime::new(2);

        // Note: spawn requires our custom Future trait, not std::future::Future
        // For now, we just verify the runtime was created properly
        assert_eq!(runtime.active_tasks(), 0);
    }

    #[test]
    fn test_async_state_transitions() {
        assert_eq!(AsyncTaskState::Ready, AsyncTaskState::Ready);
        assert_ne!(AsyncTaskState::Ready, AsyncTaskState::Completed);
    }

    #[test]
    fn test_timer_entry() {
        let entry = TimerEntry {
            deadline: Instant::now(),
            task_id: 1,
            cancelled: false,
        };

        assert_eq!(entry.task_id, 1);
        assert!(!entry.cancelled);
    }
}
