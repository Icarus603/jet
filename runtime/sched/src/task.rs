//! Task structure and state management.
//!
//! Tasks are the fundamental unit of execution in the Jet scheduler.
//! Each task has its own stack and context, allowing for M:N threading.

use crate::context::Context;
use crate::stack::{Stack, DEFAULT_STACK_SIZE};
use std::cell::UnsafeCell;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

/// Unique identifier for tasks.
pub type TaskId = u64;

static NEXT_TASK_ID: AtomicU64 = AtomicU64::new(1);

/// Generates a new unique task ID.
pub fn next_task_id() -> TaskId {
    NEXT_TASK_ID.fetch_add(1, Ordering::Relaxed)
}

/// The state of a task.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskState {
    /// Task is ready to run and waiting in a queue.
    Ready,
    /// Task is currently executing on a worker thread.
    Running,
    /// Task is blocked waiting for an event (I/O, channel, etc.).
    Blocked,
    /// Task has completed execution.
    Completed,
}

impl TaskState {
    /// Returns true if the task can be scheduled (is in Ready state).
    pub fn is_runnable(self) -> bool {
        matches!(self, TaskState::Ready)
    }

    /// Returns true if the task is currently running.
    pub fn is_running(self) -> bool {
        matches!(self, TaskState::Running)
    }

    /// Returns true if the task is blocked.
    pub fn is_blocked(self) -> bool {
        matches!(self, TaskState::Blocked)
    }

    /// Returns true if the task has completed.
    pub fn is_completed(self) -> bool {
        matches!(self, TaskState::Completed)
    }
}

/// The function type for task entry points.
pub type TaskFn = Box<dyn FnOnce() + Send + 'static>;

/// A task (user-space thread).
///
/// Tasks are lightweight threads of execution managed by the scheduler.
/// Each task has its own stack and register context, allowing the
/// scheduler to switch between tasks efficiently.
pub struct Task {
    /// Unique identifier for this task.
    pub id: TaskId,
    /// Current execution state.
    pub state: TaskState,
    /// Saved CPU context (registers).
    pub context: Context,
    /// Stack allocation for this task.
    pub stack: Stack,
    /// The function to execute (taken when task starts running).
    pub func: Option<TaskFn>,
    /// Next task in intrusive linked lists (ready queue, etc.).
    pub next: Option<Box<Task>>,
    /// Reference to the scheduler for yielding/blocking operations.
    pub scheduler: Option<Arc<crate::SchedulerInner>>,
}

impl Task {
    /// Creates a new task with the given function.
    ///
    /// # Arguments
    ///
    /// * `func` - The function to execute when the task runs.
    /// * `stack_size` - Optional custom stack size (uses default if None).
    pub fn new<F>(func: F, stack_size: Option<usize>) -> Self
    where
        F: FnOnce() + Send + 'static,
    {
        let stack = Stack::new(stack_size.unwrap_or(DEFAULT_STACK_SIZE));
        let mut context = Context::new();

        // Initialize context with task entry point
        // The task will start execution at task_entry_trampoline
        unsafe {
            context.init(
                stack.top_with_red_zone(),
                task_entry_trampoline as *const (),
            );
        }

        Self {
            id: next_task_id(),
            state: TaskState::Ready,
            context,
            stack,
            func: Some(Box::new(func)),
            next: None,
            scheduler: None,
        }
    }

    /// Sets the scheduler reference for this task.
    pub fn set_scheduler(&mut self, scheduler: Arc<crate::SchedulerInner>) {
        self.scheduler = Some(scheduler);
    }

    /// Marks the task as running.
    pub fn mark_running(&mut self) {
        self.state = TaskState::Running;
    }

    /// Marks the task as ready.
    pub fn mark_ready(&mut self) {
        self.state = TaskState::Ready;
    }

    /// Marks the task as blocked.
    pub fn mark_blocked(&mut self) {
        self.state = TaskState::Blocked;
    }

    /// Marks the task as completed.
    pub fn mark_completed(&mut self) {
        self.state = TaskState::Completed;
    }

    /// Returns true if this task can be run.
    pub fn is_runnable(&self) -> bool {
        self.state.is_runnable()
    }

    /// Returns the top of the stack (for context switching).
    pub fn stack_top(&self) -> *mut u8 {
        self.stack.top()
    }
}

impl std::fmt::Debug for Task {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Task")
            .field("id", &self.id)
            .field("state", &self.state)
            .field("context", &self.context)
            .field("has_func", &self.func.is_some())
            .finish()
    }
}

/// Task-local storage for the current task.
///
/// This is used by the scheduler to track which task is currently
/// executing on each OS thread.
pub struct TaskLocal {
    inner: UnsafeCell<Option<*mut Task>>,
}

impl TaskLocal {
    /// Creates a new empty task-local storage.
    pub const fn new() -> Self {
        Self {
            inner: UnsafeCell::new(None),
        }
    }

    /// Gets the current task pointer.
    ///
    /// # Safety
    ///
    /// Must only be called from the thread that owns this TaskLocal.
    pub unsafe fn get(&self) -> Option<*mut Task> {
        *self.inner.get()
    }

    /// Sets the current task pointer.
    ///
    /// # Safety
    ///
    /// Must only be called from the thread that owns this TaskLocal.
    /// The pointer must remain valid for the duration of the task execution.
    pub unsafe fn set(&self, task: Option<*mut Task>) {
        *self.inner.get() = task;
    }

    /// Returns true if a task is currently set.
    ///
    /// # Safety
    ///
    /// Must only be called from the thread that owns this TaskLocal.
    #[allow(dead_code)]
    pub unsafe fn is_set(&self) -> bool {
        (*self.inner.get()).is_some()
    }
}

// TaskLocal is thread-local, so Send and Sync are safe
unsafe impl Send for TaskLocal {}
unsafe impl Sync for TaskLocal {}

/// Trampoline function called when a task starts execution.
///
/// This function is called by the context switch code when a new task
/// starts. It retrieves the task function and executes it.
extern "C" fn task_entry_trampoline() {
    // Get the current task from thread-local storage
    let task_ptr = CURRENT_TASK.with(|t| unsafe { t.get() });
    let task = unsafe { &mut *task_ptr.expect("No current task in trampoline") };

    // Take the function and execute it
    let func = task.func.take().expect("Task function already taken");
    func();

    // Mark task as completed
    task.mark_completed();

    // Yield back to scheduler - the scheduler will clean up
    if let Some(ref scheduler) = task.scheduler {
        scheduler.task_completed(task.id);
    }

    // This point should never be reached as task_completed switches away
    unreachable!("Task should not return after completion");
}

thread_local! {
    pub static CURRENT_TASK: TaskLocal = const { TaskLocal::new() };
}

/// Gets the current task ID if a task is running.
pub fn current_task_id() -> Option<TaskId> {
    CURRENT_TASK.with(|t| unsafe { t.get().map(|ptr| (*ptr).id) })
}

/// Gets a raw pointer to the current task.
///
/// # Safety
///
/// The returned pointer is only valid until the next context switch.
pub unsafe fn current_task_ptr() -> Option<*mut Task> {
    CURRENT_TASK.with(|t| t.get())
}

/// Sets the current task pointer.
///
/// # Safety
///
/// The pointer must be valid for the duration of the task execution.
pub unsafe fn set_current_task(task: Option<*mut Task>) {
    CURRENT_TASK.with(|t| t.set(task));
}

#[cfg(test)]
mod tests {
    use super::*;
    // use std::sync::atomic::{AtomicU32, Ordering};

    #[test]
    fn test_task_id_generation() {
        let id1 = next_task_id();
        let id2 = next_task_id();
        assert_ne!(id1, id2);
        assert!(id2 > id1);
    }

    #[test]
    fn test_task_state() {
        assert!(TaskState::Ready.is_runnable());
        assert!(!TaskState::Running.is_runnable());
        assert!(!TaskState::Blocked.is_runnable());
        assert!(!TaskState::Completed.is_runnable());

        assert!(TaskState::Running.is_running());
        assert!(TaskState::Blocked.is_blocked());
        assert!(TaskState::Completed.is_completed());
    }

    #[test]
    fn test_task_creation() {
        let task = Task::new(|| {}, None);
        assert_eq!(task.state, TaskState::Ready);
        assert!(task.func.is_some());
    }

    #[test]
    fn test_task_state_transitions() {
        let mut task = Task::new(|| {}, None);

        assert!(task.is_runnable());

        task.mark_running();
        assert!(task.state.is_running());

        task.mark_blocked();
        assert!(task.state.is_blocked());

        task.mark_ready();
        assert!(task.is_runnable());

        task.mark_completed();
        assert!(task.state.is_completed());
    }

    #[test]
    fn test_task_local() {
        let local = TaskLocal::new();

        unsafe {
            assert!(!local.is_set());
            assert!(local.get().is_none());

            let mut task = Task::new(|| {}, None);
            local.set(Some(&mut task));

            assert!(local.is_set());
            assert!(local.get().is_some());

            local.set(None);
            assert!(!local.is_set());
        }
    }
}
