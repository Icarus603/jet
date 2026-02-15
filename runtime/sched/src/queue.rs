//! Task queue implementations for work stealing.
//!
//! Provides the queue structures used for distributing tasks across
//! worker threads using a work-stealing algorithm.

use crate::task::Task;
use crossbeam::deque::{Injector, Steal, Stealer, Worker as DequeWorker};
use std::sync::Arc;

/// A work-stealing queue for tasks.
///
/// Each worker thread has a local queue for tasks it spawns or steals.
/// When the local queue is empty, workers can steal from other workers
/// or the global queue.
pub struct TaskQueue {
    /// The local work-stealing deque.
    worker: DequeWorker<Box<Task>>,
}

impl TaskQueue {
    /// Creates a new empty task queue.
    pub fn new() -> Self {
        Self {
            worker: DequeWorker::new_fifo(),
        }
    }

    /// Pushes a task onto the local queue.
    ///
    /// This is fast and lock-free. Tasks pushed by a worker are
    /// typically executed in LIFO order (for cache efficiency).
    pub fn push(&self, task: Box<Task>) {
        self.worker.push(task);
    }

    /// Pops a task from the local queue.
    ///
    /// Returns `None` if the queue is empty.
    pub fn pop(&self) -> Option<Box<Task>> {
        self.worker.pop()
    }

    /// Creates a stealer handle for this queue.
    ///
    /// Other workers can use this handle to steal tasks from this queue.
    pub fn stealer(&self) -> Stealer<Box<Task>> {
        self.worker.stealer()
    }

    /// Returns true if the queue is empty.
    pub fn is_empty(&self) -> bool {
        self.worker.is_empty()
    }

    /// Returns the number of tasks in the queue.
    pub fn len(&self) -> usize {
        self.worker.len()
    }
}

impl Default for TaskQueue {
    fn default() -> Self {
        Self::new()
    }
}

/// The global task queue.
///
/// Tasks spawned from outside the scheduler or by workers with full
/// local queues go here. Workers check this queue when their local
/// queue is empty.
pub struct GlobalQueue {
    injector: Injector<Box<Task>>,
}

impl GlobalQueue {
    /// Creates a new empty global queue.
    pub fn new() -> Self {
        Self {
            injector: Injector::new(),
        }
    }

    /// Pushes a task onto the global queue.
    ///
    /// This is lock-free and safe to call from any thread.
    pub fn push(&self, task: Box<Task>) {
        self.injector.push(task);
    }

    /// Steals a task from the global queue.
    ///
    /// Returns `Steal::Success` if a task was stolen, `Steal::Empty`
    /// if the queue is empty, or `Steal::Retry` if the operation
    /// should be retried.
    pub fn steal(&self) -> Steal<Box<Task>> {
        self.injector.steal()
    }

    /// Returns true if the global queue is empty.
    pub fn is_empty(&self) -> bool {
        self.injector.is_empty()
    }

    /// Returns the number of tasks in the global queue.
    pub fn len(&self) -> usize {
        self.injector.len()
    }
}

impl Default for GlobalQueue {
    fn default() -> Self {
        Self::new()
    }
}

/// A handle for stealing from other workers.
///
/// Each worker maintains a list of stealers for all other workers.
/// When looking for work, a worker tries to steal from other workers
/// using these handles.
pub struct StealHandle {
    stealers: Vec<Stealer<Box<Task>>>,
}

impl StealHandle {
    /// Creates a new steal handle with the given stealers.
    pub fn new(stealers: Vec<Stealer<Box<Task>>>) -> Self {
        Self { stealers }
    }

    /// Attempts to steal a task from other workers.
    ///
    /// Iterates through all stealers and attempts to steal from each.
    /// Returns the first successfully stolen task.
    pub fn steal(&self) -> Option<Box<Task>> {
        // Try stealing from each worker in order
        // In a more sophisticated implementation, we might randomize
        // the order to reduce contention
        for stealer in &self.stealers {
            match stealer.steal() {
                Steal::Success(task) => return Some(task),
                Steal::Empty => continue,
                Steal::Retry => continue,
            }
        }
        None
    }

    /// Attempts to steal a task using a randomized strategy.
    ///
    /// This can help reduce contention between workers.
    pub fn steal_random(&self) -> Option<Box<Task>> {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        use std::thread;

        if self.stealers.is_empty() {
            return None;
        }

        // Generate a starting index based on thread ID
        let mut hasher = DefaultHasher::new();
        thread::current().id().hash(&mut hasher);
        let start = hasher.finish() as usize % self.stealers.len();

        // Try stealing from each worker starting from the random index
        for i in 0..self.stealers.len() {
            let idx = (start + i) % self.stealers.len();
            match self.stealers[idx].steal() {
                Steal::Success(task) => return Some(task),
                Steal::Empty => continue,
                Steal::Retry => continue,
            }
        }
        None
    }

    /// Returns the number of stealers.
    pub fn len(&self) -> usize {
        self.stealers.len()
    }

    /// Returns true if there are no stealers.
    pub fn is_empty(&self) -> bool {
        self.stealers.is_empty()
    }
}

/// Statistics for queue operations.
#[derive(Debug, Default, Clone, Copy)]
pub struct QueueStats {
    /// Number of tasks pushed to local queue.
    pub local_pushes: u64,
    /// Number of tasks popped from local queue.
    pub local_pops: u64,
    /// Number of successful steals from other workers.
    pub steals_from_workers: u64,
    /// Number of successful steals from global queue.
    pub steals_from_global: u64,
    /// Number of failed steal attempts.
    pub failed_steals: u64,
}

impl QueueStats {
    /// Creates a new empty stats counter.
    pub fn new() -> Self {
        Self::default()
    }

    /// Resets all counters to zero.
    pub fn reset(&mut self) {
        *self = Self::new();
    }

    /// Returns the total number of tasks processed.
    pub fn total_processed(&self) -> u64 {
        self.local_pops + self.steals_from_workers + self.steals_from_global
    }
}

/// A shared queue handle for sending tasks to the scheduler.
pub struct QueueHandle {
    global: Arc<GlobalQueue>,
}

impl QueueHandle {
    /// Creates a new queue handle.
    pub fn new(global: Arc<GlobalQueue>) -> Self {
        Self { global }
    }

    /// Spawns a task onto the global queue.
    pub fn spawn(&self, task: Box<Task>) {
        self.global.push(task);
    }
}

impl Clone for QueueHandle {
    fn clone(&self) -> Self {
        Self {
            global: Arc::clone(&self.global),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::task::Task;

    #[test]
    fn test_task_queue_basic() {
        let queue = TaskQueue::new();
        assert!(queue.is_empty());

        let task = Box::new(Task::new(|| {}, None));
        queue.push(task);
        assert!(!queue.is_empty());
        assert_eq!(queue.len(), 1);

        let popped = queue.pop();
        assert!(popped.is_some());
        assert!(queue.is_empty());
    }

    #[test]
    fn test_global_queue() {
        let global = GlobalQueue::new();
        assert!(global.is_empty());

        let task = Box::new(Task::new(|| {}, None));
        global.push(task);
        assert!(!global.is_empty());

        match global.steal() {
            Steal::Success(_) => {}
            _ => panic!("Expected to steal a task"),
        }

        assert!(global.is_empty());
    }

    #[test]
    fn test_steal_handle() {
        let _worker1 = TaskQueue::new();
        let worker2 = TaskQueue::new();

        // Add tasks to worker2
        worker2.push(Box::new(Task::new(|| {}, None)));
        worker2.push(Box::new(Task::new(|| {}, None)));

        // Create steal handle for worker1 to steal from worker2
        let steal_handle = StealHandle::new(vec![worker2.stealer()]);

        // Worker1 should be able to steal from worker2
        let stolen = steal_handle.steal();
        assert!(stolen.is_some());

        let stolen = steal_handle.steal();
        assert!(stolen.is_some());

        // No more tasks to steal
        let stolen = steal_handle.steal();
        assert!(stolen.is_none());
    }

    #[test]
    fn test_steal_random() {
        let _worker1 = TaskQueue::new();
        let worker2 = TaskQueue::new();
        let worker3 = TaskQueue::new();

        worker2.push(Box::new(Task::new(|| {}, None)));
        worker3.push(Box::new(Task::new(|| {}, None)));

        let steal_handle = StealHandle::new(vec![worker2.stealer(), worker3.stealer()]);

        // Should be able to steal tasks
        let stolen1 = steal_handle.steal_random();
        assert!(stolen1.is_some());

        let stolen2 = steal_handle.steal_random();
        assert!(stolen2.is_some());

        let stolen3 = steal_handle.steal_random();
        assert!(stolen3.is_none());
    }

    #[test]
    fn test_queue_stats() {
        let mut stats = QueueStats::new();
        assert_eq!(stats.total_processed(), 0);

        stats.local_pops = 5;
        stats.steals_from_workers = 3;
        stats.steals_from_global = 2;

        assert_eq!(stats.total_processed(), 10);

        stats.reset();
        assert_eq!(stats.total_processed(), 0);
    }

    #[test]
    fn test_queue_handle() {
        let global = Arc::new(GlobalQueue::new());
        let handle = QueueHandle::new(global.clone());

        let task = Box::new(Task::new(|| {}, None));
        handle.spawn(task);

        assert!(!global.is_empty());

        // Test clone
        let handle2 = handle.clone();
        let task2 = Box::new(Task::new(|| {}, None));
        handle2.spawn(task2);

        assert_eq!(global.len(), 2);
    }
}
