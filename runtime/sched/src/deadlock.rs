//! Deadlock detection for the Jet scheduler
//!
//! This module provides mechanisms to detect potential deadlocks in the task
//! scheduler. It tracks task dependencies and detects cycles in the wait graph.
//!
//! # Usage
//!
//! The deadlock detector runs periodically and checks for cycles in the
//! dependency graph of blocked tasks. When a cycle is detected, it reports
//! the involved tasks for debugging.
//!
//! ```ignore
//! use jet_rt_sched::deadlock::DeadlockDetector;
//!
//! let detector = DeadlockDetector::new();
//! detector.register_task_dependency(task_a, task_b); // a waits for b
//!
//! if let Some(cycle) = detector.detect_cycle() {
//!     println!("Deadlock detected! Tasks: {:?}", cycle);
//! }
//! ```

use crate::task::TaskId;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

/// A node in the wait-for graph.
#[derive(Debug, Clone)]
struct WaitNode {
    /// The task being waited on.
    waiting_for: TaskId,
    /// Timestamp when the dependency was registered.
    timestamp: std::time::Instant,
}

/// Deadlock detector for the scheduler.
///
/// Tracks dependencies between tasks and detects cycles in the wait graph.
/// This is used to identify potential deadlocks and provide debugging information.
pub struct DeadlockDetector {
    /// The wait-for graph: task_id -> Vec of tasks it is waiting for.
    wait_graph: Mutex<HashMap<TaskId, Vec<WaitNode>>>,
    /// Set of tasks currently known to the detector.
    known_tasks: Mutex<HashSet<TaskId>>,
    /// Statistics about detected deadlocks.
    stats: DeadlockStats,
    /// Whether deadlock detection is enabled.
    enabled: AtomicBool,
    /// Timeout threshold for detecting potential deadlocks (in milliseconds).
    timeout_threshold_ms: AtomicU64,
}

/// Statistics about deadlock detection.
#[derive(Debug, Default)]
pub struct DeadlockStats {
    /// Number of cycles detected.
    pub cycles_detected: AtomicU64,
    /// Number of tasks involved in deadlocks.
    pub tasks_in_deadlock: AtomicU64,
    /// Number of false positives (cleared dependencies).
    pub cleared_dependencies: AtomicU64,
}

impl DeadlockStats {
    /// Creates new statistics with zero values.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the number of cycles detected.
    pub fn cycles_detected(&self) -> u64 {
        self.cycles_detected.load(Ordering::Relaxed)
    }

    /// Returns the number of tasks involved in deadlocks.
    pub fn tasks_in_deadlock(&self) -> u64 {
        self.tasks_in_deadlock.load(Ordering::Relaxed)
    }
}

/// Information about a detected deadlock cycle.
#[derive(Debug, Clone)]
pub struct DeadlockCycle {
    /// The tasks involved in the cycle, in order.
    pub tasks: Vec<TaskId>,
    /// Human-readable description of the deadlock.
    pub description: String,
    /// Timestamp when the deadlock was detected.
    pub detected_at: std::time::Instant,
}

impl DeadlockDetector {
    /// Creates a new deadlock detector.
    pub fn new() -> Self {
        Self {
            wait_graph: Mutex::new(HashMap::new()),
            known_tasks: Mutex::new(HashSet::new()),
            stats: DeadlockStats::new(),
            enabled: AtomicBool::new(true),
            timeout_threshold_ms: AtomicU64::new(5000), // 5 seconds default
        }
    }

    /// Creates a new deadlock detector that is disabled by default.
    pub fn disabled() -> Self {
        let detector = Self::new();
        detector.set_enabled(false);
        detector
    }

    /// Enables or disables deadlock detection.
    pub fn set_enabled(&self, enabled: bool) {
        self.enabled.store(enabled, Ordering::Relaxed);
    }

    /// Returns whether deadlock detection is enabled.
    pub fn is_enabled(&self) -> bool {
        self.enabled.load(Ordering::Relaxed)
    }

    /// Sets the timeout threshold for detecting potential deadlocks.
    pub fn set_timeout_threshold(&self, ms: u64) {
        self.timeout_threshold_ms.store(ms, Ordering::Relaxed);
    }

    /// Returns the current timeout threshold in milliseconds.
    pub fn timeout_threshold_ms(&self) -> u64 {
        self.timeout_threshold_ms.load(Ordering::Relaxed)
    }

    /// Registers a task dependency: `waiting_task` is waiting for `target_task`.
    ///
    /// This adds an edge to the wait-for graph.
    pub fn register_dependency(
        &self,
        waiting_task: TaskId,
        target_task: TaskId,
        _description: Option<String>,
    ) {
        if !self.is_enabled() {
            return;
        }

        // Don't register self-dependencies
        if waiting_task == target_task {
            return;
        }

        let node = WaitNode {
            waiting_for: target_task,
            timestamp: std::time::Instant::now(),
        };

        let mut graph = self.wait_graph.lock().unwrap();
        graph.entry(waiting_task).or_default().push(node);

        let mut tasks = self.known_tasks.lock().unwrap();
        tasks.insert(waiting_task);
        tasks.insert(target_task);
    }

    /// Removes all dependencies for a task when it is unblocked.
    ///
    /// This removes all outgoing edges from `task_id` in the wait-for graph.
    pub fn clear_dependencies(&self, task_id: TaskId) {
        let mut graph = self.wait_graph.lock().unwrap();
        if graph.remove(&task_id).is_some() {
            self.stats
                .cleared_dependencies
                .fetch_add(1, Ordering::Relaxed);
        }
    }

    /// Removes a specific dependency.
    pub fn clear_specific_dependency(&self, waiting_task: TaskId, target_task: TaskId) {
        let mut graph = self.wait_graph.lock().unwrap();
        if let Some(edges) = graph.get_mut(&waiting_task) {
            edges.retain(|node| node.waiting_for != target_task);
            if edges.is_empty() {
                graph.remove(&waiting_task);
            }
        }
    }

    /// Detects cycles in the wait-for graph using DFS.
    ///
    /// Returns the first cycle found, or None if no cycles exist.
    pub fn detect_cycle(&self) -> Option<DeadlockCycle> {
        if !self.is_enabled() {
            return None;
        }

        let graph = self.wait_graph.lock().unwrap();
        let tasks = self.known_tasks.lock().unwrap();

        if tasks.is_empty() {
            return None;
        }

        let mut visited = HashSet::new();
        let mut recursion_stack = HashSet::new();
        let mut path = Vec::new();

        for &task_id in tasks.iter() {
            if !visited.contains(&task_id) {
                if let Some(cycle) = Self::dfs_detect_cycle(
                    &graph,
                    task_id,
                    &mut visited,
                    &mut recursion_stack,
                    &mut path,
                ) {
                    // Update statistics
                    self.stats.cycles_detected.fetch_add(1, Ordering::Relaxed);
                    self.stats
                        .tasks_in_deadlock
                        .fetch_add(cycle.tasks.len() as u64, Ordering::Relaxed);

                    return Some(cycle);
                }
            }
        }

        None
    }

    /// Depth-first search to detect cycles.
    fn dfs_detect_cycle(
        graph: &HashMap<TaskId, Vec<WaitNode>>,
        task_id: TaskId,
        visited: &mut HashSet<TaskId>,
        recursion_stack: &mut HashSet<TaskId>,
        path: &mut Vec<TaskId>,
    ) -> Option<DeadlockCycle> {
        visited.insert(task_id);
        recursion_stack.insert(task_id);
        path.push(task_id);

        if let Some(neighbors) = graph.get(&task_id) {
            for node in neighbors {
                let neighbor_id = node.waiting_for;

                if !visited.contains(&neighbor_id) {
                    if let Some(cycle) =
                        Self::dfs_detect_cycle(graph, neighbor_id, visited, recursion_stack, path)
                    {
                        return Some(cycle);
                    }
                } else if recursion_stack.contains(&neighbor_id) {
                    // Found a cycle - extract it from the path
                    let cycle_start = path.iter().position(|&id| id == neighbor_id).unwrap();
                    let cycle_tasks: Vec<TaskId> = path[cycle_start..].to_vec();

                    let description = format!(
                        "Task {} is waiting for task {}, forming a cycle of {} tasks",
                        task_id,
                        neighbor_id,
                        cycle_tasks.len()
                    );

                    return Some(DeadlockCycle {
                        tasks: cycle_tasks,
                        description,
                        detected_at: std::time::Instant::now(),
                    });
                }
            }
        }

        path.pop();
        recursion_stack.remove(&task_id);
        None
    }

    /// Detects tasks that have been waiting for longer than the timeout threshold.
    ///
    /// These may indicate potential deadlocks or just slow operations.
    pub fn detect_slow_tasks(&self) -> Vec<(TaskId, TaskId, u64)> {
        if !self.is_enabled() {
            return Vec::new();
        }

        let graph = self.wait_graph.lock().unwrap();
        let threshold = self.timeout_threshold_ms();
        let now = std::time::Instant::now();
        let mut slow_tasks = Vec::new();

        for (task_id, nodes) in graph.iter() {
            for node in nodes {
                let elapsed_ms = now.duration_since(node.timestamp).as_millis() as u64;
                if elapsed_ms > threshold {
                    slow_tasks.push((*task_id, node.waiting_for, elapsed_ms));
                }
            }
        }

        slow_tasks
    }

    /// Returns a snapshot of the current wait graph for debugging.
    pub fn get_wait_graph_snapshot(&self) -> Vec<(TaskId, Vec<TaskId>)> {
        let graph = self.wait_graph.lock().unwrap();
        graph
            .iter()
            .map(|(k, v)| (*k, v.iter().map(|n| n.waiting_for).collect()))
            .collect()
    }

    /// Returns statistics about deadlock detection.
    pub fn stats(&self) -> &DeadlockStats {
        &self.stats
    }

    /// Clears all registered dependencies.
    pub fn clear_all(&self) {
        let mut graph = self.wait_graph.lock().unwrap();
        graph.clear();
        let mut tasks = self.known_tasks.lock().unwrap();
        tasks.clear();
    }
}

impl Default for DeadlockDetector {
    fn default() -> Self {
        Self::new()
    }
}

/// Global deadlock detector instance.
static GLOBAL_DETECTOR: std::sync::OnceLock<Arc<DeadlockDetector>> = std::sync::OnceLock::new();

/// Returns the global deadlock detector, creating it if necessary.
pub fn global_detector() -> Arc<DeadlockDetector> {
    GLOBAL_DETECTOR
        .get_or_init(|| Arc::new(DeadlockDetector::new()))
        .clone()
}

/// Sets the global deadlock detector.
pub fn set_global_detector(detector: Arc<DeadlockDetector>) {
    // This will panic if already set, which is intentional
    let _ = GLOBAL_DETECTOR.set(detector);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deadlock_detector_creation() {
        let detector = DeadlockDetector::new();
        assert!(detector.is_enabled());
    }

    #[test]
    fn test_disabled_detector() {
        let detector = DeadlockDetector::disabled();
        assert!(!detector.is_enabled());
        assert!(detector.detect_cycle().is_none());
    }

    #[test]
    fn test_simple_cycle_detection() {
        let detector = DeadlockDetector::new();

        // Create a simple cycle: A -> B -> C -> A
        detector.register_dependency(1, 2, Some("A waits for B".to_string()));
        detector.register_dependency(2, 3, Some("B waits for C".to_string()));
        detector.register_dependency(3, 1, Some("C waits for A".to_string()));

        let cycle = detector.detect_cycle();
        assert!(cycle.is_some());

        let cycle = cycle.unwrap();
        assert_eq!(cycle.tasks.len(), 3);
        assert!(cycle.tasks.contains(&1));
        assert!(cycle.tasks.contains(&2));
        assert!(cycle.tasks.contains(&3));
    }

    #[test]
    fn test_no_cycle() {
        let detector = DeadlockDetector::new();

        // Create a chain without cycle: A -> B -> C
        detector.register_dependency(1, 2, None);
        detector.register_dependency(2, 3, None);

        assert!(detector.detect_cycle().is_none());
    }

    #[test]
    fn test_cycle_cleared_on_unblock() {
        let detector = DeadlockDetector::new();

        // Create a cycle
        detector.register_dependency(1, 2, None);
        detector.register_dependency(2, 1, None);

        assert!(detector.detect_cycle().is_some());

        // Clear one side of the cycle
        detector.clear_dependencies(1);

        assert!(detector.detect_cycle().is_none());
    }

    #[test]
    fn test_self_dependency_ignored() {
        let detector = DeadlockDetector::new();

        // Self-dependency should be ignored
        detector.register_dependency(1, 1, None);

        assert!(detector.detect_cycle().is_none());
    }

    #[test]
    fn test_slow_task_detection() {
        let detector = DeadlockDetector::new();
        detector.set_timeout_threshold(1); // 1ms threshold

        detector.register_dependency(1, 2, Some("long wait".to_string()));

        // Wait a bit
        std::thread::sleep(std::time::Duration::from_millis(10));

        let slow_tasks = detector.detect_slow_tasks();
        assert!(!slow_tasks.is_empty());
        assert_eq!(slow_tasks[0].0, 1);
        assert_eq!(slow_tasks[0].1, 2);
    }

    #[test]
    fn test_complex_cycle() {
        let detector = DeadlockDetector::new();

        // Create a complex graph with multiple cycles
        detector.register_dependency(1, 2, None);
        detector.register_dependency(2, 3, None);
        detector.register_dependency(3, 4, None);
        detector.register_dependency(4, 2, None); // Cycle: 2 -> 3 -> 4 -> 2
        detector.register_dependency(1, 5, None);
        detector.register_dependency(5, 6, None);

        let cycle = detector.detect_cycle();
        assert!(cycle.is_some());

        // The cycle should involve tasks 2, 3, and 4
        let cycle_tasks = cycle.unwrap().tasks;
        assert!(cycle_tasks.contains(&2));
        assert!(cycle_tasks.contains(&3));
        assert!(cycle_tasks.contains(&4));
    }

    #[test]
    fn test_statistics() {
        let detector = DeadlockDetector::new();

        assert_eq!(detector.stats().cycles_detected(), 0);

        // Create and detect a cycle
        detector.register_dependency(1, 2, None);
        detector.register_dependency(2, 1, None);
        detector.detect_cycle();

        assert_eq!(detector.stats().cycles_detected(), 1);
    }
}
