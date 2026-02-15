//! Thread coordination between GC and scheduler.
//!
//! This module provides coordination between the garbage collector
//! and the task scheduler, ensuring safe points are respected and
//! roots are properly scanned.

use crate::context::RuntimeContext;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

/// Thread coordination state.
static COORD_STATE: CoordState = CoordState {
    gc_requested: AtomicBool::new(false),
    gc_in_progress: AtomicBool::new(false),
    threads_stopped: AtomicUsize::new(0),
    total_threads: AtomicUsize::new(0),
};

/// Coordination state structure.
struct CoordState {
    /// Flag indicating GC has been requested
    gc_requested: AtomicBool,
    /// Flag indicating GC is in progress
    gc_in_progress: AtomicBool,
    /// Number of threads stopped at safe points
    threads_stopped: AtomicUsize,
    /// Total number of worker threads
    total_threads: AtomicUsize,
}

/// Initializes thread coordination.
///
/// This is called during runtime initialization to set up
/// coordination between the GC and scheduler.
pub fn init(_context: Arc<RuntimeContext>) {
    // Reset coordination state
    COORD_STATE.gc_requested.store(false, Ordering::SeqCst);
    COORD_STATE.gc_in_progress.store(false, Ordering::SeqCst);
    COORD_STATE.threads_stopped.store(0, Ordering::SeqCst);
    COORD_STATE.total_threads.store(0, Ordering::SeqCst);
}

/// Cleans up thread coordination.
pub fn cleanup() {
    // Reset all state
    COORD_STATE.gc_requested.store(false, Ordering::SeqCst);
    COORD_STATE.gc_in_progress.store(false, Ordering::SeqCst);
    COORD_STATE.threads_stopped.store(0, Ordering::SeqCst);
    COORD_STATE.total_threads.store(0, Ordering::SeqCst);
}

/// Registers a worker thread with the coordination system.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn register_worker_thread() {
    COORD_STATE.total_threads.fetch_add(1, Ordering::SeqCst);
    crate::safe_point::register_thread();
}

/// Unregisters a worker thread.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn unregister_worker_thread() {
    COORD_STATE.total_threads.fetch_sub(1, Ordering::SeqCst);
    crate::safe_point::unregister_thread();
}

/// Requests a GC cycle.
///
/// This signals all worker threads to reach safe points
/// so that GC can begin.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn request_gc() {
    COORD_STATE.gc_requested.store(true, Ordering::SeqCst);
}

/// Checks if GC has been requested.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn is_gc_requested() -> bool {
    COORD_STATE.gc_requested.load(Ordering::SeqCst)
}

/// Begins a GC cycle.
///
/// This should be called after all threads have reached safe points.
/// Returns true if GC can proceed, false if another GC is already in progress.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn begin_gc() -> bool {
    // Try to acquire the GC lock
    if COORD_STATE
        .gc_in_progress
        .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
        .is_ok()
    {
        COORD_STATE.gc_requested.store(false, Ordering::SeqCst);
        true
    } else {
        false
    }
}

/// Ends a GC cycle.
///
/// This should be called when GC is complete to release threads.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn end_gc() {
    COORD_STATE.gc_in_progress.store(false, Ordering::SeqCst);
    COORD_STATE.threads_stopped.store(0, Ordering::SeqCst);

    // Release all threads from safe points
    crate::safe_point::release_safe_point();
}

/// Checks if GC is currently in progress.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn is_gc_in_progress() -> bool {
    COORD_STATE.gc_in_progress.load(Ordering::SeqCst)
}

/// Called by worker threads when they reach a safe point.
///
/// If GC has been requested, this will block until GC completes.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn at_safe_point() {
    if is_gc_requested() || is_gc_in_progress() {
        // Increment stopped thread count
        COORD_STATE.threads_stopped.fetch_add(1, Ordering::SeqCst);

        // Wait for GC to complete
        while is_gc_in_progress() {
            std::thread::yield_now();
        }

        // Decrement stopped thread count
        COORD_STATE.threads_stopped.fetch_sub(1, Ordering::SeqCst);
    }
}

/// Returns the number of threads stopped at safe points.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn threads_stopped() -> usize {
    COORD_STATE.threads_stopped.load(Ordering::SeqCst)
}

/// Returns the total number of registered worker threads.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn total_worker_threads() -> usize {
    COORD_STATE.total_threads.load(Ordering::SeqCst)
}

/// Waits for all threads to reach safe points.
///
/// # Arguments
///
/// * `timeout` - Maximum time to wait.
///
/// # Returns
///
/// Returns true if all threads reached safe points, false if timed out.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn wait_for_safe_points(timeout: std::time::Duration) -> bool {
    let start = std::time::Instant::now();
    let total = total_worker_threads();

    while threads_stopped() < total {
        if start.elapsed() >= timeout {
            return false;
        }
        std::thread::yield_now();
    }

    true
}

/// GC coordinator that manages the GC lifecycle.
/// Note: Currently only used in tests but kept for future GC coordination implementation.
#[cfg(test)]
#[allow(dead_code)]
pub struct GcCoordinator {
    /// The runtime context
    context: Arc<RuntimeContext>,
}

#[cfg(test)]
#[allow(dead_code)]
impl GcCoordinator {
    /// Creates a new GC coordinator.
    pub fn new(context: Arc<RuntimeContext>) -> Self {
        Self { context }
    }

    /// Performs a coordinated GC cycle.
    ///
    /// This method:
    /// 1. Requests all threads to reach safe points
    /// 2. Waits for threads to stop
    /// 3. Performs GC
    /// 4. Resumes all threads
    ///
    /// # Arguments
    ///
    /// * `timeout` - Maximum time to wait for safe points.
    ///
    /// # Returns
    ///
    /// Returns true if GC was performed, false if timed out.
    pub fn coordinate_gc(&self, timeout: std::time::Duration) -> bool {
        // Request GC
        request_gc();

        // Wait for all threads to reach safe points
        if !wait_for_safe_points(timeout) {
            return false;
        }

        // Begin GC
        if !begin_gc() {
            // Another GC is already in progress
            return false;
        }

        // Perform GC (this would be called from the GC subsystem)
        // In a real implementation, this would call into the GC

        // End GC and resume threads
        end_gc();

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::RuntimeConfig;

    fn reset_state() {
        COORD_STATE.gc_requested.store(false, Ordering::SeqCst);
        COORD_STATE.gc_in_progress.store(false, Ordering::SeqCst);
        COORD_STATE.threads_stopped.store(0, Ordering::SeqCst);
        COORD_STATE.total_threads.store(0, Ordering::SeqCst);
    }

    #[test]
    fn test_initial_state() {
        reset_state();
        assert!(!is_gc_requested());
        assert!(!is_gc_in_progress());
        assert_eq!(threads_stopped(), 0);
        assert_eq!(total_worker_threads(), 0);
    }

    #[test]
    fn test_register_unregister_thread() {
        reset_state();

        register_worker_thread();
        assert_eq!(total_worker_threads(), 1);

        register_worker_thread();
        assert_eq!(total_worker_threads(), 2);

        unregister_worker_thread();
        assert_eq!(total_worker_threads(), 1);
    }

    #[test]
    fn test_gc_request() {
        reset_state();

        assert!(!is_gc_requested());
        request_gc();
        assert!(is_gc_requested());
    }

    #[test]
    fn test_begin_end_gc() {
        reset_state();

        assert!(!is_gc_in_progress());
        assert!(begin_gc());
        assert!(is_gc_in_progress());

        // Second begin should fail
        assert!(!begin_gc());

        end_gc();
        assert!(!is_gc_in_progress());
    }

    #[test]
    fn test_gc_coordinator_creation() {
        reset_state();

        let context = Arc::new(RuntimeContext::new(&RuntimeConfig::default()));
        let coordinator = GcCoordinator::new(context);

        // Just verify creation works
        let _ = coordinator;
    }
}
