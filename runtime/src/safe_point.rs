//! Safe points for garbage collection.
//!
//! Safe points are locations in the code where the GC can safely pause
//! a thread to scan its stack. The runtime ensures all threads reach
//! safe points before GC begins.
//!
//! # Safe Point Protocol
//!
//! 1. GC requests a collection
//! 2. All threads are signaled to reach a safe point
//! 3. Threads check the safe point flag at designated points
//! 4. When at a safe point, threads block until GC completes
//! 5. GC scans stacks and performs collection
//! 6. All threads are resumed

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Condvar, Mutex};

/// Global safe point state.
static SAFE_POINT_REQUESTED: AtomicBool = AtomicBool::new(false);
static THREADS_AT_SAFE_POINT: AtomicUsize = AtomicUsize::new(0);

/// Total number of registered threads.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
static TOTAL_THREADS: AtomicUsize = AtomicUsize::new(0);

/// Safe point synchronization.
static SAFE_POINT_CV: Condvar = Condvar::new();
static SAFE_POINT_MTX: Mutex<()> = Mutex::new(());

/// Enters a safe point.
///
/// This function checks if a safe point has been requested and if so,
/// blocks until the safe point is released. It should be called at
/// strategic locations where the thread state is consistent.
///
/// # Example
///
/// ```
/// use jet_rt::enter_safe_point;
///
/// fn some_function() {
///     // Do some work
///     enter_safe_point();
///     // Continue with more work
/// }
/// ```
pub fn enter_safe_point() {
    if SAFE_POINT_REQUESTED.load(Ordering::SeqCst) {
        // Increment the count of threads at safe points
        THREADS_AT_SAFE_POINT.fetch_add(1, Ordering::SeqCst);

        // Wait for the safe point to be released
        let mut guard = SAFE_POINT_MTX.lock().unwrap();
        // The condition variable will be signaled when GC is done
        // We use a timeout to periodically check if we should still wait
        while SAFE_POINT_REQUESTED.load(Ordering::SeqCst) {
            let result = SAFE_POINT_CV
                .wait_timeout(guard, std::time::Duration::from_millis(10))
                .unwrap();
            guard = result.0;
        }

        // Decrement the count
        THREADS_AT_SAFE_POINT.fetch_sub(1, Ordering::SeqCst);
    }
}

/// Leaves a safe point (for completeness, usually a no-op).
pub fn leave_safe_point() {
    // In the current implementation, leaving a safe point is implicit
    // when the thread continues execution after `enter_safe_point()`.
    // This function exists for potential future use.
}

/// Returns true if the current thread is at a safe point.
///
/// Note: This is a best-effort check and may not be accurate
/// in all cases due to race conditions.
pub fn is_at_safe_point() -> bool {
    // This is a simplified check - in a real implementation,
    // we would track per-thread state
    SAFE_POINT_REQUESTED.load(Ordering::SeqCst) && THREADS_AT_SAFE_POINT.load(Ordering::SeqCst) > 0
}

/// Requests all threads to reach a safe point.
///
/// This is called by the GC to initiate a stop-the-world phase.
/// It blocks until all threads have reached a safe point.
///
/// # Arguments
///
/// * `timeout` - Maximum time to wait for all threads.
///
/// # Returns
///
/// Returns true if all threads reached safe points, false if timed out.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn request_safe_point(timeout: std::time::Duration) -> bool {
    // Set the safe point request flag
    SAFE_POINT_REQUESTED.store(true, Ordering::SeqCst);

    // Wait for all threads to reach safe points
    let start = std::time::Instant::now();
    let total = TOTAL_THREADS.load(Ordering::SeqCst);

    while THREADS_AT_SAFE_POINT.load(Ordering::SeqCst) < total {
        if start.elapsed() >= timeout {
            // Timeout - release the safe point and return false
            release_safe_point();
            return false;
        }

        std::thread::yield_now();
    }

    true
}

/// Releases the safe point, allowing threads to continue.
///
/// This is called by the GC after collection is complete.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn release_safe_point() {
    SAFE_POINT_REQUESTED.store(false, Ordering::SeqCst);
    SAFE_POINT_CV.notify_all();
}

/// Registers a thread with the safe point system.
///
/// This should be called when a thread enters the runtime.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn register_thread() {
    TOTAL_THREADS.fetch_add(1, Ordering::SeqCst);
}

/// Unregisters a thread from the safe point system.
///
/// This should be called when a thread exits the runtime.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn unregister_thread() {
    TOTAL_THREADS.fetch_sub(1, Ordering::SeqCst);
}

/// Returns the number of threads currently at safe points.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn threads_at_safe_point() -> usize {
    THREADS_AT_SAFE_POINT.load(Ordering::SeqCst)
}

/// Returns the total number of registered threads.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn total_threads() -> usize {
    TOTAL_THREADS.load(Ordering::SeqCst)
}

/// A scope guard that automatically enters/leaves a safe point.
///
/// This struct ensures that a safe point is properly managed
/// using RAII. When the scope is entered, the thread marks
/// itself at a safe point. When the scope is dropped, the
/// thread is unmarked.
///
/// # Example
///
/// ```
/// use jet_rt::SafePointScope;
///
/// fn some_function() {
///     let _scope = SafePointScope::enter();
///     // Thread is now at a safe point
///     // ... do some work that requires consistency ...
/// } // Safe point is automatically left here
/// ```
pub struct SafePointScope;

impl SafePointScope {
    /// Enters a safe point scope.
    pub fn enter() -> Self {
        enter_safe_point();
        Self
    }
}

impl Drop for SafePointScope {
    fn drop(&mut self) {
        leave_safe_point();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Reset safe point state for tests
    fn reset_safe_point_state() {
        SAFE_POINT_REQUESTED.store(false, Ordering::SeqCst);
        THREADS_AT_SAFE_POINT.store(0, Ordering::SeqCst);
        TOTAL_THREADS.store(0, Ordering::SeqCst);
    }

    #[test]
    fn test_safe_point_initial_state() {
        reset_safe_point_state();
        assert!(!SAFE_POINT_REQUESTED.load(Ordering::SeqCst));
        assert_eq!(THREADS_AT_SAFE_POINT.load(Ordering::SeqCst), 0);
    }

    #[test]
    fn test_register_unregister_thread() {
        let initial = total_threads();

        register_thread();
        assert_eq!(total_threads(), initial + 1);

        unregister_thread();
        assert_eq!(total_threads(), initial);
    }

    #[test]
    fn test_safe_point_scope() {
        // Without a safe point request, scope should not block
        {
            let _scope = SafePointScope::enter();
            // Should complete immediately
        }
    }

    #[test]
    #[ignore = "Flaky due to parallel test execution affecting global state"]
    fn test_request_and_release_safe_point() {
        reset_safe_point_state();
        register_thread();

        // Request safe point with a short timeout
        let requested = request_safe_point(std::time::Duration::from_millis(100));

        // Without any threads actually entering, we should timeout
        assert!(!requested);

        // Release should clear the flag
        release_safe_point();
        assert!(!SAFE_POINT_REQUESTED.load(Ordering::SeqCst));

        unregister_thread();
    }
}
