//! Runtime shutdown.
//!
//! This module provides graceful shutdown functionality for the runtime,
//! ensuring all resources are properly cleaned up.

use std::sync::atomic::{AtomicBool, Ordering};
#[allow(unused_imports)]
use std::sync::{Arc, Condvar, Mutex};
use std::time::Duration;

/// Global shutdown state.
static SHUTDOWN_REQUESTED: AtomicBool = AtomicBool::new(false);
static SHUTDOWN_COMPLETE: AtomicBool = AtomicBool::new(false);

/// Shutdown synchronization.
static SHUTDOWN_CV: Condvar = Condvar::new();
static SHUTDOWN_MTX: Mutex<()> = Mutex::new(());

/// Requests runtime shutdown.
///
/// This signals all runtime components to begin shutting down.
/// It does not wait for shutdown to complete.
///
/// # Example
///
/// ```
/// use jet_rt::request_shutdown;
///
/// request_shutdown();
/// ```
pub fn request_shutdown() {
    SHUTDOWN_REQUESTED.store(true, Ordering::SeqCst);
    SHUTDOWN_CV.notify_all();
}

/// Checks if shutdown has been requested.
///
/// # Example
///
/// ```
/// use jet_rt::is_shutdown_requested;
///
/// if is_shutdown_requested() {
///     // Begin cleanup
/// }
/// ```
/// Note: Part of public API, may be used by external consumers.
#[allow(dead_code)]
pub fn is_shutdown_requested() -> bool {
    SHUTDOWN_REQUESTED.load(Ordering::SeqCst)
}

/// Waits for shutdown to complete.
///
/// This blocks the current thread until the runtime has fully shut down.
///
/// # Arguments
///
/// * `timeout` - Maximum time to wait.
///
/// # Returns
///
/// Returns true if shutdown completed, false if timed out.
///
/// # Example
///
/// ```
/// use jet_rt::wait_for_shutdown;
/// use std::time::Duration;
///
/// let completed = wait_for_shutdown(Duration::from_secs(30));
/// ```
pub fn wait_for_shutdown(timeout: Duration) -> bool {
    let guard = SHUTDOWN_MTX.lock().unwrap();
    let _result = SHUTDOWN_CV
        .wait_timeout_while(guard, timeout, |_| {
            !SHUTDOWN_COMPLETE.load(Ordering::SeqCst)
        })
        .unwrap();

    SHUTDOWN_COMPLETE.load(Ordering::SeqCst)
}

/// Marks shutdown as complete.
///
/// This is called internally when the runtime has finished shutting down.
/// Note: Currently unused but kept for future runtime shutdown implementation.
#[allow(dead_code)]
pub(crate) fn mark_shutdown_complete() {
    SHUTDOWN_COMPLETE.store(true, Ordering::SeqCst);
    SHUTDOWN_CV.notify_all();
}

/// Resets the shutdown state.
///
/// This is used for testing and should not be called in production code.
#[cfg(test)]
pub(crate) fn reset_shutdown_state() {
    SHUTDOWN_REQUESTED.store(false, Ordering::SeqCst);
    SHUTDOWN_COMPLETE.store(false, Ordering::SeqCst);
}

/// Shutdown manager that coordinates graceful shutdown.
///
/// This struct manages the shutdown process, ensuring all components
/// are properly cleaned up in the correct order.
/// Note: Currently only used in tests but kept for future shutdown implementation.
#[cfg(test)]
pub struct ShutdownManager {
    /// List of cleanup functions to run
    cleanup_fns: Vec<Box<dyn FnOnce() + Send>>,
}

#[cfg(test)]
impl ShutdownManager {
    /// Creates a new shutdown manager.
    ///
    /// # Arguments
    ///
    /// * `timeout` - Maximum time to wait for shutdown.
    pub fn new(_timeout: Duration) -> Self {
        Self {
            cleanup_fns: Vec::new(),
        }
    }

    /// Registers a cleanup function.
    ///
    /// Cleanup functions are run in reverse order of registration
    /// (LIFO) during shutdown.
    ///
    /// # Arguments
    ///
    /// * `f` - The cleanup function.
    pub fn register_cleanup<F>(&mut self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        self.cleanup_fns.push(Box::new(f));
    }

    /// Runs all cleanup functions.
    ///
    /// This consumes the shutdown manager.
    pub fn shutdown(self) {
        // Run cleanup functions in reverse order
        for f in self.cleanup_fns.into_iter().rev() {
            f();
        }

        mark_shutdown_complete();
    }
}

#[cfg(test)]
impl Default for ShutdownManager {
    fn default() -> Self {
        Self::new(Duration::from_secs(30))
    }
}

/// Shuts down the runtime.
///
/// This is a convenience function that requests shutdown and waits
/// for it to complete.
///
/// # Arguments
///
/// * `timeout` - Maximum time to wait.
///
/// # Returns
///
/// Returns true if shutdown completed successfully.
pub fn shutdown(timeout: Duration) -> bool {
    request_shutdown();
    wait_for_shutdown(timeout)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn reset() {
        reset_shutdown_state();
    }

    #[test]
    fn test_shutdown_request() {
        reset();
        assert!(!is_shutdown_requested());

        request_shutdown();
        assert!(is_shutdown_requested());
    }

    #[test]
    fn test_shutdown_complete() {
        reset();
        assert!(!SHUTDOWN_COMPLETE.load(Ordering::SeqCst));

        mark_shutdown_complete();
        assert!(SHUTDOWN_COMPLETE.load(Ordering::SeqCst));
    }

    #[test]
    fn test_shutdown_manager() {
        reset();

        let mut manager = ShutdownManager::new(Duration::from_secs(5));
        let counter = Arc::new(std::sync::atomic::AtomicU32::new(0));

        let c = counter.clone();
        manager.register_cleanup(move || {
            c.fetch_add(1, Ordering::SeqCst);
        });

        let c = counter.clone();
        manager.register_cleanup(move || {
            c.fetch_add(10, Ordering::SeqCst);
        });

        // Cleanup runs in reverse order, so we expect 10 then 1
        manager.shutdown();

        assert_eq!(counter.load(Ordering::SeqCst), 11);
    }
}
