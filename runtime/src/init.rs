//! Runtime initialization.
//!
//! This module handles runtime initialization and tracks whether
//! the runtime has been properly initialized.

use std::sync::atomic::{AtomicBool, Ordering};

static INITIALIZED: AtomicBool = AtomicBool::new(false);

/// Returns true if the runtime is initialized.
///
/// # Example
///
/// ```
/// use jet_rt::is_initialized;
///
/// assert!(!is_initialized());
/// ```
pub fn is_initialized() -> bool {
    INITIALIZED.load(Ordering::SeqCst)
}

/// Marks the runtime as initialized.
///
/// This is called internally by `Runtime::new()`.
pub(crate) fn mark_initialized() {
    INITIALIZED.store(true, Ordering::SeqCst);
}

/// Marks the runtime as uninitialized.
///
/// This is called internally during shutdown.
pub(crate) fn mark_uninitialized() {
    INITIALIZED.store(false, Ordering::SeqCst);
}

/// Initializes the runtime subsystem.
///
/// This is called by `Runtime::new()` to perform early initialization
/// before other components are set up.
pub fn initialize() {
    // Early initialization that doesn't require allocation
    // Signal handlers are set up separately
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_initialization_state() {
        // Initially not initialized
        assert!(!is_initialized());

        // Mark as initialized
        mark_initialized();
        assert!(is_initialized());

        // Mark as uninitialized
        mark_uninitialized();
        assert!(!is_initialized());
    }
}
