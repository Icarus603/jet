//! Panic handling and recovery.
//!
//! This module provides panic handling infrastructure that integrates
//! with the runtime's effect system and allows for recovery from panics
//! in certain contexts.

use std::cell::RefCell;
use std::panic::{self, PanicHookInfo};
use std::sync::Once;

/// Information about a panic.
#[derive(Debug, Clone)]
pub struct PanicInfo {
    /// The panic message (if available).
    pub message: Option<String>,
    /// The location where the panic occurred (if available).
    pub location: Option<String>,
    /// Whether the panic was caught and recovered from.
    pub recovered: bool,
}

impl PanicInfo {
    /// Creates a new `PanicInfo` from the current panic hook info.
    #[allow(dead_code)]
    fn from_hook(info: &PanicHookInfo<'_>) -> Self {
        let message = info
            .payload()
            .downcast_ref::<&str>()
            .map(|s| s.to_string())
            .or_else(|| info.payload().downcast_ref::<String>().cloned());

        let location = info
            .location()
            .map(|loc| format!("{}:{}", loc.file(), loc.line()));

        Self {
            message,
            location,
            recovered: false,
        }
    }
}

/// Result of a potentially panicking operation.
#[derive(Debug)]
pub enum PanicResult<T> {
    /// The operation completed successfully.
    Ok(T),
    /// The operation panicked.
    Panic(PanicInfo),
}

impl<T> PanicResult<T> {
    /// Returns true if the result is `Ok`.
    pub fn is_ok(&self) -> bool {
        matches!(self, PanicResult::Ok(_))
    }

    /// Returns true if the result is `Panic`.
    pub fn is_panic(&self) -> bool {
        matches!(self, PanicResult::Panic(_))
    }

    /// Unwraps the result, panicking if it was a panic.
    pub fn unwrap(self) -> T {
        match self {
            PanicResult::Ok(v) => v,
            PanicResult::Panic(info) => {
                panic!("Called unwrap on a PanicResult that panicked: {:?}", info)
            }
        }
    }

    /// Returns the contained value or a default.
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            PanicResult::Ok(v) => v,
            PanicResult::Panic(_) => default,
        }
    }
}

// Thread-local storage for the current panic hook
type PanicHook = Box<dyn Fn(&PanicInfo) + Send + 'static>;

thread_local! {
    static CURRENT_PANIC_HOOK: RefCell<Option<PanicHook>> = const { RefCell::new(None) };
}

/// Ensures the panic hook is only installed once.
/// Note: Currently unused but kept for future runtime initialization.
#[allow(dead_code)]
static PANIC_HOOK_SET: Once = Once::new();

/// Sets a custom panic handler.
///
/// The handler will be called when a panic occurs in the runtime.
/// This can be used for logging, cleanup, or recovery.
///
/// # Arguments
///
/// * `handler` - The panic handler function.
///
/// # Example
///
/// ```
/// use jet_rt::{set_panic_handler, PanicInfo};
///
/// set_panic_handler(|info: &PanicInfo| {
///     eprintln!("Panic occurred: {:?}", info);
/// });
/// ```
pub fn set_panic_handler<F>(handler: F)
where
    F: Fn(&PanicInfo) + Send + 'static,
{
    CURRENT_PANIC_HOOK.with(|hook| {
        *hook.borrow_mut() = Some(Box::new(handler));
    });
}

/// Installs the runtime panic hook.
///
/// This should be called once during runtime initialization.
/// Note: Currently unused but kept for future runtime initialization.
#[allow(dead_code)]
pub(crate) fn install_hook() {
    PANIC_HOOK_SET.call_once(|| {
        let default_hook = panic::take_hook();

        panic::set_hook(Box::new(move |info| {
            let panic_info = PanicInfo::from_hook(info);

            // Call any registered custom handler
            CURRENT_PANIC_HOOK.with(|hook| {
                if let Some(ref handler) = *hook.borrow() {
                    handler(&panic_info);
                }
            });

            // Call the default hook for standard panic output
            // We need to reconstruct a minimal panic info for the default hook
            default_hook(info);
        }));
    });
}

/// Catches panics in the given closure.
///
/// This function executes the closure and catches any panics that occur,
/// converting them into a `PanicResult`.
///
/// # Arguments
///
/// * `f` - The closure to execute.
///
/// # Returns
///
/// Returns `PanicResult::Ok` with the result if successful, or
/// `PanicResult::Panic` if a panic occurred.
///
/// # Example
///
/// ```
/// use jet_rt::catch_panic;
///
/// let result = catch_panic(|| {
///     "Hello, world!"
/// });
///
/// assert!(result.is_ok());
/// ```
pub fn catch_panic<F, R>(f: F) -> PanicResult<R>
where
    F: FnOnce() -> R + std::panic::UnwindSafe,
{
    match std::panic::catch_unwind(f) {
        Ok(result) => PanicResult::Ok(result),
        Err(payload) => {
            let message = payload
                .downcast_ref::<&str>()
                .map(|s| s.to_string())
                .or_else(|| payload.downcast_ref::<String>().cloned())
                .or_else(|| Some("unknown panic".to_string()));

            PanicResult::Panic(PanicInfo {
                message,
                location: None,
                recovered: true,
            })
        }
    }
}

/// Checks if the runtime is currently panicking.
///
/// This can be used to detect nested panics and abort instead
/// of attempting to recover.
/// Note: Part of public API, may be used by external consumers.
#[allow(dead_code)]
pub fn is_panicking() -> bool {
    std::thread::panicking()
}

/// Aborts the process immediately.
///
/// This is used for unrecoverable errors where continuing would
/// be unsafe.
/// Note: Part of public API, may be used by external consumers.
#[allow(dead_code)]
pub fn abort() -> ! {
    std::process::abort()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_catch_panic_success() {
        let result = catch_panic(|| 42);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    }

    #[test]
    fn test_catch_panic_failure() {
        let result = catch_panic(|| {
            panic!("Test panic");
        });

        assert!(result.is_panic());
    }

    #[test]
    fn test_panic_result_unwrap_or() {
        let result: PanicResult<i32> = PanicResult::Panic(PanicInfo {
            message: Some("test".to_string()),
            location: None,
            recovered: true,
        });

        assert_eq!(result.unwrap_or(0), 0);
    }

    #[test]
    fn test_is_panicking() {
        // Should not be panicking in normal execution
        assert!(!is_panicking());
    }
}
