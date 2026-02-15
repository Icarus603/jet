//! Signal handling for the runtime.
//!
//! This module provides signal handling for:
//! - SIGINT (Ctrl+C) - graceful shutdown
//! - SIGTERM - graceful shutdown
//! - SIGSEGV - stack overflow detection
//! - SIGUSR1/SIGUSR2 - GC coordination
//!
//! # Platform Support
//!
//! - Unix (Linux, macOS): Full support via `sigaction`
//! - Windows: Limited support (not yet implemented)

use std::sync::atomic::{AtomicBool, Ordering};

/// Flag indicating if signal handlers have been installed.
static SIGNAL_HANDLERS_INSTALLED: AtomicBool = AtomicBool::new(false);

/// Initializes signal handlers.
///
/// This should be called once during runtime initialization.
/// It is safe to call multiple times - subsequent calls are no-ops.
///
/// # Platform Support
///
/// - Unix: Installs handlers for SIGINT, SIGTERM, SIGSEGV, SIGUSR1, SIGUSR2
/// - Windows: No-op (signals handled differently)
pub fn init() {
    if SIGNAL_HANDLERS_INSTALLED.load(Ordering::SeqCst) {
        return;
    }

    #[cfg(unix)]
    {
        unsafe {
            install_unix_handlers();
        }
    }

    SIGNAL_HANDLERS_INSTALLED.store(true, Ordering::SeqCst);
}

/// Returns true if signal handlers have been installed.
/// Note: Part of public API, may be used by external consumers.
#[allow(dead_code)]
pub fn are_handlers_installed() -> bool {
    SIGNAL_HANDLERS_INSTALLED.load(Ordering::SeqCst)
}

#[cfg(unix)]
unsafe fn install_unix_handlers() {
    use libc::{
        sigaction, sigemptyset, sighandler_t, SA_SIGINFO, SIGINT, SIGSEGV, SIGTERM, SIGUSR1,
        SIGUSR2,
    };

    // Handler for graceful shutdown (SIGINT, SIGTERM)
    extern "C" fn shutdown_handler(_sig: libc::c_int) {
        crate::shutdown::request_shutdown();
    }

    // Handler for segfault (stack overflow detection)
    extern "C" fn segfault_handler(
        sig: libc::c_int,
        _info: *mut libc::siginfo_t,
        _context: *mut libc::c_void,
    ) {
        // Check if this is a stack overflow
        if sig == SIGSEGV {
            // In a real implementation, we would check if the faulting address
            // is near the stack limit to detect stack overflow
            eprintln!("Segmentation fault detected");

            // Try to print a backtrace if possible
            #[cfg(feature = "std")]
            {
                eprintln!("Backtrace:");
                // std::backtrace::Backtrace::force_capture();
            }
        }

        // Re-raise the signal to get the default behavior (core dump)
        unsafe {
            libc::raise(sig);
        }
    }

    // Handler for GC signals
    extern "C" fn gc_handler(sig: libc::c_int) {
        match sig {
            SIGUSR1 => {
                // Request GC
                // In a real implementation, this would signal the GC thread
            }
            SIGUSR2 => {
                // Stop for GC (safe point)
                // In a real implementation, this would pause the thread
            }
            _ => {}
        }
    }

    // Set up SIGINT handler
    let mut sa: sigaction = std::mem::zeroed();
    sa.sa_sigaction = shutdown_handler as *const () as sighandler_t;
    sigemptyset(&mut sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGINT, &sa, std::ptr::null_mut());

    // Set up SIGTERM handler
    sigaction(SIGTERM, &sa, std::ptr::null_mut());

    // Set up SIGSEGV handler with SA_SIGINFO
    sa.sa_sigaction = segfault_handler as *const () as sighandler_t;
    sa.sa_flags = SA_SIGINFO;
    sigaction(SIGSEGV, &sa, std::ptr::null_mut());

    // Set up GC signal handlers
    sa.sa_sigaction = gc_handler as *const () as sighandler_t;
    sa.sa_flags = 0;
    sigaction(SIGUSR1, &sa, std::ptr::null_mut());
    sigaction(SIGUSR2, &sa, std::ptr::null_mut());
}

/// Requests a garbage collection via signal.
///
/// This sends SIGUSR1 to the current process to request a GC cycle.
/// It is a no-op on platforms that don't support signals.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn request_gc_signal() {
    #[cfg(unix)]
    unsafe {
        libc::raise(SIGUSR1);
    }
}

/// Signals threads to stop at safe points.
///
/// This sends SIGUSR2 to the current process to signal threads
/// to reach safe points for GC.
/// Note: Part of public API for GC integration, may be used by external callers.
#[allow(dead_code)]
pub fn signal_safe_point() {
    #[cfg(unix)]
    unsafe {
        libc::raise(SIGUSR2);
    }
}

#[cfg(unix)]
#[allow(dead_code)]
const SIGUSR1: libc::c_int = 10;
#[cfg(unix)]
#[allow(dead_code)]
const SIGUSR2: libc::c_int = 12;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signal_init() {
        // Installing handlers should always result in installed=true,
        // regardless of prior runtime initialization in other tests.
        init();
        assert!(are_handlers_installed());

        // Second call should be a no-op
        init();
        assert!(are_handlers_installed());
    }
}
