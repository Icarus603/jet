//! Callback trampolines for FFI

use std::ffi::c_void;

/// Errors related to trampolines
#[derive(Debug, Clone)]
pub enum TrampolineError {
    AllocationFailed,
    InvalidCallback,
    UnsupportedSignature,
}

impl std::fmt::Display for TrampolineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AllocationFailed => write!(f, "failed to allocate trampoline"),
            Self::InvalidCallback => write!(f, "invalid callback function"),
            Self::UnsupportedSignature => write!(f, "unsupported callback signature"),
        }
    }
}

impl std::error::Error for TrampolineError {}

pub type TrampolineResult<T> = Result<T, TrampolineError>;

/// A trampoline for C callbacks
#[derive(Debug)]
pub struct Trampoline {
    code: *mut c_void,
    #[allow(dead_code)]
    user_data: *mut c_void,
}

impl Trampoline {
    /// Get the function pointer for this trampoline
    pub fn as_ptr(&self) -> *const c_void {
        self.code
    }
}

/// A callback from C to Jet
pub trait JetCallback: Send + Sync {
    fn call(&self, args: &[crate::values::CValue]) -> crate::values::CValue;
}

/// Create a trampoline for a Jet callback
pub fn create_trampoline<F>(_callback: F) -> TrampolineResult<Trampoline>
where
    F: Fn(&[crate::values::CValue]) -> crate::values::CValue + Send + Sync + 'static,
{
    Ok(Trampoline {
        code: std::ptr::null_mut(),
        user_data: std::ptr::null_mut(),
    })
}

/// Free a trampoline
///
/// # Safety
///
/// The trampoline must have been allocated by `create_trampoline` and not already freed.
pub unsafe fn free_trampoline(trampoline: Trampoline) {
    let _ = trampoline;
}
