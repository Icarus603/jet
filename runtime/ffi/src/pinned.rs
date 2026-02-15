//! Pinned values for FFI

use std::ffi::c_void;
use std::ptr::NonNull;

/// A pinned value that won't move in memory
#[derive(Debug)]
pub struct PinnedValue<T> {
    inner: Box<T>,
}

impl<T> PinnedValue<T> {
    /// Create a new pinned value
    pub fn new(value: T) -> Self {
        Self {
            inner: Box::new(value),
        }
    }

    /// Get a pointer to the pinned value
    pub fn as_ptr(&self) -> *const T {
        self.inner.as_ref()
    }

    /// Get a mutable pointer to the pinned value
    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.inner.as_mut()
    }

    /// Convert to a raw pointer suitable for C
    pub fn as_c_ptr(&self) -> *const c_void {
        self.as_ptr() as *const c_void
    }
}

impl<T> AsRef<T> for PinnedValue<T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T> AsMut<T> for PinnedValue<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

/// Handle to a pinned value
#[derive(Debug, Clone, Copy)]
pub struct PinHandle {
    ptr: NonNull<c_void>,
}

impl PinHandle {
    pub fn new(ptr: NonNull<c_void>) -> Self {
        Self { ptr }
    }

    pub fn as_ptr(&self) -> *mut c_void {
        self.ptr.as_ptr()
    }
}

/// Errors related to pinning
#[derive(Debug, Clone)]
pub enum PinError {
    AlreadyPinned,
    InvalidHandle,
}

impl std::fmt::Display for PinError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AlreadyPinned => write!(f, "value already pinned"),
            Self::InvalidHandle => write!(f, "invalid pin handle"),
        }
    }
}

impl std::error::Error for PinError {}

pub type PinResult<T> = Result<T, PinError>;

/// Registry for managing pinned values
#[derive(Debug, Default)]
pub struct PinRegistry {
    count: usize,
}

impl PinRegistry {
    pub fn new() -> Self {
        Self { count: 0 }
    }

    pub fn register(&mut self) -> PinHandle {
        self.count += 1;
        PinHandle::new(NonNull::dangling())
    }

    pub fn unregister(&mut self, _handle: PinHandle) {
        if self.count > 0 {
            self.count -= 1;
        }
    }
}
