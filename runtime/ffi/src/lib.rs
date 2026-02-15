//! Jet Runtime FFI System
//!
//! This crate provides Foreign Function Interface (FFI) support for the Jet programming language,
//! enabling seamless interoperability with C libraries. It handles:
//!
//! - **C Function Calling**: Call C functions from Jet with automatic type marshalling
//! - **C Struct Definitions**: Define C-compatible structs with proper memory layout
//! - **Memory Layout Compatibility**: Ensure Jet types match C memory layout expectations
//! - **Type Marshalling**: Convert between Jet and C representations
//!
//! # Overview
//!
//! The FFI system is built on top of `libffi` for portable C function invocation.
//! It provides safe wrappers around unsafe operations, with proper memory management
//! and type safety guarantees.
//!
//! # Example
//!
//! ```rust,ignore
//! use jet_rt_ffi::{CFunction, CType, CValue, FfiContext, FunctionSignature};
//!
//! // Define a C function signature
//! let sig = FunctionSignature::new(
//!     CType::int64(),
//!     vec![CType::int64(), CType::int64()]
//! );
//!
//! // Create a callable wrapper around a C function pointer
//! // let c_add = unsafe { CFunction::new(add_ptr, sig) };
//!
//! // Call the C function
//! // let result = c_add.call(&[CValue::int64(5), CValue::int64(3)]);
//! ```

use std::ffi::{c_char, c_void, CStr, CString};
use std::mem;
use std::ptr;

// Re-export core types
pub mod function;
pub mod layout;
pub mod library;
pub mod marshal;
pub mod pinned;
pub mod trampoline;
pub mod types;
pub mod values;

pub use types::{Alignment, CType, CTypeKind, FunctionSignature, Size, StructField, StructLayout};

pub use values::{CValue, CValueInner, OwnedCValue};

pub use function::{CFunction, FfiCallError, FfiResult};

pub use types::CallConv;

pub use marshal::{CToJetMarshaller, JetToCMarshaller, MarshalError, MarshalResult, Marshaller};

pub use layout::{
    calculate_struct_layout, calculate_union_layout, CLayout, FieldOffset, LayoutCalculator,
    LayoutError, LayoutResult,
};

pub use library::{close_library, open_library, Library, LibraryError, LibraryResult, Symbol};

pub use pinned::{PinError, PinHandle, PinRegistry, PinResult, PinnedValue};

pub use trampoline::{
    create_trampoline, JetCallback, Trampoline, TrampolineError, TrampolineResult,
};

/// Version of the FFI crate
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Maximum number of arguments supported for FFI calls
pub const MAX_FFI_ARGS: usize = 64;

/// Context for FFI operations
///
/// Maintains state for FFI calls including error handling and memory management.
#[derive(Debug)]
pub struct FfiContext {
    /// Last error message
    last_error: Option<String>,
    /// Pinned values for this context
    pinned: PinRegistry,
    /// Marshaller for type conversions
    marshaller: Marshaller,
}

impl FfiContext {
    /// Create a new FFI context
    pub fn new() -> Self {
        Self {
            last_error: None,
            pinned: PinRegistry::new(),
            marshaller: Marshaller::new(),
        }
    }

    /// Get the last error message
    pub fn last_error(&self) -> Option<&str> {
        self.last_error.as_deref()
    }

    /// Set the last error message
    pub fn set_error(&mut self, error: impl Into<String>) {
        self.last_error = Some(error.into());
    }

    /// Clear the last error
    pub fn clear_error(&mut self) {
        self.last_error = None;
    }

    /// Get a reference to the pin registry
    pub fn pin_registry(&mut self) -> &mut PinRegistry {
        &mut self.pinned
    }

    /// Get a reference to the marshaller
    pub fn marshaller(&self) -> &Marshaller {
        &self.marshaller
    }

    /// Register a pinned value for the duration of an FFI call
    pub fn register_pin(&mut self) -> PinResult<PinHandle> {
        Ok(self.pinned.register())
    }

    /// Unregister a pinned value
    pub fn unregister_pin(&mut self, handle: PinHandle) {
        self.pinned.unregister(handle);
    }
}

impl Default for FfiContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Initialize the FFI subsystem
///
/// This should be called once at runtime startup before any FFI operations.
pub fn init() {
    // Initialize libffi or other platform-specific FFI support
    // This is a no-op on most platforms but may be required for some
}

/// Shutdown the FFI subsystem
///
/// This should be called at runtime shutdown to ensure proper cleanup.
pub fn shutdown() {
    // Cleanup any global FFI state
}

/// Check if a type is a valid FFI type
///
/// Returns true if the type can be passed across the FFI boundary.
pub fn is_valid_ffi_type(ty: &CType) -> bool {
    match ty.kind() {
        CTypeKind::Void
        | CTypeKind::Bool
        | CTypeKind::Int8
        | CTypeKind::Int16
        | CTypeKind::Int32
        | CTypeKind::Int64
        | CTypeKind::UInt8
        | CTypeKind::UInt16
        | CTypeKind::UInt32
        | CTypeKind::UInt64
        | CTypeKind::Float
        | CTypeKind::Double
        | CTypeKind::Pointer { .. }
        | CTypeKind::Function { .. } => true,
        CTypeKind::Struct { fields, .. } => fields.iter().all(|f| is_valid_ffi_type(f.ty())),
        CTypeKind::Union { variants, .. } => variants.iter().all(is_valid_ffi_type),
        CTypeKind::Array { element, .. } => is_valid_ffi_type(element),
        CTypeKind::Opaque => false,
    }
}

/// Get the size of a type in bytes
pub fn size_of_type(ty: &CType) -> Size {
    match ty.kind() {
        CTypeKind::Void => 0,
        CTypeKind::Bool => 1,
        CTypeKind::Int8 => 1,
        CTypeKind::Int16 => 2,
        CTypeKind::Int32 => 4,
        CTypeKind::Int64 => 8,
        CTypeKind::UInt8 => 1,
        CTypeKind::UInt16 => 2,
        CTypeKind::UInt32 => 4,
        CTypeKind::UInt64 => 8,
        CTypeKind::Float => 4,
        CTypeKind::Double => 8,
        CTypeKind::Pointer { .. } => mem::size_of::<*mut c_void>(),
        CTypeKind::Function { .. } => mem::size_of::<*const c_void>(),
        CTypeKind::Struct { size, .. } => *size,
        CTypeKind::Union { size, .. } => *size,
        CTypeKind::Array { element, count } => size_of_type(element) * count,
        CTypeKind::Opaque => 0,
    }
}

/// Get the alignment of a type in bytes
pub fn align_of_type(ty: &CType) -> Alignment {
    match ty.kind() {
        CTypeKind::Void => 1,
        CTypeKind::Bool => 1,
        CTypeKind::Int8 => 1,
        CTypeKind::Int16 => 2,
        CTypeKind::Int32 => 4,
        CTypeKind::Int64 => 8,
        CTypeKind::UInt8 => 1,
        CTypeKind::UInt16 => 2,
        CTypeKind::UInt32 => 4,
        CTypeKind::UInt64 => 8,
        CTypeKind::Float => 4,
        CTypeKind::Double => 8,
        CTypeKind::Pointer { .. } => mem::align_of::<*mut c_void>(),
        CTypeKind::Function { .. } => mem::align_of::<*const c_void>(),
        CTypeKind::Struct { alignment, .. } => *alignment,
        CTypeKind::Union { alignment, .. } => *alignment,
        CTypeKind::Array { element, .. } => align_of_type(element),
        CTypeKind::Opaque => 1,
    }
}

/// Convert a Jet string to a C string
///
/// # Safety
///
/// The returned CString must not outlive the input string.
pub unsafe fn jet_string_to_c_string(s: &str) -> CString {
    CString::new(s).unwrap_or_default()
}

/// Convert a C string to a Jet string
///
/// # Safety
///
/// The input pointer must point to a valid null-terminated UTF-8 string.
pub unsafe fn c_string_to_jet_string(ptr: *const c_char) -> Option<String> {
    if ptr.is_null() {
        return None;
    }
    CStr::from_ptr(ptr).to_str().ok().map(|s| s.to_string())
}

/// Create a null-terminated C string array from a slice of strings
///
/// # Safety
///
/// The returned vector contains pointers to allocated memory that must be freed
/// by calling `free_c_string_array`.
pub unsafe fn create_c_string_array(strings: &[String]) -> Vec<*mut c_char> {
    let mut result: Vec<*mut c_char> = Vec::with_capacity(strings.len() + 1);

    for s in strings {
        let c_string = CString::new(s.as_str()).unwrap_or_default();
        result.push(c_string.into_raw());
    }

    // Null-terminate the array
    result.push(ptr::null_mut());

    result
}

/// Free a C string array created by `create_c_string_array`
///
/// # Safety
///
/// The input must be a valid pointer to an array created by `create_c_string_array`.
pub unsafe fn free_c_string_array(arr: *mut *mut c_char) {
    if arr.is_null() {
        return;
    }

    let mut i = 0;
    loop {
        let ptr = *arr.add(i);
        if ptr.is_null() {
            break;
        }
        let _ = CString::from_raw(ptr);
        i += 1;
    }
}

/// Align a size up to the given alignment
pub const fn align_up(size: Size, align: Alignment) -> Size {
    if align == 0 {
        return size;
    }
    (size + align - 1) & !(align - 1)
}

/// Check if a pointer is properly aligned for a type
pub fn is_aligned<T>(ptr: *const T) -> bool {
    let align = mem::align_of::<T>();
    (ptr as usize).is_multiple_of(align)
}

/// A scoped guard that ensures cleanup on drop
pub struct FfiGuard<F: FnOnce()> {
    cleanup: Option<F>,
}

impl<F: FnOnce()> FfiGuard<F> {
    /// Create a new guard with the given cleanup function
    pub fn new(cleanup: F) -> Self {
        Self {
            cleanup: Some(cleanup),
        }
    }

    /// Disarm the guard, preventing cleanup from running
    pub fn disarm(mut self) {
        self.cleanup = None;
    }
}

impl<F: FnOnce()> Drop for FfiGuard<F> {
    fn drop(&mut self) {
        if let Some(cleanup) = self.cleanup.take() {
            cleanup();
        }
    }
}

// Helper function for tests
#[allow(dead_code)]
unsafe fn jet_string_to_c_cstring(s: &str) -> CString {
    CString::new(s).unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert!(!VERSION.is_empty());
    }

    #[test]
    fn test_ffi_context() {
        let mut ctx = FfiContext::new();
        assert!(ctx.last_error().is_none());

        ctx.set_error("test error");
        assert_eq!(ctx.last_error(), Some("test error"));

        ctx.clear_error();
        assert!(ctx.last_error().is_none());
    }

    #[test]
    fn test_size_of_type() {
        assert_eq!(size_of_type(&CType::void()), 0);
        assert_eq!(size_of_type(&CType::bool()), 1);
        assert_eq!(size_of_type(&CType::int8()), 1);
        assert_eq!(size_of_type(&CType::int16()), 2);
        assert_eq!(size_of_type(&CType::int32()), 4);
        assert_eq!(size_of_type(&CType::int64()), 8);
        assert_eq!(size_of_type(&CType::float()), 4);
        assert_eq!(size_of_type(&CType::double()), 8);
    }

    #[test]
    fn test_align_of_type() {
        assert_eq!(align_of_type(&CType::void()), 1);
        assert_eq!(align_of_type(&CType::bool()), 1);
        assert_eq!(align_of_type(&CType::int8()), 1);
        assert_eq!(align_of_type(&CType::int16()), 2);
        assert_eq!(align_of_type(&CType::int32()), 4);
        assert_eq!(align_of_type(&CType::int64()), 8);
        assert_eq!(align_of_type(&CType::float()), 4);
        assert_eq!(align_of_type(&CType::double()), 8);
    }

    #[test]
    fn test_align_up() {
        assert_eq!(align_up(0, 8), 0);
        assert_eq!(align_up(1, 8), 8);
        assert_eq!(align_up(7, 8), 8);
        assert_eq!(align_up(8, 8), 8);
        assert_eq!(align_up(9, 8), 16);
    }

    #[test]
    fn test_is_aligned() {
        let ptr = 0x1000 as *const i32;
        assert!(is_aligned(ptr));

        let ptr = 0x1001 as *const i32;
        assert!(!is_aligned(ptr));
    }

    #[test]
    fn test_valid_ffi_types() {
        assert!(is_valid_ffi_type(&CType::int32()));
        assert!(is_valid_ffi_type(&CType::pointer(CType::int32())));
        assert!(is_valid_ffi_type(&CType::function(CType::void(), vec![])));

        let struct_ty = CType::struct_type(
            "Test",
            vec![
                StructField::new("a", CType::int32()),
                StructField::new("b", CType::int64()),
            ],
        );
        assert!(is_valid_ffi_type(&struct_ty));
    }

    #[test]
    fn test_ffi_guard() {
        let mut cleaned = false;
        {
            let _guard = FfiGuard::new(|| {
                cleaned = true;
            });
        }
        assert!(cleaned);
    }

    #[test]
    fn test_ffi_guard_disarm() {
        let mut cleaned = false;
        {
            let guard = FfiGuard::new(|| {
                cleaned = true;
            });
            guard.disarm();
        }
        assert!(!cleaned);
    }

    #[test]
    fn test_c_string_conversion() {
        let original = "hello world";
        let c_string = unsafe { jet_string_to_c_cstring(original) };
        assert_eq!(c_string.to_str().unwrap(), original);
    }
}
