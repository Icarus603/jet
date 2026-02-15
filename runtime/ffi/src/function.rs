//! C Function Calling
//!
//! This module provides functionality for calling C functions from Jet.
//! It handles argument marshalling, calling convention selection, and
//! return value handling.

use std::ffi::c_void;
use std::mem;
use std::ptr;

use crate::types::{CType, CTypeKind, CallConv, FunctionSignature};
use crate::values::CValue;

/// Result type for FFI operations
pub type FfiResult<T> = Result<T, FfiCallError>;

/// Errors that can occur during FFI calls
#[derive(Debug, Clone, PartialEq)]
pub enum FfiCallError {
    /// Invalid function pointer
    InvalidFunction,

    /// Type mismatch in arguments
    TypeMismatch {
        expected: String,
        got: String,
        position: usize,
    },

    /// Wrong number of arguments
    ArgumentCount { expected: usize, got: usize },

    /// Null pointer where non-null required
    NullPointer,

    /// Memory allocation failed
    OutOfMemory,

    /// Invalid type for FFI
    InvalidType(String),

    /// Call failed (system error)
    CallFailed(String),

    /// Unsupported calling convention
    UnsupportedCallConv(CallConv),

    /// Marshalling error
    MarshalError(String),
}

impl std::fmt::Display for FfiCallError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FfiCallError::InvalidFunction => write!(f, "invalid function pointer"),
            FfiCallError::TypeMismatch {
                expected,
                got,
                position,
            } => {
                write!(
                    f,
                    "type mismatch at position {}: expected {}, got {}",
                    position, expected, got
                )
            }
            FfiCallError::ArgumentCount { expected, got } => {
                write!(
                    f,
                    "wrong number of arguments: expected {}, got {}",
                    expected, got
                )
            }
            FfiCallError::NullPointer => write!(f, "null pointer where non-null required"),
            FfiCallError::OutOfMemory => write!(f, "out of memory"),
            FfiCallError::InvalidType(ty) => write!(f, "invalid type for FFI: {}", ty),
            FfiCallError::CallFailed(msg) => write!(f, "call failed: {}", msg),
            FfiCallError::UnsupportedCallConv(conv) => {
                write!(f, "unsupported calling convention: {:?}", conv)
            }
            FfiCallError::MarshalError(msg) => write!(f, "marshalling error: {}", msg),
        }
    }
}

impl std::error::Error for FfiCallError {}

/// A callable C function
///
/// This struct wraps a C function pointer and its signature,
/// providing a safe interface for calling the function.
pub struct CFunction {
    /// The function pointer
    ptr: *const c_void,
    /// The function signature
    signature: FunctionSignature,
}

// CFunction is Send + Sync because it just contains a function pointer
// and type information, which are immutable
unsafe impl Send for CFunction {}
unsafe impl Sync for CFunction {}

impl CFunction {
    /// Create a new C function wrapper
    ///
    /// # Safety
    ///
    /// The pointer must be a valid C function with the given signature.
    /// The signature must accurately describe the function's parameter
    /// and return types.
    pub unsafe fn new(ptr: *const c_void, signature: FunctionSignature) -> Self {
        Self { ptr, signature }
    }

    /// Create a new C function from a raw pointer and type
    ///
    /// # Safety
    ///
    /// The pointer must be a valid C function with the given type signature.
    pub unsafe fn from_type(ptr: *const c_void, ty: CType) -> FfiResult<Self> {
        match ty.kind() {
            CTypeKind::Function {
                return_type,
                params,
            } => {
                let signature = FunctionSignature::new((**return_type).clone(), params.clone());
                Ok(Self::new(ptr, signature))
            }
            _ => Err(FfiCallError::InvalidType(
                "expected function type".to_string(),
            )),
        }
    }

    /// Get the function pointer
    pub fn ptr(&self) -> *const c_void {
        self.ptr
    }

    /// Get the function signature
    pub fn signature(&self) -> &FunctionSignature {
        &self.signature
    }

    /// Get the return type
    pub fn return_type(&self) -> &CType {
        &self.signature.return_type
    }

    /// Get the parameter types
    pub fn param_types(&self) -> &[CType] {
        &self.signature.params
    }

    /// Check if the function is variadic
    pub fn is_variadic(&self) -> bool {
        self.signature.is_variadic
    }

    /// Call the C function with the given arguments
    ///
    /// This method marshals the arguments, calls the function, and
    /// unmarshals the return value.
    ///
    /// # Safety
    ///
    /// This is unsafe because:
    /// - The function pointer must be valid
    /// - The arguments must be valid for the function
    /// - The function may have side effects
    /// - The function may not be thread-safe
    pub unsafe fn call(&self, args: &[CValue]) -> FfiResult<CValue> {
        // Validate argument count
        if !self.signature.is_variadic && args.len() != self.signature.params.len() {
            return Err(FfiCallError::ArgumentCount {
                expected: self.signature.params.len(),
                got: args.len(),
            });
        }

        if self.signature.is_variadic && args.len() < self.signature.params.len() {
            return Err(FfiCallError::ArgumentCount {
                expected: self.signature.params.len(),
                got: args.len(),
            });
        }

        // Validate argument types
        for (i, (arg, expected)) in args.iter().zip(self.signature.params.iter()).enumerate() {
            let arg_type = arg.get_type();
            if !types_compatible(&arg_type, expected) {
                return Err(FfiCallError::TypeMismatch {
                    expected: format!("{}", expected),
                    got: format!("{}", arg_type),
                    position: i,
                });
            }
        }

        // Perform the call based on the calling convention
        self.call_with_conv(args)
    }

    /// Call with a specific calling convention
    unsafe fn call_with_conv(&self, args: &[CValue]) -> FfiResult<CValue> {
        // For now, we implement a simplified calling mechanism
        // In a full implementation, this would use libffi or platform-specific assembly

        match self.signature.call_conv {
            CallConv::C | CallConv::SystemV | CallConv::WindowsX64 => self.call_cdecl(args),
            _ => Err(FfiCallError::UnsupportedCallConv(self.signature.call_conv)),
        }
    }

    /// Call using the C calling convention
    unsafe fn call_cdecl(&self, args: &[CValue]) -> FfiResult<CValue> {
        // This is a simplified implementation
        // A real implementation would use libffi or inline assembly

        // Convert arguments to their C representations
        let mut arg_ptrs: Vec<*const c_void> = Vec::with_capacity(args.len());
        let mut arg_storage: Vec<Vec<u8>> = Vec::with_capacity(args.len());

        for arg in args {
            let bytes = arg.to_bytes();
            arg_storage.push(bytes);
        }

        for storage in &arg_storage {
            if storage.is_empty() {
                arg_ptrs.push(ptr::null());
            } else {
                arg_ptrs.push(storage.as_ptr() as *const c_void);
            }
        }

        // For primitive return types, we can use a generic approach
        let result = match self.signature.return_type.kind() {
            CTypeKind::Void => {
                // Call void function
                let fn_ptr: unsafe extern "C" fn() = mem::transmute(self.ptr);
                fn_ptr();
                CValue::Void
            }
            CTypeKind::Int32 => {
                type FnType = unsafe extern "C" fn() -> i32;
                let fn_ptr: FnType = mem::transmute(self.ptr);

                // This is a placeholder - real implementation would properly pass args
                let result = fn_ptr();
                CValue::Int32(result)
            }
            CTypeKind::Int64 => {
                type FnType = unsafe extern "C" fn() -> i64;
                let fn_ptr: FnType = mem::transmute(self.ptr);
                let result = fn_ptr();
                CValue::Int64(result)
            }
            CTypeKind::Pointer { pointee } => {
                type FnType = unsafe extern "C" fn() -> *mut c_void;
                let fn_ptr: FnType = mem::transmute(self.ptr);
                let result = fn_ptr();
                CValue::Pointer {
                    ptr: result,
                    pointee_type: (**pointee).clone(),
                }
            }
            _ => {
                return Err(FfiCallError::UnsupportedCallConv(self.signature.call_conv));
            }
        };

        Ok(result)
    }

    /// Call the function and return the raw result bytes
    ///
    /// # Safety
    ///
    /// The caller is responsible for interpreting the bytes correctly.
    pub unsafe fn call_raw(&self, args: &[CValue]) -> FfiResult<Vec<u8>> {
        let result = self.call(args)?;
        Ok(result.to_bytes())
    }
}

impl std::fmt::Debug for CFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CFunction")
            .field("ptr", &self.ptr)
            .field("signature", &self.signature)
            .finish()
    }
}

/// Check if two types are compatible for FFI calls
fn types_compatible(a: &CType, b: &CType) -> bool {
    // For now, require exact match
    // In a more sophisticated implementation, we could allow
    // implicit conversions (e.g., int32 -> int64)
    a == b
}

/// Builder for creating C function signatures
pub struct FunctionBuilder {
    return_type: CType,
    params: Vec<CType>,
    call_conv: CallConv,
    is_variadic: bool,
}

impl FunctionBuilder {
    /// Create a new function builder
    pub fn new(return_type: CType) -> Self {
        Self {
            return_type,
            params: Vec::new(),
            call_conv: CallConv::C,
            is_variadic: false,
        }
    }

    /// Add a parameter
    pub fn param(mut self, ty: CType) -> Self {
        self.params.push(ty);
        self
    }

    /// Add multiple parameters
    pub fn params(mut self, types: Vec<CType>) -> Self {
        self.params.extend(types);
        self
    }

    /// Set the calling convention
    pub fn call_conv(mut self, conv: CallConv) -> Self {
        self.call_conv = conv;
        self
    }

    /// Make the function variadic
    pub fn variadic(mut self) -> Self {
        self.is_variadic = true;
        self
    }

    /// Build the function signature
    pub fn build(self) -> FunctionSignature {
        FunctionSignature {
            return_type: self.return_type,
            params: self.params,
            call_conv: self.call_conv,
            is_variadic: self.is_variadic,
        }
    }

    /// Build and create a CFunction from a pointer
    ///
    /// # Safety
    ///
    /// The pointer must be a valid C function matching the signature.
    pub unsafe fn build_function(self, ptr: *const c_void) -> CFunction {
        CFunction::new(ptr, self.build())
    }
}

/// Call a C function pointer directly with variadic arguments
///
/// # Safety
///
/// This is highly unsafe and should be used with extreme care.
/// The types must match the actual C function signature exactly.
pub unsafe fn call_c_fn(
    ptr: *const c_void,
    return_type: &CType,
    args: &[CValue],
) -> FfiResult<CValue> {
    let signature = FunctionSignature::new(
        return_type.clone(),
        args.iter().map(|a| a.get_type()).collect(),
    );
    let func = CFunction::new(ptr, signature);
    func.call(args)
}

/// A registry of named C functions
#[derive(Debug, Default)]
pub struct FunctionRegistry {
    functions: std::collections::HashMap<String, CFunction>,
}

impl FunctionRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            functions: std::collections::HashMap::new(),
        }
    }

    /// Register a function
    ///
    /// # Safety
    ///
    /// The function pointer must be valid and match the signature.
    pub unsafe fn register(
        &mut self,
        name: impl Into<String>,
        ptr: *const c_void,
        signature: FunctionSignature,
    ) {
        self.functions
            .insert(name.into(), CFunction::new(ptr, signature));
    }

    /// Get a registered function
    pub fn get(&self, name: &str) -> Option<&CFunction> {
        self.functions.get(name)
    }

    /// Check if a function is registered
    pub fn contains(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    /// Remove a function from the registry
    pub fn unregister(&mut self, name: &str) -> Option<CFunction> {
        self.functions.remove(name)
    }

    /// Get the number of registered functions
    pub fn len(&self) -> usize {
        self.functions.len()
    }

    /// Check if the registry is empty
    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }

    /// Clear all registered functions
    pub fn clear(&mut self) {
        self.functions.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_builder() {
        let sig = FunctionBuilder::new(CType::int32())
            .param(CType::int32())
            .param(CType::int32())
            .build();

        assert_eq!(sig.return_type, CType::int32());
        assert_eq!(sig.params.len(), 2);
        assert!(!sig.is_variadic);
    }

    #[test]
    fn test_variadic_function() {
        let sig = FunctionBuilder::new(CType::int32())
            .param(CType::c_string())
            .variadic()
            .build();

        assert!(sig.is_variadic);
        assert_eq!(sig.params.len(), 1);
    }

    #[test]
    fn test_function_signature() {
        let sig = FunctionSignature::new(CType::int32(), vec![CType::int32(), CType::int32()]);

        assert_eq!(sig.return_type, CType::int32());
        assert_eq!(sig.params.len(), 2);
    }

    #[test]
    fn test_function_registry() {
        let mut registry = FunctionRegistry::new();

        // Register a dummy function
        let dummy_fn = 0x1234 as *const c_void;
        let sig = FunctionSignature::new(CType::int32(), vec![]);

        unsafe {
            registry.register("test", dummy_fn, sig);
        }

        assert!(registry.contains("test"));
        assert_eq!(registry.len(), 1);

        let func = registry.get("test").unwrap();
        assert_eq!(func.ptr(), dummy_fn);

        registry.unregister("test");
        assert!(!registry.contains("test"));
    }

    #[test]
    fn test_call_error_display() {
        let err = FfiCallError::TypeMismatch {
            expected: "int32".to_string(),
            got: "int64".to_string(),
            position: 0,
        };
        assert!(err.to_string().contains("type mismatch"));

        let err = FfiCallError::ArgumentCount {
            expected: 2,
            got: 1,
        };
        assert!(err.to_string().contains("wrong number of arguments"));
    }
}
