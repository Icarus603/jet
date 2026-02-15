//! C Value Representation
//!
//! This module provides types for representing C values in Rust.
//! These values can be passed to and returned from C functions.

use std::ffi::{c_void, CString};
use std::ptr;

use crate::types::{CType, CTypeKind, Size};

/// A C value that can be passed to or returned from FFI functions
///
/// This enum represents values of various C types. It is used to
/// marshal values between Jet and C representations.
#[derive(Debug, Clone)]
pub enum CValue {
    /// Void (no value)
    Void,

    /// Boolean value
    Bool(bool),

    /// 8-bit signed integer
    Int8(i8),

    /// 16-bit signed integer
    Int16(i16),

    /// 32-bit signed integer
    Int32(i32),

    /// 64-bit signed integer
    Int64(i64),

    /// 8-bit unsigned integer
    UInt8(u8),

    /// 16-bit unsigned integer
    UInt16(u16),

    /// 32-bit unsigned integer
    UInt32(u32),

    /// 64-bit unsigned integer
    UInt64(u64),

    /// 32-bit floating point
    Float(f32),

    /// 64-bit floating point
    Double(f64),

    /// Pointer value (may be null)
    Pointer {
        /// The pointer value
        ptr: *mut c_void,
        /// The type being pointed to
        pointee_type: CType,
    },

    /// Function pointer
    FunctionPtr {
        /// The function pointer
        ptr: *const c_void,
        /// The function signature
        signature: CType,
    },

    /// Struct value (stored as bytes)
    Struct {
        /// The struct type
        ty: CType,
        /// The struct data
        data: Vec<u8>,
    },

    /// Union value
    Union {
        /// The union type
        ty: CType,
        /// Active variant index
        active: usize,
        /// The union data
        data: Vec<u8>,
    },

    /// Array value
    Array {
        /// The element type
        element_type: CType,
        /// Array elements
        elements: Vec<CValue>,
    },

    /// C string (null-terminated)
    CString(CString),
}

impl CValue {
    /// Create a void value
    pub fn void() -> Self {
        CValue::Void
    }

    /// Create a boolean value
    pub fn bool(v: bool) -> Self {
        CValue::Bool(v)
    }

    /// Create an 8-bit signed integer value
    pub fn int8(v: i8) -> Self {
        CValue::Int8(v)
    }

    /// Create a 16-bit signed integer value
    pub fn int16(v: i16) -> Self {
        CValue::Int16(v)
    }

    /// Create a 32-bit signed integer value
    pub fn int32(v: i32) -> Self {
        CValue::Int32(v)
    }

    /// Create a 64-bit signed integer value
    pub fn int64(v: i64) -> Self {
        CValue::Int64(v)
    }

    /// Create an 8-bit unsigned integer value
    pub fn uint8(v: u8) -> Self {
        CValue::UInt8(v)
    }

    /// Create a 16-bit unsigned integer value
    pub fn uint16(v: u16) -> Self {
        CValue::UInt16(v)
    }

    /// Create a 32-bit unsigned integer value
    pub fn uint32(v: u32) -> Self {
        CValue::UInt32(v)
    }

    /// Create a 64-bit unsigned integer value
    pub fn uint64(v: u64) -> Self {
        CValue::UInt64(v)
    }

    /// Create a 32-bit float value
    pub fn float(v: f32) -> Self {
        CValue::Float(v)
    }

    /// Create a 64-bit float value
    pub fn double(v: f64) -> Self {
        CValue::Double(v)
    }

    /// Create a pointer value
    ///
    /// # Safety
    ///
    /// The pointer must be valid for the given pointee type.
    pub unsafe fn pointer(ptr: *mut c_void, pointee_type: CType) -> Self {
        CValue::Pointer { ptr, pointee_type }
    }

    /// Create a null pointer of the given type
    pub fn null_pointer(pointee_type: CType) -> Self {
        CValue::Pointer {
            ptr: ptr::null_mut(),
            pointee_type,
        }
    }

    /// Create a function pointer value
    ///
    /// # Safety
    ///
    /// The pointer must point to a valid function with the given signature.
    pub unsafe fn function_ptr(ptr: *const c_void, signature: CType) -> Self {
        CValue::FunctionPtr { ptr, signature }
    }

    /// Create a struct value from raw bytes
    ///
    /// # Safety
    ///
    /// The data must be a valid representation of the struct type.
    pub unsafe fn struct_from_bytes(ty: CType, data: Vec<u8>) -> Self {
        CValue::Struct { ty, data }
    }

    /// Create an array value
    pub fn array(element_type: CType, elements: Vec<CValue>) -> Self {
        CValue::Array {
            element_type,
            elements,
        }
    }

    /// Create a C string value
    pub fn c_string(s: impl Into<Vec<u8>>) -> Option<Self> {
        CString::new(s).ok().map(CValue::CString)
    }

    /// Get the type of this value
    pub fn get_type(&self) -> CType {
        match self {
            CValue::Void => CType::void(),
            CValue::Bool(_) => CType::bool(),
            CValue::Int8(_) => CType::int8(),
            CValue::Int16(_) => CType::int16(),
            CValue::Int32(_) => CType::int32(),
            CValue::Int64(_) => CType::int64(),
            CValue::UInt8(_) => CType::uint8(),
            CValue::UInt16(_) => CType::uint16(),
            CValue::UInt32(_) => CType::uint32(),
            CValue::UInt64(_) => CType::uint64(),
            CValue::Float(_) => CType::float(),
            CValue::Double(_) => CType::double(),
            CValue::Pointer { pointee_type, .. } => CType::pointer(pointee_type.clone()),
            CValue::FunctionPtr { signature, .. } => signature.clone(),
            CValue::Struct { ty, .. } => ty.clone(),
            CValue::Union { ty, .. } => ty.clone(),
            CValue::Array {
                element_type,
                elements,
            } => CType::array(element_type.clone(), elements.len()),
            CValue::CString(_) => CType::c_string(),
        }
    }

    /// Get the size of this value in bytes
    pub fn size(&self) -> Size {
        self.get_type().size()
    }

    /// Check if this value is void
    pub fn is_void(&self) -> bool {
        matches!(self, CValue::Void)
    }

    /// Check if this value is a null pointer
    pub fn is_null(&self) -> bool {
        match self {
            CValue::Pointer { ptr, .. } => ptr.is_null(),
            _ => false,
        }
    }

    /// Get the pointer value if this is a pointer
    pub fn as_pointer(&self) -> Option<*mut c_void> {
        match self {
            CValue::Pointer { ptr, .. } => Some(*ptr),
            _ => None,
        }
    }

    /// Get the integer value as i64
    pub fn as_i64(&self) -> Option<i64> {
        match *self {
            CValue::Int8(v) => Some(v as i64),
            CValue::Int16(v) => Some(v as i64),
            CValue::Int32(v) => Some(v as i64),
            CValue::Int64(v) => Some(v),
            CValue::UInt8(v) => Some(v as i64),
            CValue::UInt16(v) => Some(v as i64),
            CValue::UInt32(v) => Some(v as i64),
            CValue::UInt64(v) => Some(v as i64),
            _ => None,
        }
    }

    /// Get the unsigned integer value as u64
    pub fn as_u64(&self) -> Option<u64> {
        match *self {
            CValue::Int8(v) => Some(v as u64),
            CValue::Int16(v) => Some(v as u64),
            CValue::Int32(v) => Some(v as u64),
            CValue::Int64(v) => Some(v as u64),
            CValue::UInt8(v) => Some(v as u64),
            CValue::UInt16(v) => Some(v as u64),
            CValue::UInt32(v) => Some(v as u64),
            CValue::UInt64(v) => Some(v),
            _ => None,
        }
    }

    /// Get the float value as f64
    pub fn as_f64(&self) -> Option<f64> {
        match *self {
            CValue::Float(v) => Some(v as f64),
            CValue::Double(v) => Some(v),
            _ => None,
        }
    }

    /// Get the boolean value
    pub fn as_bool(&self) -> Option<bool> {
        match *self {
            CValue::Bool(v) => Some(v),
            _ => None,
        }
    }

    /// Convert this value to raw bytes for FFI calls
    ///
    /// Returns a vector of bytes that can be passed to C functions.
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            CValue::Void => vec![],
            CValue::Bool(v) => vec![*v as u8],
            CValue::Int8(v) => vec![*v as u8],
            CValue::Int16(v) => v.to_ne_bytes().to_vec(),
            CValue::Int32(v) => v.to_ne_bytes().to_vec(),
            CValue::Int64(v) => v.to_ne_bytes().to_vec(),
            CValue::UInt8(v) => vec![*v],
            CValue::UInt16(v) => v.to_ne_bytes().to_vec(),
            CValue::UInt32(v) => v.to_ne_bytes().to_vec(),
            CValue::UInt64(v) => v.to_ne_bytes().to_vec(),
            CValue::Float(v) => v.to_ne_bytes().to_vec(),
            CValue::Double(v) => v.to_ne_bytes().to_vec(),
            CValue::Pointer { ptr, .. } => (*ptr as usize).to_ne_bytes().to_vec(),
            CValue::FunctionPtr { ptr, .. } => (*ptr as usize).to_ne_bytes().to_vec(),
            CValue::Struct { data, .. } => data.clone(),
            CValue::Union { data, .. } => data.clone(),
            CValue::Array { elements, .. } => {
                let mut result = Vec::new();
                for elem in elements {
                    result.extend_from_slice(&elem.to_bytes());
                }
                result
            }
            CValue::CString(s) => {
                // Return the pointer to the string
                (s.as_ptr() as usize).to_ne_bytes().to_vec()
            }
        }
    }

    /// Create a CValue from raw bytes and a type
    ///
    /// # Safety
    ///
    /// The bytes must be a valid representation of the given type.
    pub unsafe fn from_bytes(bytes: &[u8], ty: &CType) -> Option<Self> {
        if bytes.len() < ty.size() {
            return None;
        }

        match ty.kind() {
            CTypeKind::Void => Some(CValue::Void),
            CTypeKind::Bool => Some(CValue::Bool(bytes[0] != 0)),
            CTypeKind::Int8 => Some(CValue::Int8(bytes[0] as i8)),
            CTypeKind::Int16 => {
                let bytes_array: [u8; 2] = [bytes[0], bytes[1]];
                Some(CValue::Int16(i16::from_ne_bytes(bytes_array)))
            }
            CTypeKind::Int32 => {
                let bytes_array: [u8; 4] = [bytes[0], bytes[1], bytes[2], bytes[3]];
                Some(CValue::Int32(i32::from_ne_bytes(bytes_array)))
            }
            CTypeKind::Int64 => {
                let bytes_array: [u8; 8] = bytes[0..8].try_into().ok()?;
                Some(CValue::Int64(i64::from_ne_bytes(bytes_array)))
            }
            CTypeKind::UInt8 => Some(CValue::UInt8(bytes[0])),
            CTypeKind::UInt16 => {
                let bytes_array: [u8; 2] = [bytes[0], bytes[1]];
                Some(CValue::UInt16(u16::from_ne_bytes(bytes_array)))
            }
            CTypeKind::UInt32 => {
                let bytes_array: [u8; 4] = [bytes[0], bytes[1], bytes[2], bytes[3]];
                Some(CValue::UInt32(u32::from_ne_bytes(bytes_array)))
            }
            CTypeKind::UInt64 => {
                let bytes_array: [u8; 8] = bytes[0..8].try_into().ok()?;
                Some(CValue::UInt64(u64::from_ne_bytes(bytes_array)))
            }
            CTypeKind::Float => {
                let bytes_array: [u8; 4] = [bytes[0], bytes[1], bytes[2], bytes[3]];
                Some(CValue::Float(f32::from_ne_bytes(bytes_array)))
            }
            CTypeKind::Double => {
                let bytes_array: [u8; 8] = bytes[0..8].try_into().ok()?;
                Some(CValue::Double(f64::from_ne_bytes(bytes_array)))
            }
            CTypeKind::Pointer { pointee } => {
                let bytes_array: [u8; 8] = bytes[0..8].try_into().ok()?;
                let ptr = usize::from_ne_bytes(bytes_array) as *mut c_void;
                Some(CValue::Pointer {
                    ptr,
                    pointee_type: (**pointee).clone(),
                })
            }
            _ => None, // Complex types not supported in from_bytes
        }
    }
}

/// An owned C value that manages its own memory
///
/// This type is similar to `CValue` but owns any heap-allocated data
/// and ensures proper cleanup when dropped.
pub struct OwnedCValue {
    value: CValue,
    owned_data: Vec<Vec<u8>>,
}

impl OwnedCValue {
    /// Create a new owned value from a CValue
    pub fn new(value: CValue) -> Self {
        Self {
            value,
            owned_data: Vec::new(),
        }
    }

    /// Create an owned pointer value with allocated memory
    pub fn owned_pointer(data: Vec<u8>, pointee_type: CType) -> Self {
        let ptr = data.as_ptr() as *mut c_void;
        let value = CValue::Pointer { ptr, pointee_type };
        Self {
            value,
            owned_data: vec![data],
        }
    }

    /// Get a reference to the underlying value
    pub fn value(&self) -> &CValue {
        &self.value
    }

    /// Convert to a CValue (does not transfer ownership)
    pub fn as_cvalue(&self) -> CValue {
        self.value.clone()
    }

    /// Get the type of this value
    pub fn get_type(&self) -> CType {
        self.value.get_type()
    }
}

impl Clone for OwnedCValue {
    fn clone(&self) -> Self {
        // Deep clone - allocate new memory for owned data
        let mut new_owned = Vec::with_capacity(self.owned_data.len());
        for data in &self.owned_data {
            new_owned.push(data.clone());
        }

        // Update pointer if needed
        let value = match &self.value {
            CValue::Pointer {
                ptr: _,
                pointee_type,
            } => {
                if !new_owned.is_empty() {
                    CValue::Pointer {
                        ptr: new_owned[0].as_ptr() as *mut c_void,
                        pointee_type: pointee_type.clone(),
                    }
                } else {
                    self.value.clone()
                }
            }
            _ => self.value.clone(),
        };

        Self {
            value,
            owned_data: new_owned,
        }
    }
}

/// Inner representation of a C value for low-level access
pub union CValueInner {
    pub void: (),
    pub bool: bool,
    pub int8: i8,
    pub int16: i16,
    pub int32: i32,
    pub int64: i64,
    pub uint8: u8,
    pub uint16: u16,
    pub uint32: u32,
    pub uint64: u64,
    pub float: f32,
    pub double: f64,
    pub pointer: *mut c_void,
    pub function_ptr: *const c_void,
}

impl CValueInner {
    /// Create a new inner value from a CValue
    pub fn from_cvalue(value: &CValue) -> Self {
        match value {
            CValue::Void => Self { void: () },
            CValue::Bool(v) => Self { bool: *v },
            CValue::Int8(v) => Self { int8: *v },
            CValue::Int16(v) => Self { int16: *v },
            CValue::Int32(v) => Self { int32: *v },
            CValue::Int64(v) => Self { int64: *v },
            CValue::UInt8(v) => Self { uint8: *v },
            CValue::UInt16(v) => Self { uint16: *v },
            CValue::UInt32(v) => Self { uint32: *v },
            CValue::UInt64(v) => Self { uint64: *v },
            CValue::Float(v) => Self { float: *v },
            CValue::Double(v) => Self { double: *v },
            CValue::Pointer { ptr, .. } => Self { pointer: *ptr },
            CValue::FunctionPtr { ptr, .. } => Self { function_ptr: *ptr },
            _ => Self { void: () },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_values() {
        assert_eq!(CValue::int8(42).as_i64(), Some(42));
        assert_eq!(CValue::int16(1000).as_i64(), Some(1000));
        assert_eq!(CValue::int32(100000).as_i64(), Some(100000));
        assert_eq!(CValue::int64(10000000000).as_i64(), Some(10000000000));

        assert_eq!(CValue::uint8(42).as_u64(), Some(42));
        assert_eq!(CValue::uint16(1000).as_u64(), Some(1000));
        assert_eq!(CValue::uint32(100000).as_u64(), Some(100000));
        assert_eq!(CValue::uint64(10000000000).as_u64(), Some(10000000000));
    }

    #[test]
    fn test_float_values() {
        assert!(
            (CValue::float(std::f32::consts::PI).as_f64().unwrap() - std::f64::consts::PI).abs()
                < 0.001
        );
        assert!(
            (CValue::double(std::f64::consts::PI).as_f64().unwrap() - std::f64::consts::PI).abs()
                < 0.00001
        );
    }

    #[test]
    fn test_bool_value() {
        assert_eq!(CValue::bool(true).as_bool(), Some(true));
        assert_eq!(CValue::bool(false).as_bool(), Some(false));
    }

    #[test]
    fn test_pointer_value() {
        let ptr_value: *mut i32 = &mut 42;
        let cval = unsafe { CValue::pointer(ptr_value as *mut c_void, CType::int32()) };
        assert!(!cval.is_null());
        assert_eq!(cval.as_pointer(), Some(ptr_value as *mut c_void));
    }

    #[test]
    fn test_null_pointer() {
        let null = CValue::null_pointer(CType::int32());
        assert!(null.is_null());
        assert_eq!(null.as_pointer(), Some(std::ptr::null_mut()));
    }

    #[test]
    fn test_c_string() {
        let cstr = CValue::c_string("hello").unwrap();
        assert_eq!(cstr.get_type(), CType::c_string());
    }

    #[test]
    fn test_to_bytes() {
        assert_eq!(CValue::int8(42).to_bytes(), vec![42]);
        assert_eq!(CValue::int16(0x1234).to_bytes(), 0x1234i16.to_ne_bytes());
        assert_eq!(
            CValue::int32(0x12345678).to_bytes(),
            0x12345678i32.to_ne_bytes()
        );
    }

    #[test]
    fn test_void_value() {
        assert!(CValue::void().is_void());
        assert!(CValue::void().to_bytes().is_empty());
    }

    #[test]
    fn test_array_value() {
        let arr = CValue::array(
            CType::int32(),
            vec![CValue::int32(1), CValue::int32(2), CValue::int32(3)],
        );
        assert_eq!(arr.get_type(), CType::array(CType::int32(), 3));
    }

    #[test]
    fn test_owned_value() {
        let data = vec![1, 2, 3, 4];
        let owned = OwnedCValue::owned_pointer(data, CType::int32());
        assert_eq!(owned.get_type(), CType::pointer(CType::int32()));
        assert!(!owned.value().is_null());
    }

    #[test]
    fn test_cvalue_inner() {
        let inner = CValueInner::from_cvalue(&CValue::int32(42));
        unsafe {
            assert_eq!(inner.int32, 42);
        }
    }
}
