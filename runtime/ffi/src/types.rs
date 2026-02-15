//! C Type Definitions
//!
//! This module defines C-compatible types for FFI. These types represent
//! the full range of C types that can be used in FFI declarations.

use std::fmt;

/// Size of a type in bytes
pub type Size = usize;

/// Alignment of a type in bytes
pub type Alignment = usize;

/// A C type descriptor
///
/// This enum represents all possible C types that can be used in FFI.
/// It includes primitive types, pointers, arrays, structs, unions, and functions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CType {
    pub kind: CTypeKind,
}

impl CType {
    /// Create a new C type from a kind
    pub fn new(kind: CTypeKind) -> Self {
        Self { kind }
    }

    /// Get the kind of this type
    pub fn kind(&self) -> &CTypeKind {
        &self.kind
    }

    /// Void type
    pub fn void() -> Self {
        Self::new(CTypeKind::Void)
    }

    /// Boolean type (C99 _Bool)
    pub fn bool() -> Self {
        Self::new(CTypeKind::Bool)
    }

    /// 8-bit signed integer
    pub fn int8() -> Self {
        Self::new(CTypeKind::Int8)
    }

    /// 16-bit signed integer
    pub fn int16() -> Self {
        Self::new(CTypeKind::Int16)
    }

    /// 32-bit signed integer
    pub fn int32() -> Self {
        Self::new(CTypeKind::Int32)
    }

    /// 64-bit signed integer
    pub fn int64() -> Self {
        Self::new(CTypeKind::Int64)
    }

    /// 8-bit unsigned integer
    pub fn uint8() -> Self {
        Self::new(CTypeKind::UInt8)
    }

    /// 16-bit unsigned integer
    pub fn uint16() -> Self {
        Self::new(CTypeKind::UInt16)
    }

    /// 32-bit unsigned integer
    pub fn uint32() -> Self {
        Self::new(CTypeKind::UInt32)
    }

    /// 64-bit unsigned integer
    pub fn uint64() -> Self {
        Self::new(CTypeKind::UInt64)
    }

    /// Platform-sized signed integer (C int)
    pub fn c_int() -> Self {
        Self::new(CTypeKind::Int32)
    }

    /// Platform-sized unsigned integer (C unsigned int)
    pub fn c_uint() -> Self {
        Self::new(CTypeKind::UInt32)
    }

    /// C long type (platform-dependent, typically 32 or 64 bits)
    pub fn c_long() -> Self {
        // On most 64-bit systems, long is 64 bits on Unix, 32 bits on Windows
        // We'll use 64 bits as the more common case
        Self::new(CTypeKind::Int64)
    }

    /// C unsigned long type
    pub fn c_ulong() -> Self {
        Self::new(CTypeKind::UInt64)
    }

    /// C size_t type (platform-dependent pointer-sized unsigned)
    pub fn size_t() -> Self {
        Self::new(CTypeKind::UInt64)
    }

    /// C ssize_t type (platform-dependent pointer-sized signed)
    pub fn ssize_t() -> Self {
        Self::new(CTypeKind::Int64)
    }

    /// 32-bit floating point
    pub fn float() -> Self {
        Self::new(CTypeKind::Float)
    }

    /// 64-bit floating point
    pub fn double() -> Self {
        Self::new(CTypeKind::Double)
    }

    /// Pointer to another type
    pub fn pointer(pointee: CType) -> Self {
        Self::new(CTypeKind::Pointer {
            pointee: Box::new(pointee),
        })
    }

    /// Mutable pointer to another type (alias for pointer)
    pub fn mut_pointer(pointee: CType) -> Self {
        Self::pointer(pointee)
    }

    /// Const pointer to another type
    pub fn const_pointer(pointee: CType) -> Self {
        // In C, const is part of the type system but for FFI purposes
        // we treat const and non-const pointers the same
        Self::pointer(pointee)
    }

    /// Pointer to void (void*)
    pub fn void_pointer() -> Self {
        Self::pointer(CType::void())
    }

    /// C string type (char*)
    pub fn c_string() -> Self {
        Self::pointer(CType::int8())
    }

    /// Array type with fixed size
    pub fn array(element: CType, count: usize) -> Self {
        Self::new(CTypeKind::Array {
            element: Box::new(element),
            count,
        })
    }

    /// Struct type
    pub fn struct_type(name: impl Into<String>, fields: Vec<StructField>) -> Self {
        let layout = StructLayout::calculate(&fields);
        Self::new(CTypeKind::Struct {
            name: name.into(),
            fields,
            size: layout.size,
            alignment: layout.alignment,
        })
    }

    /// Union type
    pub fn union_type(name: impl Into<String>, variants: Vec<CType>) -> Self {
        let max_size = variants.iter().map(|v| v.size()).max().unwrap_or(0);
        let max_align = variants.iter().map(|v| v.alignment()).max().unwrap_or(1);
        let size = super::align_up(max_size, max_align);

        Self::new(CTypeKind::Union {
            name: name.into(),
            variants,
            size,
            alignment: max_align,
        })
    }

    /// Function pointer type
    pub fn function(return_type: CType, params: Vec<CType>) -> Self {
        Self::new(CTypeKind::Function {
            return_type: Box::new(return_type),
            params,
        })
    }

    /// Function with varargs
    pub fn variadic_function(return_type: CType, params: Vec<CType>) -> Self {
        Self::new(CTypeKind::Function {
            return_type: Box::new(return_type),
            params,
        })
    }

    /// Opaque type (incomplete struct)
    pub fn opaque(_name: impl Into<String>) -> Self {
        Self::new(CTypeKind::Opaque)
    }

    /// Get the size of this type in bytes
    pub fn size(&self) -> Size {
        match &self.kind {
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
            CTypeKind::Pointer { .. } => std::mem::size_of::<*mut ()>(),
            CTypeKind::Function { .. } => std::mem::size_of::<*const ()>(),
            CTypeKind::Struct { size, .. } => *size,
            CTypeKind::Union { size, .. } => *size,
            CTypeKind::Array { element, count } => element.size() * count,
            CTypeKind::Opaque => 0,
        }
    }

    /// Get the alignment of this type in bytes
    pub fn alignment(&self) -> Alignment {
        match &self.kind {
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
            CTypeKind::Pointer { .. } => std::mem::align_of::<*mut ()>(),
            CTypeKind::Function { .. } => std::mem::align_of::<*const ()>(),
            CTypeKind::Struct { alignment, .. } => *alignment,
            CTypeKind::Union { alignment, .. } => *alignment,
            CTypeKind::Array { element, .. } => element.alignment(),
            CTypeKind::Opaque => 1,
        }
    }

    /// Check if this type is a pointer
    pub fn is_pointer(&self) -> bool {
        matches!(self.kind, CTypeKind::Pointer { .. })
    }

    /// Check if this type is a function pointer
    pub fn is_function(&self) -> bool {
        matches!(self.kind, CTypeKind::Function { .. })
    }

    /// Check if this type is a struct
    pub fn is_struct(&self) -> bool {
        matches!(self.kind, CTypeKind::Struct { .. })
    }

    /// Check if this type is an array
    pub fn is_array(&self) -> bool {
        matches!(self.kind, CTypeKind::Array { .. })
    }

    /// Check if this type is void
    pub fn is_void(&self) -> bool {
        matches!(self.kind, CTypeKind::Void)
    }

    /// Get the pointee type if this is a pointer
    pub fn pointee(&self) -> Option<&CType> {
        match &self.kind {
            CTypeKind::Pointer { pointee } => Some(pointee),
            _ => None,
        }
    }

    /// Get the element type if this is an array
    pub fn element_type(&self) -> Option<&CType> {
        match &self.kind {
            CTypeKind::Array { element, .. } => Some(element),
            _ => None,
        }
    }

    /// Get the array length if this is an array
    pub fn array_len(&self) -> Option<usize> {
        match &self.kind {
            CTypeKind::Array { count, .. } => Some(*count),
            _ => None,
        }
    }

    /// Get the return type if this is a function
    pub fn return_type(&self) -> Option<&CType> {
        match &self.kind {
            CTypeKind::Function { return_type, .. } => Some(return_type),
            _ => None,
        }
    }

    /// Get the parameter types if this is a function
    pub fn param_types(&self) -> Option<&[CType]> {
        match &self.kind {
            CTypeKind::Function { params, .. } => Some(params),
            _ => None,
        }
    }

    /// Get the struct fields if this is a struct
    pub fn struct_fields(&self) -> Option<&[StructField]> {
        match &self.kind {
            CTypeKind::Struct { fields, .. } => Some(fields),
            _ => None,
        }
    }
}

impl fmt::Display for CType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            CTypeKind::Void => write!(f, "void"),
            CTypeKind::Bool => write!(f, "_Bool"),
            CTypeKind::Int8 => write!(f, "int8_t"),
            CTypeKind::Int16 => write!(f, "int16_t"),
            CTypeKind::Int32 => write!(f, "int32_t"),
            CTypeKind::Int64 => write!(f, "int64_t"),
            CTypeKind::UInt8 => write!(f, "uint8_t"),
            CTypeKind::UInt16 => write!(f, "uint16_t"),
            CTypeKind::UInt32 => write!(f, "uint32_t"),
            CTypeKind::UInt64 => write!(f, "uint64_t"),
            CTypeKind::Float => write!(f, "float"),
            CTypeKind::Double => write!(f, "double"),
            CTypeKind::Pointer { pointee } => write!(f, "{}*", pointee),
            CTypeKind::Function {
                return_type,
                params,
            } => {
                write!(f, "{} (*)(", return_type)?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ")")
            }
            CTypeKind::Struct { name, .. } => write!(f, "struct {}", name),
            CTypeKind::Union { name, .. } => write!(f, "union {}", name),
            CTypeKind::Array { element, count } => write!(f, "{}[{}]", element, count),
            CTypeKind::Opaque => write!(f, "<opaque>"),
        }
    }
}

/// The kind of a C type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CTypeKind {
    /// Void type
    Void,

    /// Boolean type (_Bool in C99)
    Bool,

    /// 8-bit signed integer
    Int8,

    /// 16-bit signed integer
    Int16,

    /// 32-bit signed integer
    Int32,

    /// 64-bit signed integer
    Int64,

    /// 8-bit unsigned integer
    UInt8,

    /// 16-bit unsigned integer
    UInt16,

    /// 32-bit unsigned integer
    UInt32,

    /// 64-bit unsigned integer
    UInt64,

    /// 32-bit IEEE 754 float
    Float,

    /// 64-bit IEEE 754 double
    Double,

    /// Pointer to another type
    Pointer {
        /// The type being pointed to
        pointee: Box<CType>,
    },

    /// Function pointer type
    Function {
        /// Return type
        return_type: Box<CType>,
        /// Parameter types
        params: Vec<CType>,
    },

    /// Struct type
    Struct {
        /// Struct name
        name: String,
        /// Struct fields
        fields: Vec<StructField>,
        /// Total size in bytes
        size: Size,
        /// Alignment in bytes
        alignment: Alignment,
    },

    /// Union type
    Union {
        /// Union name
        name: String,
        /// Union variants
        variants: Vec<CType>,
        /// Total size in bytes (max of variants, aligned)
        size: Size,
        /// Alignment in bytes (max of variants)
        alignment: Alignment,
    },

    /// Array type with fixed size
    Array {
        /// Element type
        element: Box<CType>,
        /// Number of elements
        count: usize,
    },

    /// Opaque/incomplete type
    Opaque,
}

/// A field in a struct
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    /// Field name
    pub name: String,
    /// Field type
    pub ty: CType,
    /// Byte offset within the struct
    pub offset: Size,
}

impl StructField {
    /// Create a new struct field
    pub fn new(name: impl Into<String>, ty: CType) -> Self {
        Self {
            name: name.into(),
            ty,
            offset: 0, // Will be calculated by layout
        }
    }

    /// Create a new struct field with a specific offset
    pub fn with_offset(name: impl Into<String>, ty: CType, offset: Size) -> Self {
        Self {
            name: name.into(),
            ty,
            offset,
        }
    }

    /// Get the field name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the field type
    pub fn ty(&self) -> &CType {
        &self.ty
    }

    /// Get the field offset
    pub fn offset(&self) -> Size {
        self.offset
    }

    /// Get the field size
    pub fn size(&self) -> Size {
        self.ty.size()
    }

    /// Get the field alignment
    pub fn alignment(&self) -> Alignment {
        self.ty.alignment()
    }
}

/// Layout information for a struct
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructLayout {
    /// Total size in bytes
    pub size: Size,
    /// Alignment in bytes
    pub alignment: Alignment,
    /// Padding at the end
    pub tail_padding: Size,
}

impl StructLayout {
    /// Calculate the layout for a set of fields
    pub fn calculate(fields: &[StructField]) -> Self {
        let mut offset = 0;
        let mut max_align = 1;

        for field in fields {
            let align = field.alignment();
            max_align = max_align.max(align);

            // Align the current offset to the field's alignment
            offset = super::align_up(offset, align);
            offset += field.size();
        }

        // Align the total size to the struct's alignment
        let size = super::align_up(offset, max_align);
        let tail_padding = size - offset;

        Self {
            size,
            alignment: max_align,
            tail_padding,
        }
    }

    /// Get the offset of a field by index
    pub fn field_offset(&self, fields: &[StructField], index: usize) -> Option<Size> {
        if index >= fields.len() {
            return None;
        }

        let mut offset = 0;
        for (i, field) in fields.iter().enumerate() {
            if i == index {
                return Some(super::align_up(offset, field.alignment()));
            }
            offset = super::align_up(offset, field.alignment()) + field.size();
        }

        None
    }
}

/// Function signature for FFI calls
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    /// Return type
    pub return_type: CType,
    /// Parameter types
    pub params: Vec<CType>,
    /// Calling convention
    pub call_conv: CallConv,
    /// Whether the function is variadic
    pub is_variadic: bool,
}

impl FunctionSignature {
    /// Create a new function signature
    pub fn new(return_type: CType, params: Vec<CType>) -> Self {
        Self {
            return_type,
            params,
            call_conv: CallConv::platform_default(),
            is_variadic: false,
        }
    }

    /// Create a variadic function signature
    pub fn variadic(return_type: CType, params: Vec<CType>) -> Self {
        Self {
            return_type,
            params,
            call_conv: CallConv::platform_default(),
            is_variadic: true,
        }
    }

    /// Set the calling convention
    pub fn with_call_conv(mut self, call_conv: CallConv) -> Self {
        self.call_conv = call_conv;
        self
    }

    /// Get the total size of all parameters
    pub fn params_size(&self) -> Size {
        self.params.iter().map(|p| p.size()).sum()
    }
}

/// Calling convention for functions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallConv {
    /// C calling convention (cdecl)
    C,

    /// System V AMD64 ABI (Unix x86-64)
    SystemV,

    /// Microsoft x64 calling convention
    WindowsX64,

    /// stdcall (Windows 32-bit)
    Stdcall,

    /// fastcall
    Fastcall,

    /// Thiscall (C++ member functions)
    Thiscall,

    /// Vectorcall (Windows)
    Vectorcall,
}

impl CallConv {
    /// Get the platform default calling convention
    pub fn platform_default() -> Self {
        #[cfg(all(target_arch = "x86_64", target_os = "windows"))]
        return CallConv::WindowsX64;

        #[cfg(all(target_arch = "x86_64", not(target_os = "windows")))]
        return CallConv::SystemV;

        #[cfg(target_arch = "x86")]
        return CallConv::C;

        #[cfg(not(any(target_arch = "x86_64", target_arch = "x86")))]
        return CallConv::C;
    }

    /// Get the name of this calling convention
    pub fn name(&self) -> &'static str {
        match self {
            CallConv::C => "cdecl",
            CallConv::SystemV => "system_v",
            CallConv::WindowsX64 => "windows_x64",
            CallConv::Stdcall => "stdcall",
            CallConv::Fastcall => "fastcall",
            CallConv::Thiscall => "thiscall",
            CallConv::Vectorcall => "vectorcall",
        }
    }
}

/// Offset of a field within a struct
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldOffset {
    /// Byte offset
    pub offset: Size,
    /// Field index
    pub index: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_types() {
        assert_eq!(CType::void().size(), 0);
        assert_eq!(CType::bool().size(), 1);
        assert_eq!(CType::int8().size(), 1);
        assert_eq!(CType::int16().size(), 2);
        assert_eq!(CType::int32().size(), 4);
        assert_eq!(CType::int64().size(), 8);
        assert_eq!(CType::float().size(), 4);
        assert_eq!(CType::double().size(), 8);
    }

    #[test]
    fn test_pointer_type() {
        let ptr = CType::pointer(CType::int32());
        assert!(ptr.is_pointer());
        assert_eq!(ptr.pointee(), Some(&CType::int32()));
        assert_eq!(ptr.size(), std::mem::size_of::<*mut ()>());
    }

    #[test]
    fn test_array_type() {
        let arr = CType::array(CType::int32(), 10);
        assert!(arr.is_array());
        assert_eq!(arr.element_type(), Some(&CType::int32()));
        assert_eq!(arr.array_len(), Some(10));
        assert_eq!(arr.size(), 40);
    }

    #[test]
    fn test_function_type() {
        let func = CType::function(CType::int32(), vec![CType::int32(), CType::int32()]);
        assert!(func.is_function());
        assert_eq!(func.return_type(), Some(&CType::int32()));
        assert_eq!(
            func.param_types(),
            Some(&[CType::int32(), CType::int32()][..])
        );
    }

    #[test]
    fn test_struct_layout() {
        let fields = vec![
            StructField::new("a", CType::int32()),
            StructField::new("b", CType::int64()),
        ];

        let layout = StructLayout::calculate(&fields);
        assert_eq!(layout.alignment, 8);
        assert_eq!(layout.size, 16);

        // Field 'a' at offset 0, 'b' at offset 8 (aligned)
        assert_eq!(layout.field_offset(&fields, 0), Some(0));
        assert_eq!(layout.field_offset(&fields, 1), Some(8));
    }

    #[test]
    fn test_struct_type() {
        let struct_ty = CType::struct_type(
            "Point",
            vec![
                StructField::new("x", CType::int32()),
                StructField::new("y", CType::int32()),
            ],
        );

        assert!(struct_ty.is_struct());
        assert_eq!(struct_ty.size(), 8);

        let fields = struct_ty.struct_fields().unwrap();
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name(), "x");
        assert_eq!(fields[1].name(), "y");
    }

    #[test]
    fn test_union_type() {
        let union_ty = CType::union_type(
            "Value",
            vec![CType::int32(), CType::int64(), CType::double()],
        );

        assert_eq!(union_ty.size(), 8); // max size aligned
        assert_eq!(union_ty.alignment(), 8);
    }

    #[test]
    fn test_function_signature() {
        let sig = FunctionSignature::new(
            CType::int32(),
            vec![CType::int32(), CType::pointer(CType::int8())],
        );

        assert_eq!(sig.return_type, CType::int32());
        assert_eq!(sig.params.len(), 2);
        assert!(!sig.is_variadic);
        assert_eq!(sig.call_conv, CallConv::platform_default());
    }

    #[test]
    fn test_variadic_signature() {
        let sig = FunctionSignature::variadic(CType::int32(), vec![CType::c_string()]);
        assert!(sig.is_variadic);
    }

    #[test]
    fn test_type_display() {
        assert_eq!(format!("{}", CType::int32()), "int32_t");
        assert_eq!(format!("{}", CType::pointer(CType::int32())), "int32_t*");

        let func = CType::function(CType::void(), vec![CType::int32(), CType::int32()]);
        assert_eq!(format!("{}", func), "void (*)(int32_t, int32_t)");
    }

    #[test]
    fn test_nested_pointer() {
        let ptr_ptr = CType::pointer(CType::pointer(CType::int32()));
        assert_eq!(format!("{}", ptr_ptr), "int32_t**");
    }

    #[test]
    fn test_complex_struct() {
        // struct with mixed types and padding
        let struct_ty = CType::struct_type(
            "Mixed",
            vec![
                StructField::new("a", CType::int8()),  // offset 0
                StructField::new("b", CType::int32()), // offset 4 (aligned)
                StructField::new("c", CType::int8()),  // offset 8
                StructField::new("d", CType::int64()), // offset 16 (aligned)
            ],
        );

        assert_eq!(struct_ty.alignment(), 8);
        assert_eq!(struct_ty.size(), 24); // 16 + 8 = 24
    }
}
