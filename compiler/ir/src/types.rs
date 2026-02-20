//! Type representations for Jet IR.
//!
//! This module defines the type system used in the intermediate representation.
//! Types are kept simple to facilitate easy translation to LLVM.

use std::fmt;

/// A type identifier used for interning types.
pub type TypeId = u32;

/// A type definition in the IR module.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDef {
    /// The name of the type (for named structs/enums).
    pub name: String,
    /// The actual type definition.
    pub kind: TypeKind,
}

/// The kind of a type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// An opaque struct type (forward declaration).
    Opaque,
    /// A struct type with fields.
    Struct(Vec<Ty>),
    /// An enum type with variants.
    Enum(Vec<(String, Vec<Ty>)>),
    /// A type alias.
    Alias(Ty),
}

/// A type in the Jet IR.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    /// Void type (no value).
    Void,
    /// Integer type with specified bit width.
    Int(u32),
    /// Floating point type (32 or 64 bits).
    Float(u32),
    /// Boolean type.
    Bool,
    /// Pointer to another type.
    Ptr(Box<Ty>),
    /// Struct type with field types.
    Struct(Vec<Ty>),
    /// Array type with element type and size.
    Array(Box<Ty>, usize),
    /// Function type with parameter types and return type.
    Function(Vec<Ty>, Box<Ty>),
    /// Generic type that has been monomorphized.
    Generic(String, Vec<Ty>),
    /// Reference to a named type definition.
    Named(String),
    /// Ghost type (erased at runtime, used for verification).
    /// The inner type represents the underlying type for type checking.
    Ghost(Box<Ty>),
}

impl Ty {
    /// Returns the size in bytes for this type (simplified, assumes 64-bit target).
    ///
    /// This is used for memory layout calculations.
    pub fn size_in_bytes(&self) -> usize {
        match self {
            Ty::Void => 0,
            Ty::Int(bits) => (*bits as usize).div_ceil(8),
            Ty::Float(32) => 4,
            Ty::Float(64) => 8,
            Ty::Float(bits) => (*bits as usize).div_ceil(8),
            Ty::Bool => 1,
            Ty::Ptr(_) => 8, // 64-bit pointer
            Ty::Struct(fields) => {
                // Simplified: sum of field sizes without padding
                fields.iter().map(|f| f.size_in_bytes()).sum()
            }
            Ty::Array(elem, count) => elem.size_in_bytes() * count,
            Ty::Function(_, _) => 8, // Function pointer
            Ty::Generic(_, _) => 8,  // Monomorphized to pointer or struct
            Ty::Named(_) => 8,       // Assume pointer-sized until resolved
            Ty::Ghost(_) => 0,       // Ghost types are erased at runtime
        }
    }

    /// Returns true if this type is a pointer type.
    pub fn is_pointer(&self) -> bool {
        matches!(self, Ty::Ptr(_))
    }

    /// Returns true if this type is an integer type.
    pub fn is_integer(&self) -> bool {
        matches!(self, Ty::Int(_))
    }

    /// Returns true if this type is a floating point type.
    pub fn is_float(&self) -> bool {
        matches!(self, Ty::Float(_))
    }

    /// Returns true if this type is void.
    pub fn is_void(&self) -> bool {
        matches!(self, Ty::Void)
    }

    /// Returns the element type if this is a pointer, otherwise None.
    pub fn element_type(&self) -> Option<&Ty> {
        match self {
            Ty::Ptr(elem) => Some(elem),
            _ => None,
        }
    }

    /// Common integer types.
    pub const I8: Ty = Ty::Int(8);
    pub const I16: Ty = Ty::Int(16);
    pub const I32: Ty = Ty::Int(32);
    pub const I64: Ty = Ty::Int(64);
    pub const I128: Ty = Ty::Int(128);

    /// Common float types.
    pub const F32: Ty = Ty::Float(32);
    pub const F64: Ty = Ty::Float(64);
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Void => write!(f, "void"),
            Ty::Int(bits) => write!(f, "i{}", bits),
            Ty::Float(bits) => write!(f, "f{}", bits),
            Ty::Bool => write!(f, "bool"),
            Ty::Ptr(elem) => write!(f, "* {}", elem),
            Ty::Struct(fields) => {
                write!(f, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, "}}")
            }
            Ty::Array(elem, count) => write!(f, "[{} x {}]", elem, count),
            Ty::Function(params, ret) => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", ret)
            }
            Ty::Generic(name, args) => {
                write!(f, "{}<", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ">")
            }
            Ty::Named(name) => write!(f, "{}", name),
            Ty::Ghost(inner) => write!(f, "ghost {}", inner),
        }
    }
}

/// Type interner for efficient type storage and comparison.
#[derive(Debug, Default)]
pub struct TypeInterner {
    types: indexmap::IndexSet<Ty>,
}

impl TypeInterner {
    /// Creates a new empty type interner.
    pub fn new() -> Self {
        Self::default()
    }

    /// Interns a type and returns its ID.
    pub fn intern(&mut self, ty: Ty) -> TypeId {
        let (index, _) = self.types.insert_full(ty);
        index as TypeId
    }

    /// Gets a type by its ID.
    pub fn get(&self, id: TypeId) -> Option<&Ty> {
        self.types.get_index(id as usize)
    }

    /// Returns the number of interned types.
    pub fn len(&self) -> usize {
        self.types.len()
    }

    /// Returns true if no types have been interned.
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_size() {
        assert_eq!(Ty::I8.size_in_bytes(), 1);
        assert_eq!(Ty::I32.size_in_bytes(), 4);
        assert_eq!(Ty::I64.size_in_bytes(), 8);
        assert_eq!(Ty::F32.size_in_bytes(), 4);
        assert_eq!(Ty::F64.size_in_bytes(), 8);
        assert_eq!(Ty::Bool.size_in_bytes(), 1);
        assert_eq!(Ty::Ptr(Box::new(Ty::I32)).size_in_bytes(), 8);
    }

    #[test]
    fn test_type_predicates() {
        assert!(Ty::I32.is_integer());
        assert!(!Ty::F64.is_integer());
        assert!(Ty::F32.is_float());
        assert!(!Ty::Bool.is_float());
        assert!(Ty::Ptr(Box::new(Ty::I32)).is_pointer());
        assert!(!Ty::I32.is_pointer());
        assert!(Ty::Void.is_void());
    }

    #[test]
    fn test_type_interner() {
        let mut interner = TypeInterner::new();

        let id1 = interner.intern(Ty::I32);
        let id2 = interner.intern(Ty::I64);
        let id3 = interner.intern(Ty::I32); // Same as id1

        assert_eq!(id1, id3);
        assert_ne!(id1, id2);
        assert_eq!(interner.len(), 2);

        assert_eq!(interner.get(id1), Some(&Ty::I32));
        assert_eq!(interner.get(id2), Some(&Ty::I64));
    }

    #[test]
    fn test_type_display() {
        assert_eq!(format!("{}", Ty::I32), "i32");
        assert_eq!(format!("{}", Ty::F64), "f64");
        assert_eq!(format!("{}", Ty::Bool), "bool");
        assert_eq!(format!("{}", Ty::Void), "void");
        assert_eq!(format!("{}", Ty::Ptr(Box::new(Ty::I32))), "* i32");
    }
}
