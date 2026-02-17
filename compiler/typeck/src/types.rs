//! Type representation and context for the Jet type system
//!
//! This module defines the core type system structures including:
//! - TypeId: Interned type identifiers
//! - TypeKind: The different kinds of types (primitives, composites, etc.)
//! - TypeContext: Arena for interning and managing types
//! - TypeVar: Unification variables for type inference

use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicU32, Ordering};

/// Unique identifier for an interned type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(pub u32);

impl TypeId {
    /// The unit type ()
    pub const UNIT: TypeId = TypeId(0);
    /// The never type !
    pub const NEVER: TypeId = TypeId(1);
    /// The bool type
    pub const BOOL: TypeId = TypeId(2);
    /// The int type (i64)
    pub const INT: TypeId = TypeId(3);
    /// The uint type (u64)
    pub const UINT: TypeId = TypeId(4);
    /// The float type (f64)
    pub const FLOAT: TypeId = TypeId(5);
    /// The char type
    pub const CHAR: TypeId = TypeId(6);
    /// The string type
    pub const STRING: TypeId = TypeId(7);
}

/// Size of integer types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntSize {
    I8,
    I16,
    I32,
    I64,
    Isize,
}

impl IntSize {
    /// Returns the size in bytes
    pub fn size_in_bytes(&self) -> usize {
        match self {
            IntSize::I8 => 1,
            IntSize::I16 => 2,
            IntSize::I32 => 4,
            IntSize::I64 => 8,
            IntSize::Isize => 8, // Assuming 64-bit platform
        }
    }
}

impl fmt::Display for IntSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntSize::I8 => write!(f, "8"),
            IntSize::I16 => write!(f, "16"),
            IntSize::I32 => write!(f, "32"),
            IntSize::I64 => write!(f, "64"),
            IntSize::Isize => write!(f, ""),
        }
    }
}

/// Size of float types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatSize {
    F32,
    F64,
}

impl FloatSize {
    /// Returns the size in bytes
    pub fn size_in_bytes(&self) -> usize {
        match self {
            FloatSize::F32 => 4,
            FloatSize::F64 => 8,
        }
    }
}

impl fmt::Display for FloatSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatSize::F32 => write!(f, "32"),
            FloatSize::F64 => write!(f, "64"),
        }
    }
}

/// Mutability for reference types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Immutable,
    Mutable,
}

impl fmt::Display for Mutability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mutability::Immutable => write!(f, ""),
            Mutability::Mutable => write!(f, "mut "),
        }
    }
}

/// Definition ID for structs, enums, and other named types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

/// Variable ID for type variables (unification variables)
pub type VarId = u32;

/// The kind of a type variable
#[derive(Debug, Clone, PartialEq)]
pub enum TypeVarKind {
    /// Unbound type variable at a specific level (for generalization)
    Unbound { level: Level },
    /// Linked to another type (after unification)
    Link(TypeId),
    /// Generic parameter (cannot be unified)
    Generic(DefId),
}

/// Type variable for unification
#[derive(Debug, Clone)]
pub struct TypeVar {
    pub id: VarId,
    pub kind: TypeVarKind,
}

/// Level for let-generalization (keeps track of binding depth)
pub type Level = u32;

/// Effect set for function types
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct EffectSet {
    pub effects: Vec<EffectInstance>,
}

impl EffectSet {
    /// Create an empty effect set
    pub fn empty() -> Self {
        Self {
            effects: Vec::new(),
        }
    }

    /// Create an effect set with a single effect
    pub fn singleton(effect: EffectInstance) -> Self {
        Self {
            effects: vec![effect],
        }
    }

    /// Check if this effect set contains a specific effect
    pub fn contains(&self, effect: &EffectInstance) -> bool {
        self.effects.iter().any(|e| e == effect)
    }

    /// Add an effect to the set
    pub fn add(&mut self, effect: EffectInstance) {
        if !self.contains(&effect) {
            self.effects.push(effect);
        }
    }

    /// Extend with effects from another set
    pub fn extend(&mut self, other: &EffectSet) {
        for effect in &other.effects {
            self.add(effect.clone());
        }
    }

    /// Returns true if there are no effects
    pub fn is_empty(&self) -> bool {
        self.effects.is_empty()
    }
}

/// An instance of an effect
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectInstance {
    /// The effect type definition
    pub effect: DefId,
    /// Type arguments for the effect
    pub args: Vec<TypeId>,
}

/// The kind of a type (what the type actually is)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// Signed integer (i8, i16, i32, i64, int)
    Int(IntSize),
    /// Unsigned integer (u8, u16, u32, u64, uint)
    Uint(IntSize),
    /// Floating point (f32, f64, float)
    Float(FloatSize),
    /// Boolean
    Bool,
    /// Character (Unicode scalar)
    Char,
    /// String (UTF-8)
    String,
    /// Unit type ()
    Unit,
    /// Never type (!) - the empty type
    Never,
    /// Tuple type (T1, T2, ...)
    Tuple(Vec<TypeId>),
    /// Array type [T; n]
    Array(TypeId, usize),
    /// Slice type `\[T\]`
    Slice(TypeId),
    /// Struct type
    Struct(DefId),
    /// Enum type
    Enum(DefId),
    /// Function type fn(T1, T2) -> R ! Effects
    Function {
        params: Vec<TypeId>,
        ret: TypeId,
        effects: EffectSet,
    },
    /// Reference type &T or &mut T
    Ref(TypeId, Mutability),
    /// Raw pointer type (for unsafe code)
    RawPtr(TypeId, Mutability),
    /// Type variable (for inference)
    Var(VarId),
    /// Generic parameter
    Param(DefId),
    /// Channel type `chan\[T\]`
    Channel(TypeId),
    /// Async type async T
    Async(TypeId),
}

/// Context for interning and managing types
#[derive(Debug)]
pub struct TypeContext {
    /// Arena of interned types
    types: Vec<TypeKind>,
    /// Mapping from type kind to type ID (for interning)
    intern_table: HashMap<TypeKind, TypeId>,
    /// Type variables (unification variables)
    type_vars: Vec<TypeVar>,
    /// Counter for generating fresh variable IDs
    #[allow(dead_code)]
    var_counter: AtomicU32,
    /// Counter for generating fresh definition IDs
    #[allow(dead_code)]
    def_counter: AtomicU32,
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeContext {
    /// Create a new type context with primitive types pre-interned
    pub fn new() -> Self {
        let mut ctx = Self {
            types: Vec::new(),
            intern_table: HashMap::new(),
            type_vars: Vec::new(),
            var_counter: AtomicU32::new(0),
            def_counter: AtomicU32::new(0),
        };

        // Pre-intern primitive types with fixed IDs
        ctx.intern(TypeKind::Unit);
        ctx.intern(TypeKind::Never);
        ctx.intern(TypeKind::Bool);
        ctx.intern(TypeKind::Int(IntSize::I64));
        ctx.intern(TypeKind::Uint(IntSize::I64));
        ctx.intern(TypeKind::Float(FloatSize::F64));
        ctx.intern(TypeKind::Char);
        ctx.intern(TypeKind::String);

        ctx
    }

    /// Get the kind of a type by its ID
    pub fn type_kind(&self, id: TypeId) -> &TypeKind {
        &self.types[id.0 as usize]
    }

    /// Intern a type kind and return its ID
    pub fn intern(&mut self, kind: TypeKind) -> TypeId {
        if let Some(&id) = self.intern_table.get(&kind) {
            return id;
        }

        let id = TypeId(self.types.len() as u32);
        self.types.push(kind.clone());
        self.intern_table.insert(kind, id);
        id
    }

    /// Create a new type variable
    pub fn fresh_var(&mut self, level: Level) -> TypeId {
        let id = self.var_counter.fetch_add(1, Ordering::SeqCst);
        self.type_vars.push(TypeVar {
            id,
            kind: TypeVarKind::Unbound { level },
        });
        self.intern(TypeKind::Var(id))
    }

    /// Get a type variable by its ID
    pub fn get_var(&self, id: VarId) -> Option<&TypeVar> {
        self.type_vars.get(id as usize)
    }

    /// Get a mutable reference to a type variable
    pub fn get_var_mut(&mut self, id: VarId) -> Option<&mut TypeVar> {
        self.type_vars.get_mut(id as usize)
    }

    /// Update the kind of a type variable
    pub fn set_var_kind(&mut self, id: VarId, kind: TypeVarKind) {
        if let Some(var) = self.type_vars.get_mut(id as usize) {
            var.kind = kind;
        }
    }

    /// Create a fresh definition ID
    pub fn fresh_def_id(&self) -> DefId {
        DefId(self.def_counter.fetch_add(1, Ordering::SeqCst))
    }

    /// Create a tuple type
    pub fn mk_tuple(&mut self, elements: Vec<TypeId>) -> TypeId {
        self.intern(TypeKind::Tuple(elements))
    }

    /// Create an array type
    pub fn mk_array(&mut self, element: TypeId, size: usize) -> TypeId {
        self.intern(TypeKind::Array(element, size))
    }

    /// Create a slice type
    pub fn mk_slice(&mut self, element: TypeId) -> TypeId {
        self.intern(TypeKind::Slice(element))
    }

    /// Create a function type
    pub fn mk_function(&mut self, params: Vec<TypeId>, ret: TypeId, effects: EffectSet) -> TypeId {
        self.intern(TypeKind::Function {
            params,
            ret,
            effects,
        })
    }

    /// Create a reference type
    pub fn mk_ref(&mut self, inner: TypeId, mutability: Mutability) -> TypeId {
        self.intern(TypeKind::Ref(inner, mutability))
    }

    /// Create an integer type
    pub fn mk_int(&mut self, size: IntSize) -> TypeId {
        self.intern(TypeKind::Int(size))
    }

    /// Create an unsigned integer type
    pub fn mk_uint(&mut self, size: IntSize) -> TypeId {
        self.intern(TypeKind::Uint(size))
    }

    /// Create a float type
    pub fn mk_float(&mut self, size: FloatSize) -> TypeId {
        self.intern(TypeKind::Float(size))
    }

    /// Format a type as a string
    pub fn type_to_string(&self, id: TypeId) -> String {
        format!("{}", TypeFormatter { tcx: self, id })
    }

    /// Get the level of a type variable
    pub fn var_level(&self, var_id: VarId) -> Option<Level> {
        self.type_vars
            .get(var_id as usize)
            .and_then(|v| match v.kind {
                TypeVarKind::Unbound { level } => Some(level),
                _ => None,
            })
    }
}

/// Helper struct for formatting types
struct TypeFormatter<'a> {
    tcx: &'a TypeContext,
    id: TypeId,
}

impl<'a> fmt::Display for TypeFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.tcx.type_kind(self.id) {
            TypeKind::Int(size) => write!(f, "int{}", size),
            TypeKind::Uint(size) => write!(f, "uint{}", size),
            TypeKind::Float(size) => write!(f, "float{}", size),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Char => write!(f, "char"),
            TypeKind::String => write!(f, "string"),
            TypeKind::Unit => write!(f, "unit"),
            TypeKind::Never => write!(f, "!"),
            TypeKind::Tuple(elements) => {
                write!(f, "(")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(
                        f,
                        "{}",
                        TypeFormatter {
                            tcx: self.tcx,
                            id: *elem
                        }
                    )?;
                }
                write!(f, ")")
            }
            TypeKind::Array(elem, size) => {
                write!(
                    f,
                    "[{}; {}]",
                    TypeFormatter {
                        tcx: self.tcx,
                        id: *elem
                    },
                    size
                )
            }
            TypeKind::Slice(elem) => {
                write!(
                    f,
                    "[{}]",
                    TypeFormatter {
                        tcx: self.tcx,
                        id: *elem
                    }
                )
            }
            TypeKind::Struct(def_id) => write!(f, "Struct#{}<...>", def_id.0),
            TypeKind::Enum(def_id) => write!(f, "Enum#{}<...>", def_id.0),
            TypeKind::Function {
                params,
                ret,
                effects,
            } => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(
                        f,
                        "{}",
                        TypeFormatter {
                            tcx: self.tcx,
                            id: *param
                        }
                    )?;
                }
                write!(
                    f,
                    ") -> {}",
                    TypeFormatter {
                        tcx: self.tcx,
                        id: *ret
                    }
                )?;
                if !effects.is_empty() {
                    write!(f, " ! ...")?;
                }
                Ok(())
            }
            TypeKind::Ref(inner, mutability) => {
                write!(f, "&{}", mutability)?;
                write!(
                    f,
                    "{}",
                    TypeFormatter {
                        tcx: self.tcx,
                        id: *inner
                    }
                )
            }
            TypeKind::RawPtr(inner, mutability) => {
                write!(
                    f,
                    "*{}{}",
                    mutability,
                    TypeFormatter {
                        tcx: self.tcx,
                        id: *inner
                    }
                )
            }
            TypeKind::Var(id) => write!(f, "'t{}", id),
            TypeKind::Param(def_id) => write!(f, "'p{}", def_id.0),
            TypeKind::Channel(elem) => {
                write!(f, "chan[")?;
                write!(
                    f,
                    "{}",
                    TypeFormatter {
                        tcx: self.tcx,
                        id: *elem
                    }
                )?;
                write!(f, "]")
            }
            TypeKind::Async(inner) => {
                write!(f, "async ")?;
                write!(
                    f,
                    "{}",
                    TypeFormatter {
                        tcx: self.tcx,
                        id: *inner
                    }
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_types() {
        let tcx = TypeContext::new();
        assert_eq!(tcx.type_to_string(TypeId::UNIT), "unit");
        assert_eq!(tcx.type_to_string(TypeId::BOOL), "bool");
        assert_eq!(tcx.type_to_string(TypeId::INT), "int64");
        assert_eq!(tcx.type_to_string(TypeId::FLOAT), "float64");
        assert_eq!(tcx.type_to_string(TypeId::STRING), "string");
    }

    #[test]
    fn test_tuple_type() {
        let mut tcx = TypeContext::new();
        let tuple = tcx.mk_tuple(vec![TypeId::INT, TypeId::STRING]);
        assert_eq!(tcx.type_to_string(tuple), "(int64, string)");
    }

    #[test]
    fn test_function_type() {
        let mut tcx = TypeContext::new();
        let func = tcx.mk_function(
            vec![TypeId::INT, TypeId::INT],
            TypeId::BOOL,
            EffectSet::empty(),
        );
        assert_eq!(tcx.type_to_string(func), "fn(int64, int64) -> bool");
    }

    #[test]
    fn test_type_variable() {
        let mut tcx = TypeContext::new();
        let var = tcx.fresh_var(0);
        assert_eq!(tcx.type_to_string(var), "'t0");
    }

    #[test]
    fn test_interning() {
        let mut tcx = TypeContext::new();
        let t1 = tcx.mk_tuple(vec![TypeId::INT, TypeId::BOOL]);
        let t2 = tcx.mk_tuple(vec![TypeId::INT, TypeId::BOOL]);
        assert_eq!(t1, t2); // Should be interned to same ID
    }

    #[test]
    fn test_reference_type() {
        let mut tcx = TypeContext::new();
        let ref_int = tcx.mk_ref(TypeId::INT, Mutability::Immutable);
        let ref_mut_int = tcx.mk_ref(TypeId::INT, Mutability::Mutable);
        assert_eq!(tcx.type_to_string(ref_int), "&int64");
        assert_eq!(tcx.type_to_string(ref_mut_int), "&mut int64");
    }
}
