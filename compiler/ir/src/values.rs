//! Value system for Jet IR.
//!
//! This module defines the SSA value system used throughout the IR.
//! Values are identified by unique IDs and can be constants or instructions.

use crate::types::Ty;
use std::fmt;

/// A unique identifier for a value in SSA form.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ValueId(pub u32);

impl ValueId {
    /// Creates a new value ID from a raw index.
    pub fn new(index: u32) -> Self {
        ValueId(index)
    }

    /// Returns the raw index of this value ID.
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for ValueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

/// A unique identifier for a basic block.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub u32);

impl BlockId {
    /// Creates a new block ID from a raw index.
    pub fn new(index: u32) -> Self {
        BlockId(index)
    }

    /// Returns the raw index of this block ID.
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

/// A constant value in the IR.
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    /// Integer constant with type.
    Int(i64, Ty),
    /// Float constant with type.
    Float(f64, Ty),
    /// Boolean constant.
    Bool(bool),
    /// String constant (null-terminated).
    String(String),
    /// Null pointer constant.
    Null(Ty),
    /// Undefined value.
    Undef(Ty),
    /// Zero initializer.
    Zero(Ty),
}

impl Constant {
    /// Returns the type of this constant.
    pub fn ty(&self) -> Ty {
        match self {
            Constant::Int(_, ty) => ty.clone(),
            Constant::Float(_, ty) => ty.clone(),
            Constant::Bool(_) => Ty::Bool,
            Constant::String(_) => Ty::Ptr(Box::new(Ty::I8)),
            Constant::Null(ty) => ty.clone(),
            Constant::Undef(ty) => ty.clone(),
            Constant::Zero(ty) => ty.clone(),
        }
    }

    /// Creates a constant integer of type i32.
    pub fn i32(val: i32) -> Self {
        Constant::Int(val as i64, Ty::I32)
    }

    /// Creates a constant integer of type i64.
    pub fn i64(val: i64) -> Self {
        Constant::Int(val, Ty::I64)
    }

    /// Creates a constant float of type f32.
    pub fn f32(val: f32) -> Self {
        Constant::Float(val as f64, Ty::F32)
    }

    /// Creates a constant float of type f64.
    pub fn f64(val: f64) -> Self {
        Constant::Float(val, Ty::F64)
    }

    /// Creates a constant boolean.
    pub fn bool(val: bool) -> Self {
        Constant::Bool(val)
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Int(val, ty) => write!(f, "{} {}", ty, val),
            Constant::Float(val, ty) => write!(f, "{} {}", ty, val),
            Constant::Bool(val) => write!(f, "{}", val),
            Constant::String(val) => write!(f, "\"{}\"", val.escape_default()),
            Constant::Null(ty) => write!(f, "null {}", ty),
            Constant::Undef(ty) => write!(f, "undef {}", ty),
            Constant::Zero(ty) => write!(f, "zeroinit {}", ty),
        }
    }
}

/// Information about a value in the value table.
#[derive(Debug, Clone)]
pub struct ValueInfo {
    /// The type of this value.
    pub ty: Ty,
    /// The name of this value (for debugging).
    pub name: Option<String>,
    /// Whether this value is a constant.
    pub is_constant: bool,
}

impl ValueInfo {
    /// Creates a new value info.
    pub fn new(ty: Ty) -> Self {
        Self {
            ty,
            name: None,
            is_constant: false,
        }
    }

    /// Sets the name of this value.
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Marks this value as a constant.
    pub fn as_constant(mut self) -> Self {
        self.is_constant = true;
        self
    }
}

/// A table for managing SSA values.
#[derive(Debug, Default)]
pub struct ValueTable {
    values: Vec<ValueInfo>,
    constants: Vec<Constant>,
    next_id: u32,
}

impl ValueTable {
    /// Creates a new empty value table.
    pub fn new() -> Self {
        Self::default()
    }

    /// Allocates a new value ID.
    pub fn alloc(&mut self, info: ValueInfo) -> ValueId {
        let id = ValueId(self.next_id);
        self.next_id += 1;
        self.values.push(info);
        id
    }

    /// Allocates a constant value and returns its ID.
    pub fn alloc_constant(&mut self, constant: Constant) -> ValueId {
        let ty = constant.ty().clone();
        let const_index = self.constants.len();
        self.constants.push(constant);

        let id = ValueId(self.next_id);
        self.next_id += 1;
        self.values.push(
            ValueInfo::new(ty)
                .with_name(format!("const_{}", const_index))
                .as_constant(),
        );
        id
    }

    /// Gets information about a value.
    pub fn get(&self, id: ValueId) -> Option<&ValueInfo> {
        self.values.get(id.index())
    }

    /// Gets the type of a value.
    pub fn ty(&self, id: ValueId) -> Option<&Ty> {
        self.get(id).map(|info| &info.ty)
    }

    /// Gets a constant by its index.
    pub fn get_constant(&self, index: usize) -> Option<&Constant> {
        self.constants.get(index)
    }

    /// Returns the number of values in the table.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Returns true if there are no values.
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Returns an iterator over all values.
    pub fn iter(&self) -> impl Iterator<Item = (ValueId, &ValueInfo)> {
        self.values
            .iter()
            .enumerate()
            .map(|(i, info)| (ValueId(i as u32), info))
    }
}

/// A parameter to a function or block.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    /// The name of the parameter.
    pub name: String,
    /// The type of the parameter.
    pub ty: Ty,
    /// The value ID assigned to this parameter.
    pub value: ValueId,
}

impl Param {
    /// Creates a new parameter.
    pub fn new(name: impl Into<String>, ty: Ty, value: ValueId) -> Self {
        Self {
            name: name.into(),
            ty,
            value,
        }
    }
}

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_id() {
        let id1 = ValueId::new(0);
        let id2 = ValueId::new(1);

        assert_eq!(id1.index(), 0);
        assert_eq!(id2.index(), 1);
        assert_eq!(format!("{}", id1), "%0");
    }

    #[test]
    fn test_block_id() {
        let id = BlockId::new(5);
        assert_eq!(id.index(), 5);
        assert_eq!(format!("{}", id), "bb5");
    }

    #[test]
    fn test_constant_helpers() {
        let c1 = Constant::i32(42);
        assert_eq!(c1, Constant::Int(42, Ty::I32));

        let c2 = Constant::i64(100);
        assert_eq!(c2, Constant::Int(100, Ty::I64));

        let c3 = Constant::f32(std::f32::consts::PI);
        assert!(
            matches!(c3, Constant::Float(v, Ty::F32) if (v - std::f64::consts::PI).abs() < 0.001)
        );

        let c4 = Constant::bool(true);
        assert_eq!(c4, Constant::Bool(true));
    }

    #[test]
    fn test_value_table() {
        let mut table = ValueTable::new();

        let info = ValueInfo::new(Ty::I32).with_name("x");
        let id1 = table.alloc(info);

        let const_id = table.alloc_constant(Constant::i32(42));

        assert_eq!(table.len(), 2);
        assert_eq!(table.ty(id1), Some(&Ty::I32));
        assert!(table.get(const_id).unwrap().is_constant);
    }

    #[test]
    fn test_param() {
        let param = Param::new("x", Ty::I32, ValueId::new(0));
        assert_eq!(param.name, "x");
        assert_eq!(param.ty, Ty::I32);
        assert_eq!(format!("{}", param), "x: i32");
    }
}
