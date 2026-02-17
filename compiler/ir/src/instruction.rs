//! Instructions for Jet IR.
//!
//! This module defines all instructions in the IR. Instructions are in SSA form
//! and operate on values. Each instruction that produces a result assigns to a
//! unique value ID.

use crate::types::Ty;
use crate::values::{BlockId, ValueId};
use std::fmt;

/// A binary arithmetic or logical operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    // Bitwise
    And,
    Or,
    Xor,
    Shl,
    Shr,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "add"),
            BinaryOp::Sub => write!(f, "sub"),
            BinaryOp::Mul => write!(f, "mul"),
            BinaryOp::Div => write!(f, "div"),
            BinaryOp::Rem => write!(f, "rem"),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::Xor => write!(f, "xor"),
            BinaryOp::Shl => write!(f, "shl"),
            BinaryOp::Shr => write!(f, "shr"),
            BinaryOp::Eq => write!(f, "eq"),
            BinaryOp::Ne => write!(f, "ne"),
            BinaryOp::Lt => write!(f, "lt"),
            BinaryOp::Le => write!(f, "le"),
            BinaryOp::Gt => write!(f, "gt"),
            BinaryOp::Ge => write!(f, "ge"),
        }
    }
}

/// A unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "neg"),
            UnaryOp::Not => write!(f, "not"),
        }
    }
}

/// An instruction in the IR.
///
/// Most instructions produce a value and assign it to a `ValueId`.
/// The `result` field indicates which value this instruction defines.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Literals
    /// Load a constant value.
    Const {
        result: ValueId,
        value: ConstantValue,
    },

    // Arithmetic
    /// Binary operation: result = lhs op rhs
    Binary {
        result: ValueId,
        op: BinaryOp,
        lhs: ValueId,
        rhs: ValueId,
    },

    /// Unary operation: result = op operand
    Unary {
        result: ValueId,
        op: UnaryOp,
        operand: ValueId,
    },

    // Memory operations
    /// Allocate stack space for a value of the given type.
    Alloc { result: ValueId, ty: Ty },

    /// Load a value from a pointer.
    Load {
        result: ValueId,
        ptr: ValueId,
        ty: Ty,
    },

    /// Store a value to a pointer.
    Store { ptr: ValueId, value: ValueId },

    /// Get a pointer to a struct field.
    GetFieldPtr {
        result: ValueId,
        ptr: ValueId,
        field_index: usize,
        struct_ty: Ty,
    },

    /// Get a pointer to an array element.
    GetElementPtr {
        result: ValueId,
        ptr: ValueId,
        index: ValueId,
        elem_ty: Ty,
    },

    /// Cast a pointer to another pointer type.
    BitCast {
        result: ValueId,
        value: ValueId,
        ty: Ty,
    },

    /// Integer to integer cast.
    IntCast {
        result: ValueId,
        value: ValueId,
        ty: Ty,
    },

    /// Float to float cast.
    FloatCast {
        result: ValueId,
        value: ValueId,
        ty: Ty,
    },

    /// Integer to float cast.
    IntToFloat {
        result: ValueId,
        value: ValueId,
        ty: Ty,
    },

    /// Float to integer cast.
    FloatToInt {
        result: ValueId,
        value: ValueId,
        ty: Ty,
    },

    // Control flow (within block)
    /// Call a function.
    Call {
        result: ValueId,
        func: String,
        args: Vec<ValueId>,
        ty: Ty,
    },

    /// Call a function pointer.
    CallIndirect {
        result: ValueId,
        ptr: ValueId,
        args: Vec<ValueId>,
        ty: Ty,
    },

    /// Phi node for SSA merge points.
    Phi {
        result: ValueId,
        incoming: Vec<(BlockId, ValueId)>,
        ty: Ty,
    },

    // Aggregate operations
    /// Create a struct value.
    StructAgg {
        result: ValueId,
        fields: Vec<ValueId>,
        ty: Ty,
    },

    /// Create an array value.
    ArrayAgg {
        result: ValueId,
        elements: Vec<ValueId>,
        ty: Ty,
    },

    /// Extract a value from a struct.
    ExtractField {
        result: ValueId,
        aggregate: ValueId,
        field_index: usize,
    },

    /// Insert a value into a struct.
    InsertField {
        result: ValueId,
        aggregate: ValueId,
        field_index: usize,
        value: ValueId,
    },

    // Effect operations
    /// Try to call a function that can raise an error.
    TryCall {
        result: ValueId,
        func: String,
        args: Vec<ValueId>,
        success_block: BlockId,
        failure_block: BlockId,
        failure_val: ValueId,
    },

    /// Resume from an error handler with a value.
    Resume { value: ValueId },

    /// Await an async value.
    Await { result: ValueId, future: ValueId },

    // Debug
    /// Debug print a value.
    DebugPrint { value: ValueId },
}

/// A constant value for the Const instruction.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Int(i64, Ty),
    Float(f64, Ty),
    Bool(bool),
    String(String),
    Null(Ty),
    Undef(Ty),
    Zero(Ty),
}

impl Instruction {
    /// Returns the result value ID if this instruction produces one.
    pub fn result(&self) -> Option<ValueId> {
        match self {
            Instruction::Const { result, .. } => Some(*result),
            Instruction::Binary { result, .. } => Some(*result),
            Instruction::Unary { result, .. } => Some(*result),
            Instruction::Alloc { result, .. } => Some(*result),
            Instruction::Load { result, .. } => Some(*result),
            Instruction::Store { .. } => None,
            Instruction::GetFieldPtr { result, .. } => Some(*result),
            Instruction::GetElementPtr { result, .. } => Some(*result),
            Instruction::BitCast { result, .. } => Some(*result),
            Instruction::IntCast { result, .. } => Some(*result),
            Instruction::FloatCast { result, .. } => Some(*result),
            Instruction::IntToFloat { result, .. } => Some(*result),
            Instruction::FloatToInt { result, .. } => Some(*result),
            Instruction::Call { result, .. } => Some(*result),
            Instruction::CallIndirect { result, .. } => Some(*result),
            Instruction::Phi { result, .. } => Some(*result),
            Instruction::StructAgg { result, .. } => Some(*result),
            Instruction::ArrayAgg { result, .. } => Some(*result),
            Instruction::ExtractField { result, .. } => Some(*result),
            Instruction::InsertField { result, .. } => Some(*result),
            Instruction::TryCall { result, .. } => Some(*result),
            Instruction::Resume { .. } => None,
            Instruction::Await { result, .. } => Some(*result),
            Instruction::DebugPrint { .. } => None,
        }
    }

    /// Returns all value IDs used as operands by this instruction.
    pub fn operands(&self) -> Vec<ValueId> {
        match self {
            Instruction::Const { .. } => vec![],
            Instruction::Binary { lhs, rhs, .. } => vec![*lhs, *rhs],
            Instruction::Unary { operand, .. } => vec![*operand],
            Instruction::Alloc { .. } => vec![],
            Instruction::Load { ptr, .. } => vec![*ptr],
            Instruction::Store { ptr, value } => vec![*ptr, *value],
            Instruction::GetFieldPtr { ptr, .. } => vec![*ptr],
            Instruction::GetElementPtr { ptr, index, .. } => vec![*ptr, *index],
            Instruction::BitCast { value, .. } => vec![*value],
            Instruction::IntCast { value, .. } => vec![*value],
            Instruction::FloatCast { value, .. } => vec![*value],
            Instruction::IntToFloat { value, .. } => vec![*value],
            Instruction::FloatToInt { value, .. } => vec![*value],
            Instruction::Call { args, .. } => args.clone(),
            Instruction::CallIndirect { ptr, args, .. } => {
                let mut ops = vec![*ptr];
                ops.extend(args);
                ops
            }
            Instruction::Phi { incoming, .. } => incoming.iter().map(|(_, v)| *v).collect(),
            Instruction::StructAgg { fields, .. } => fields.clone(),
            Instruction::ArrayAgg { elements, .. } => elements.clone(),
            Instruction::ExtractField { aggregate, .. } => vec![*aggregate],
            Instruction::InsertField {
                aggregate, value, ..
            } => vec![*aggregate, *value],
            Instruction::TryCall { args, .. } => args.clone(),
            Instruction::Resume { value } => vec![*value],
            Instruction::Await { future, .. } => vec![*future],
            Instruction::DebugPrint { value } => vec![*value],
        }
    }

    /// Returns true if this instruction has side effects.
    pub fn has_side_effects(&self) -> bool {
        matches!(
            self,
            Instruction::Store { .. }
                | Instruction::Call { .. }
                | Instruction::CallIndirect { .. }
                | Instruction::TryCall { .. }
                | Instruction::Resume { .. }
                | Instruction::Await { .. }
                | Instruction::DebugPrint { .. }
        )
    }

    /// Returns true if this instruction is a terminator (controls flow to other blocks).
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Instruction::TryCall { .. } | Instruction::Resume { .. }
        )
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Const { result, value } => {
                write!(f, "{} = const {:?}", result, value)
            }
            Instruction::Binary {
                result,
                op,
                lhs,
                rhs,
            } => {
                write!(f, "{} = {} {}, {}", result, op, lhs, rhs)
            }
            Instruction::Unary {
                result,
                op,
                operand,
            } => {
                write!(f, "{} = {} {}", result, op, operand)
            }
            Instruction::Alloc { result, ty } => {
                write!(f, "{} = alloc {}", result, ty)
            }
            Instruction::Load { result, ptr, ty } => {
                write!(f, "{} = load {} ({})", result, ptr, ty)
            }
            Instruction::Store { ptr, value } => {
                write!(f, "store {}, {}", value, ptr)
            }
            Instruction::GetFieldPtr {
                result,
                ptr,
                field_index,
                struct_ty,
            } => {
                write!(
                    f,
                    "{} = getfieldptr {}, {}, {}",
                    result, ptr, field_index, struct_ty
                )
            }
            Instruction::GetElementPtr {
                result,
                ptr,
                index,
                elem_ty,
            } => {
                write!(
                    f,
                    "{} = getelementptr {}, {}, {}",
                    result, ptr, index, elem_ty
                )
            }
            Instruction::BitCast { result, value, ty } => {
                write!(f, "{} = bitcast {} to {}", result, value, ty)
            }
            Instruction::IntCast { result, value, ty } => {
                write!(f, "{} = intcast {} to {}", result, value, ty)
            }
            Instruction::FloatCast { result, value, ty } => {
                write!(f, "{} = floatcast {} to {}", result, value, ty)
            }
            Instruction::IntToFloat { result, value, ty } => {
                write!(f, "{} = inttofloat {} to {}", result, value, ty)
            }
            Instruction::FloatToInt { result, value, ty } => {
                write!(f, "{} = floattoint {} to {}", result, value, ty)
            }
            Instruction::Call {
                result,
                func,
                args,
                ty,
            } => {
                write!(f, "{} = call {} {} (", result, ty, func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Instruction::CallIndirect {
                result,
                ptr,
                args,
                ty,
            } => {
                write!(f, "{} = callind {} {} (", result, ty, ptr)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Instruction::Phi {
                result,
                incoming,
                ty,
            } => {
                write!(f, "{} = phi {} ", result, ty)?;
                for (i, (block, val)) in incoming.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "[ {}, {} ]", val, block)?;
                }
                Ok(())
            }
            Instruction::StructAgg { result, fields, ty } => {
                write!(f, "{} = structagg {} {{ ", result, ty)?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, " }}")
            }
            Instruction::ArrayAgg {
                result,
                elements,
                ty,
            } => {
                write!(f, "{} = arrayagg {} [ ", result, ty)?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, " ]")
            }
            Instruction::ExtractField {
                result,
                aggregate,
                field_index,
            } => {
                write!(
                    f,
                    "{} = extractfield {}, {}",
                    result, aggregate, field_index
                )
            }
            Instruction::InsertField {
                result,
                aggregate,
                field_index,
                value,
            } => {
                write!(
                    f,
                    "{} = insertfield {}, {}, {}",
                    result, aggregate, field_index, value
                )
            }
            Instruction::TryCall {
                result,
                func,
                args,
                success_block,
                failure_block,
                failure_val,
            } => {
                write!(f, "{} = trycall {}(", result, func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(
                    f,
                    ") success {} failure {}({})",
                    success_block, failure_block, failure_val
                )
            }
            Instruction::Resume { value } => {
                write!(f, "resume {}", value)
            }
            Instruction::Await { result, future } => {
                write!(f, "{} = await {}", result, future)
            }
            Instruction::DebugPrint { value } => {
                write!(f, "debugprint {}", value)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_op_display() {
        assert_eq!(format!("{}", BinaryOp::Add), "add");
        assert_eq!(format!("{}", BinaryOp::Mul), "mul");
        assert_eq!(format!("{}", BinaryOp::Eq), "eq");
    }

    #[test]
    fn test_instruction_result() {
        let result = ValueId::new(0);
        let lhs = ValueId::new(1);
        let rhs = ValueId::new(2);

        let inst = Instruction::Binary {
            result,
            op: BinaryOp::Add,
            lhs,
            rhs,
        };

        assert_eq!(inst.result(), Some(result));
        assert_eq!(inst.operands(), vec![lhs, rhs]);
    }

    #[test]
    fn test_instruction_operands() {
        let ptr = ValueId::new(0);
        let val = ValueId::new(1);

        let store = Instruction::Store { ptr, value: val };
        assert_eq!(store.operands(), vec![ptr, val]);
        assert!(store.has_side_effects());
    }

    #[test]
    fn test_phi_operands() {
        let result = ValueId::new(0);
        let v1 = ValueId::new(1);
        let v2 = ValueId::new(2);
        let bb1 = BlockId::new(0);
        let bb2 = BlockId::new(1);

        let phi = Instruction::Phi {
            result,
            incoming: vec![(bb1, v1), (bb2, v2)],
            ty: Ty::I32,
        };

        assert_eq!(phi.operands(), vec![v1, v2]);
    }

    #[test]
    fn test_instruction_display() {
        let result = ValueId::new(0);
        let inst = Instruction::Const {
            result,
            value: ConstantValue::Int(42, Ty::I32),
        };
        let s = format!("{}", inst);
        assert!(s.contains("const"));
        assert!(s.contains("%0"));
    }
}
