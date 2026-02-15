//! Block terminators for Jet IR.
//!
//! Terminators are the final instruction in each basic block that determines
//! where control flow goes next.

use crate::values::{BlockId, ValueId};
use std::fmt;

/// A terminator instruction that ends a basic block.
#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    /// Return from the function with an optional value.
    Return(Option<ValueId>),

    /// Unconditional branch to another block.
    Branch(BlockId),

    /// Conditional branch based on a boolean value.
    CondBranch {
        cond: ValueId,
        then_block: BlockId,
        else_block: BlockId,
    },

    /// Switch on an integer value.
    Switch {
        value: ValueId,
        default_block: BlockId,
        cases: Vec<(i64, BlockId)>,
    },

    /// Unreachable code.
    Unreachable,
}

impl Terminator {
    /// Returns all successor block IDs.
    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            Terminator::Return(_) => vec![],
            Terminator::Branch(block) => vec![*block],
            Terminator::CondBranch {
                then_block,
                else_block,
                ..
            } => vec![*then_block, *else_block],
            Terminator::Switch {
                default_block,
                cases,
                ..
            } => {
                let mut blocks = vec![*default_block];
                blocks.extend(cases.iter().map(|(_, b)| *b));
                blocks
            }
            Terminator::Unreachable => vec![],
        }
    }

    /// Returns all value IDs used as operands.
    pub fn operands(&self) -> Vec<ValueId> {
        match self {
            Terminator::Return(val) => val.iter().copied().collect(),
            Terminator::Branch(_) => vec![],
            Terminator::CondBranch { cond, .. } => vec![*cond],
            Terminator::Switch { value, .. } => vec![*value],
            Terminator::Unreachable => vec![],
        }
    }

    /// Returns true if this terminator returns from the function.
    pub fn is_return(&self) -> bool {
        matches!(self, Terminator::Return(_))
    }

    /// Returns true if this terminator is a branch (conditional or unconditional).
    pub fn is_branch(&self) -> bool {
        matches!(
            self,
            Terminator::Branch(_) | Terminator::CondBranch { .. } | Terminator::Switch { .. }
        )
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Return(None) => write!(f, "ret void"),
            Terminator::Return(Some(val)) => write!(f, "ret {}", val),
            Terminator::Branch(block) => write!(f, "br {}", block),
            Terminator::CondBranch {
                cond,
                then_block,
                else_block,
            } => {
                write!(f, "br {} ? {} : {}", cond, then_block, else_block)
            }
            Terminator::Switch {
                value,
                default_block,
                cases,
            } => {
                write!(f, "switch {} {{", value)?;
                for (val, block) in cases {
                    write!(f, " {}: {},", val, block)?;
                }
                write!(f, " default: {} }}", default_block)
            }
            Terminator::Unreachable => write!(f, "unreachable"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_return_terminator() {
        let ret_void = Terminator::Return(None);
        assert!(ret_void.is_return());
        assert!(!ret_void.is_branch());
        assert_eq!(ret_void.successors(), vec![]);
        assert_eq!(ret_void.operands(), vec![]);

        let val = ValueId::new(0);
        let ret_val = Terminator::Return(Some(val));
        assert_eq!(ret_val.operands(), vec![val]);
    }

    #[test]
    fn test_branch_terminator() {
        let block = BlockId::new(1);
        let br = Terminator::Branch(block);
        assert!(!br.is_return());
        assert!(br.is_branch());
        assert_eq!(br.successors(), vec![block]);
    }

    #[test]
    fn test_cond_branch_terminator() {
        let cond = ValueId::new(0);
        let then_block = BlockId::new(1);
        let else_block = BlockId::new(2);

        let br = Terminator::CondBranch {
            cond,
            then_block,
            else_block,
        };

        assert!(!br.is_return());
        assert!(br.is_branch());
        assert_eq!(br.successors(), vec![then_block, else_block]);
        assert_eq!(br.operands(), vec![cond]);
    }

    #[test]
    fn test_switch_terminator() {
        let val = ValueId::new(0);
        let default = BlockId::new(3);
        let case1 = BlockId::new(1);
        let case2 = BlockId::new(2);

        let switch = Terminator::Switch {
            value: val,
            default_block: default,
            cases: vec![(1, case1), (2, case2)],
        };

        assert!(!switch.is_return());
        assert!(switch.is_branch());
        assert_eq!(switch.successors(), vec![default, case1, case2]);
        assert_eq!(switch.operands(), vec![val]);
    }

    #[test]
    fn test_unreachable_terminator() {
        let unreach = Terminator::Unreachable;
        assert!(!unreach.is_return());
        assert!(!unreach.is_branch());
        assert_eq!(unreach.successors(), vec![]);
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Terminator::Return(None)), "ret void");

        let val = ValueId::new(0);
        assert_eq!(format!("{}", Terminator::Return(Some(val))), "ret %0");

        let block = BlockId::new(1);
        assert_eq!(format!("{}", Terminator::Branch(block)), "br bb1");
    }
}
