//! Constant Folding pass
//!
//! This pass evaluates constant expressions at compile time and replaces them
//! with their computed values. It handles:
//!
//! - Arithmetic operations on constants (add, sub, mul, div, rem)
//! - Comparisons on constants (eq, ne, lt, le, gt, ge)
//! - Unary operations on constants (neg, not)
//! - Phi nodes with all constant incoming values
//! - Branch simplification when condition is constant

use crate::pass::{Pass, PassResult};
use jet_ir::{
    BinaryOp, ConstantValue, Function, Instruction, Module, Terminator, UnaryOp, ValueId,
};
use rustc_hash::FxHashMap;

/// Constant folding pass.
///
/// Evaluates constant expressions at compile time and propagates them.
pub struct ConstantFolding {
    /// Map from value IDs to their constant values (if known).
    constants: FxHashMap<ValueId, ConstantValue>,
    /// Whether to simplify branches with constant conditions.
    simplify_branches: bool,
}

impl ConstantFolding {
    /// Creates a new constant folding pass.
    pub fn new() -> Self {
        Self {
            constants: FxHashMap::default(),
            simplify_branches: true,
        }
    }

    /// Creates a new constant folding pass without branch simplification.
    pub fn without_branch_simplification() -> Self {
        Self {
            constants: FxHashMap::default(),
            simplify_branches: false,
        }
    }

    /// Run constant folding on a function.
    fn fold_in_function(&mut self, func: &mut Function) -> PassResult {
        self.constants.clear();
        let mut changed = false;

        // Collect initial constants from Const instructions
        for block in &func.blocks {
            for inst in &block.instructions {
                if let Instruction::Const { result, value } = inst {
                    self.constants.insert(*result, value.clone());
                }
            }
        }

        // Fold instructions
        for block in &mut func.blocks {
            for inst in &mut block.instructions {
                if self.try_fold_instruction(inst) {
                    changed = true;
                }
            }

            // Try to simplify terminator
            if self.simplify_branches && self.try_simplify_terminator(&mut block.terminator) {
                changed = true;
            }
        }

        if changed {
            PassResult::Changed
        } else {
            PassResult::Unchanged
        }
    }

    /// Try to fold a single instruction.
    /// Returns true if the instruction was folded.
    fn try_fold_instruction(&mut self, inst: &mut Instruction) -> bool {
        match inst {
            Instruction::Binary {
                result,
                op,
                lhs,
                rhs,
                ..
            } => {
                let result_id = *result;
                if let (Some(lval), Some(rval)) = (self.constants.get(lhs), self.constants.get(rhs))
                {
                    if let Some(folded) = self.fold_binary_op(*op, lval, rval) {
                        *inst = Instruction::Const {
                            result: result_id,
                            value: folded.clone(),
                        };
                        self.constants.insert(result_id, folded);
                        return true;
                    }
                }
                false
            }

            Instruction::Unary {
                result,
                op,
                operand,
                ..
            } => {
                let result_id = *result;
                if let Some(val) = self.constants.get(operand) {
                    if let Some(folded) = self.fold_unary_op(*op, val) {
                        *inst = Instruction::Const {
                            result: result_id,
                            value: folded.clone(),
                        };
                        self.constants.insert(result_id, folded);
                        return true;
                    }
                }
                false
            }

            Instruction::Phi { result, incoming } => {
                let result_id = *result;
                // If all incoming values are the same constant, fold to that constant
                let const_values: Vec<_> = incoming
                    .iter()
                    .filter_map(|(_, val)| self.constants.get(val))
                    .collect();

                if const_values.len() == incoming.len() && !const_values.is_empty() {
                    // Check if all constants are equal
                    let first = &const_values[0];
                    if const_values.iter().all(|v| v == first) {
                        *inst = Instruction::Const {
                            result: result_id,
                            value: (*first).clone(),
                        };
                        self.constants.insert(result_id, (*first).clone());
                        return true;
                    }
                }
                false
            }

            _ => false,
        }
    }

    /// Try to simplify a terminator based on constant conditions.
    fn try_simplify_terminator(&self, terminator: &mut Terminator) -> bool {
        match terminator {
            Terminator::CondBranch {
                cond,
                then_block,
                else_block,
            } => {
                if let Some(ConstantValue::Bool(true)) = self.constants.get(cond) {
                    *terminator = Terminator::Branch(*then_block);
                    return true;
                }
                if let Some(ConstantValue::Bool(false)) = self.constants.get(cond) {
                    *terminator = Terminator::Branch(*else_block);
                    return true;
                }
                false
            }

            Terminator::Switch {
                value,
                default_block,
                cases,
            } => {
                if let Some(ConstantValue::Int(val, _)) = self.constants.get(value) {
                    // Find matching case
                    for (case_val, case_block) in cases {
                        if *case_val == *val {
                            *terminator = Terminator::Branch(*case_block);
                            return true;
                        }
                    }
                    // No match, go to default
                    *terminator = Terminator::Branch(*default_block);
                    return true;
                }
                false
            }

            _ => false,
        }
    }

    /// Fold a binary operation on constant values.
    fn fold_binary_op(
        &self,
        op: BinaryOp,
        lhs: &ConstantValue,
        rhs: &ConstantValue,
    ) -> Option<ConstantValue> {
        use ConstantValue::*;

        match (op, lhs, rhs) {
            // Integer arithmetic
            (BinaryOp::Add, Int(l, ty), Int(r, _)) => Some(Int(l.wrapping_add(*r), ty.clone())),
            (BinaryOp::Sub, Int(l, ty), Int(r, _)) => Some(Int(l.wrapping_sub(*r), ty.clone())),
            (BinaryOp::Mul, Int(l, ty), Int(r, _)) => Some(Int(l.wrapping_mul(*r), ty.clone())),
            (BinaryOp::Div, Int(l, ty), Int(r, _)) => {
                if *r == 0 {
                    None // Division by zero
                } else {
                    Some(Int(l.wrapping_div(*r), ty.clone()))
                }
            }
            (BinaryOp::Rem, Int(l, ty), Int(r, _)) => {
                if *r == 0 {
                    None // Division by zero
                } else {
                    Some(Int(l.wrapping_rem(*r), ty.clone()))
                }
            }

            // Float arithmetic
            (BinaryOp::Add, Float(l, ty), Float(r, _)) => Some(Float(l + r, ty.clone())),
            (BinaryOp::Sub, Float(l, ty), Float(r, _)) => Some(Float(l - r, ty.clone())),
            (BinaryOp::Mul, Float(l, ty), Float(r, _)) => Some(Float(l * r, ty.clone())),
            (BinaryOp::Div, Float(l, ty), Float(r, _)) => Some(Float(l / r, ty.clone())),
            (BinaryOp::Rem, Float(l, ty), Float(r, _)) => Some(Float(l % r, ty.clone())),

            // Integer comparisons
            (BinaryOp::Eq, Int(l, _), Int(r, _)) => Some(Bool(l == r)),
            (BinaryOp::Ne, Int(l, _), Int(r, _)) => Some(Bool(l != r)),
            (BinaryOp::Lt, Int(l, _), Int(r, _)) => Some(Bool(l < r)),
            (BinaryOp::Le, Int(l, _), Int(r, _)) => Some(Bool(l <= r)),
            (BinaryOp::Gt, Int(l, _), Int(r, _)) => Some(Bool(l > r)),
            (BinaryOp::Ge, Int(l, _), Int(r, _)) => Some(Bool(l >= r)),

            // Float comparisons
            (BinaryOp::Eq, Float(l, _), Float(r, _)) => Some(Bool(l == r)),
            (BinaryOp::Ne, Float(l, _), Float(r, _)) => Some(Bool(l != r)),
            (BinaryOp::Lt, Float(l, _), Float(r, _)) => Some(Bool(l < r)),
            (BinaryOp::Le, Float(l, _), Float(r, _)) => Some(Bool(l <= r)),
            (BinaryOp::Gt, Float(l, _), Float(r, _)) => Some(Bool(l > r)),
            (BinaryOp::Ge, Float(l, _), Float(r, _)) => Some(Bool(l >= r)),

            // Boolean comparisons
            (BinaryOp::Eq, Bool(l), Bool(r)) => Some(Bool(l == r)),
            (BinaryOp::Ne, Bool(l), Bool(r)) => Some(Bool(l != r)),

            // Bitwise operations on integers
            (BinaryOp::And, Int(l, ty), Int(r, _)) => Some(Int(l & r, ty.clone())),
            (BinaryOp::Or, Int(l, ty), Int(r, _)) => Some(Int(l | r, ty.clone())),
            (BinaryOp::Xor, Int(l, ty), Int(r, _)) => Some(Int(l ^ r, ty.clone())),
            (BinaryOp::Shl, Int(l, ty), Int(r, _)) => {
                Some(Int(l.wrapping_shl(*r as u32), ty.clone()))
            }
            (BinaryOp::Shr, Int(l, ty), Int(r, _)) => {
                Some(Int(l.wrapping_shr(*r as u32), ty.clone()))
            }

            // Bitwise operations on booleans
            (BinaryOp::And, Bool(l), Bool(r)) => Some(Bool(*l && *r)),
            (BinaryOp::Or, Bool(l), Bool(r)) => Some(Bool(*l || *r)),
            (BinaryOp::Xor, Bool(l), Bool(r)) => Some(Bool(*l != *r)),

            _ => None,
        }
    }

    /// Fold a unary operation on a constant value.
    fn fold_unary_op(&self, op: UnaryOp, operand: &ConstantValue) -> Option<ConstantValue> {
        use ConstantValue::*;

        match (op, operand) {
            (UnaryOp::Neg, Int(v, ty)) => Some(Int(v.wrapping_neg(), ty.clone())),
            (UnaryOp::Neg, Float(v, ty)) => Some(Float(-v, ty.clone())),
            (UnaryOp::Not, Bool(v)) => Some(Bool(!v)),
            (UnaryOp::Not, Int(v, ty)) => Some(Int(!v, ty.clone())),
            _ => None,
        }
    }
}

impl Default for ConstantFolding {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for ConstantFolding {
    fn name(&self) -> &str {
        "constant-folding"
    }

    fn run_on_function(&mut self, func: &mut Function) -> PassResult {
        self.fold_in_function(func)
    }

    fn run_on_module(&mut self, module: &mut Module) -> PassResult {
        let mut changed = false;

        for func in &mut module.functions {
            if !func.is_external {
                let result = self.fold_in_function(func);
                if result.changed() {
                    changed = true;
                }
            }
        }

        if changed {
            PassResult::Changed
        } else {
            PassResult::Unchanged
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_ir::instruction::BinaryOp;
    use jet_ir::types::Ty;
    use jet_ir::values::{BlockId, Param, ValueId};
    use jet_ir::{BasicBlock, Terminator};
    use std::f64::consts::{PI, TAU};

    #[test]
    fn test_fold_add() {
        let mut func = Function::new("test", vec![], Ty::I32);
        let mut block = BasicBlock::new(BlockId::new(0));

        let v0 = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result: v0,
            value: ConstantValue::Int(10, Ty::I32),
        });

        let v1 = ValueId::new(1);
        block.add_instruction(Instruction::Const {
            result: v1,
            value: ConstantValue::Int(32, Ty::I32),
        });

        let v2 = ValueId::new(2);
        block.add_instruction(Instruction::Binary {
            result: v2,
            op: BinaryOp::Add,
            lhs: v0,
            rhs: v1,
        });

        block.set_terminator(Terminator::Return(Some(v2)));
        func.add_block(block);

        let mut pass = ConstantFolding::new();
        let result = pass.run_on_function(&mut func);

        assert!(result.changed());
        // The binary add should be folded to a const
        assert_eq!(func.blocks[0].instructions.len(), 3);
        match &func.blocks[0].instructions[2] {
            Instruction::Const { value, .. } => {
                assert_eq!(*value, ConstantValue::Int(42, Ty::I32));
            }
            _ => panic!("Expected const instruction"),
        }
    }

    #[test]
    fn test_fold_comparison() {
        let mut func = Function::new("test", vec![], Ty::Bool);
        let mut block = BasicBlock::new(BlockId::new(0));

        let v0 = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result: v0,
            value: ConstantValue::Int(5, Ty::I32),
        });

        let v1 = ValueId::new(1);
        block.add_instruction(Instruction::Const {
            result: v1,
            value: ConstantValue::Int(10, Ty::I32),
        });

        let v2 = ValueId::new(2);
        block.add_instruction(Instruction::Binary {
            result: v2,
            op: BinaryOp::Lt,
            lhs: v0,
            rhs: v1,
        });

        block.set_terminator(Terminator::Return(Some(v2)));
        func.add_block(block);

        let mut pass = ConstantFolding::new();
        let result = pass.run_on_function(&mut func);

        assert!(result.changed());
        match &func.blocks[0].instructions[2] {
            Instruction::Const { value, .. } => {
                assert_eq!(*value, ConstantValue::Bool(true));
            }
            _ => panic!("Expected const instruction"),
        }
    }

    #[test]
    fn test_simplify_cond_branch() {
        let mut func = Function::new("test", vec![], Ty::Void);

        // Entry block with constant condition
        let mut entry = BasicBlock::new(BlockId::new(0));
        let cond = ValueId::new(0);
        entry.add_instruction(Instruction::Const {
            result: cond,
            value: ConstantValue::Bool(true),
        });

        let then_bb = BlockId::new(1);
        let else_bb = BlockId::new(2);
        entry.set_terminator(Terminator::CondBranch {
            cond,
            then_block: then_bb,
            else_block: else_bb,
        });
        func.add_block(entry);

        // Then block
        let mut then_block = BasicBlock::new(then_bb);
        then_block.set_terminator(Terminator::Return(None));
        func.add_block(then_block);

        // Else block (will become unreachable)
        let mut else_block = BasicBlock::new(else_bb);
        else_block.set_terminator(Terminator::Return(None));
        func.add_block(else_block);

        let mut pass = ConstantFolding::new();
        let result = pass.run_on_function(&mut func);

        assert!(result.changed());
        // The entry block should now have an unconditional branch
        match &func.blocks[0].terminator {
            Terminator::Branch(target) => assert_eq!(*target, then_bb),
            _ => panic!("Expected unconditional branch"),
        }
    }

    #[test]
    fn test_fold_phi_constant() {
        let mut func = Function::new("test", vec![], Ty::I32);
        let mut block = BasicBlock::new(BlockId::new(0));

        let v0 = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result: v0,
            value: ConstantValue::Int(42, Ty::I32),
        });

        let v1 = ValueId::new(1);
        block.add_instruction(Instruction::Const {
            result: v1,
            value: ConstantValue::Int(42, Ty::I32),
        });

        // Phi with two identical constants
        let v2 = ValueId::new(2);
        block.add_instruction(Instruction::Phi {
            result: v2,
            incoming: vec![(BlockId::new(1), v0), (BlockId::new(2), v1)],
        });

        block.set_terminator(Terminator::Return(Some(v2)));
        func.add_block(block);

        let mut pass = ConstantFolding::new();
        let result = pass.run_on_function(&mut func);

        assert!(result.changed());
        match &func.blocks[0].instructions[2] {
            Instruction::Const { value, .. } => {
                assert_eq!(*value, ConstantValue::Int(42, Ty::I32));
            }
            _ => panic!("Expected const instruction"),
        }
    }

    #[test]
    fn test_fold_unary() {
        let mut func = Function::new("test", vec![], Ty::I32);
        let mut block = BasicBlock::new(BlockId::new(0));

        let v0 = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result: v0,
            value: ConstantValue::Int(42, Ty::I32),
        });

        let v1 = ValueId::new(1);
        block.add_instruction(Instruction::Unary {
            result: v1,
            op: UnaryOp::Neg,
            operand: v0,
        });

        block.set_terminator(Terminator::Return(Some(v1)));
        func.add_block(block);

        let mut pass = ConstantFolding::new();
        let result = pass.run_on_function(&mut func);

        assert!(result.changed());
        match &func.blocks[0].instructions[1] {
            Instruction::Const { value, .. } => {
                assert_eq!(*value, ConstantValue::Int(-42, Ty::I32));
            }
            _ => panic!("Expected const instruction"),
        }
    }

    #[test]
    fn test_no_fold_unknown() {
        // Test that we don't fold when operands are unknown
        let param = Param::new("x", Ty::I32, ValueId::new(0));
        let mut func = Function::new("test", vec![param], Ty::I32);
        let mut block = BasicBlock::new(BlockId::new(0));

        // Parameter value (not a constant)
        let param = ValueId::new(0);

        let v1 = ValueId::new(1);
        block.add_instruction(Instruction::Const {
            result: v1,
            value: ConstantValue::Int(10, Ty::I32),
        });

        let v2 = ValueId::new(2);
        block.add_instruction(Instruction::Binary {
            result: v2,
            op: BinaryOp::Add,
            lhs: param,
            rhs: v1,
        });

        block.set_terminator(Terminator::Return(Some(v2)));
        func.add_block(block);

        let mut pass = ConstantFolding::new();
        let result = pass.run_on_function(&mut func);

        // Should not change because param is not a known constant
        assert!(!result.changed());
    }

    #[test]
    fn test_fold_division_by_zero() {
        // Division by zero should not fold (returns None)
        let mut func = Function::new("test", vec![], Ty::I32);
        let mut block = BasicBlock::new(BlockId::new(0));

        let v0 = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result: v0,
            value: ConstantValue::Int(42, Ty::I32),
        });

        let v1 = ValueId::new(1);
        block.add_instruction(Instruction::Const {
            result: v1,
            value: ConstantValue::Int(0, Ty::I32),
        });

        let v2 = ValueId::new(2);
        block.add_instruction(Instruction::Binary {
            result: v2,
            op: BinaryOp::Div,
            lhs: v0,
            rhs: v1,
        });

        block.set_terminator(Terminator::Return(Some(v2)));
        func.add_block(block);

        let mut pass = ConstantFolding::new();
        let result = pass.run_on_function(&mut func);

        // Should not fold division by zero
        assert!(!result.changed());
    }

    #[test]
    fn test_fold_float() {
        let mut func = Function::new("test", vec![], Ty::F64);
        let mut block = BasicBlock::new(BlockId::new(0));

        let v0 = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result: v0,
            value: ConstantValue::Float(PI, Ty::F64),
        });

        let v1 = ValueId::new(1);
        block.add_instruction(Instruction::Const {
            result: v1,
            value: ConstantValue::Float(2.0, Ty::F64),
        });

        let v2 = ValueId::new(2);
        block.add_instruction(Instruction::Binary {
            result: v2,
            op: BinaryOp::Mul,
            lhs: v0,
            rhs: v1,
        });

        block.set_terminator(Terminator::Return(Some(v2)));
        func.add_block(block);

        let mut pass = ConstantFolding::new();
        let result = pass.run_on_function(&mut func);

        assert!(result.changed());
        match &func.blocks[0].instructions[2] {
            Instruction::Const { value, .. } => {
                if let ConstantValue::Float(v, _) = value {
                    assert!((v - TAU).abs() < 0.001);
                } else {
                    panic!("Expected float constant");
                }
            }
            _ => panic!("Expected const instruction"),
        }
    }
}
