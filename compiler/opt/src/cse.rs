//! Common Subexpression Elimination (CSE) pass
//!
//! This pass eliminates redundant computations by identifying and removing
//! duplicate expressions. It uses hash-based value numbering to track
//! equivalent expressions.

use crate::pass::{Pass, PassResult};
use jet_ir::{BinaryOp, BlockId, Function, Instruction, Module, Ty, UnaryOp, ValueId};
use rustc_hash::FxHashMap;

/// A value number represents a unique computed value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ValueNumber(u32);

impl ValueNumber {
    fn new(index: u32) -> Self {
        Self(index)
    }
}

/// An expression that can be value-numbered.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Expression {
    /// A constant value.
    Const(ConstantValue),
    /// A binary operation.
    Binary {
        op: BinaryOp,
        lhs: ValueNumber,
        rhs: ValueNumber,
    },
    /// A unary operation.
    Unary { op: UnaryOp, operand: ValueNumber },
    /// A load from memory.
    Load { ptr: ValueNumber },
    /// A getelementptr operation.
    GetElementPtr {
        ptr: ValueNumber,
        index: ValueNumber,
    },
    /// A getfieldptr operation.
    GetFieldPtr {
        ptr: ValueNumber,
        field_index: usize,
    },
    /// A phi node (with sorted incoming values for canonicalization).
    Phi(Vec<(BlockId, ValueNumber)>),
    /// An expression we can't/don't want to number.
    _Opaque,
}

/// A simplified constant value for CSE.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstantValue {
    Int(i64, Ty),
    Bool(bool),
    Null(Ty),
    Undef(Ty),
    Zero(Ty),
}

impl From<jet_ir::instruction::ConstantValue> for ConstantValue {
    fn from(value: jet_ir::instruction::ConstantValue) -> Self {
        use jet_ir::instruction::ConstantValue as CV;
        match value {
            CV::Int(i, ty) => ConstantValue::Int(i, ty),
            CV::Bool(b) => ConstantValue::Bool(b),
            CV::Null(ty) => ConstantValue::Null(ty),
            CV::Undef(ty) => ConstantValue::Undef(ty),
            CV::Zero(ty) => ConstantValue::Zero(ty),
            _ => ConstantValue::Undef(Ty::Void), // Floats and strings are treated as opaque
        }
    }
}

/// Common Subexpression Elimination pass.
///
/// Eliminates redundant computations using hash-based value numbering.
pub struct CommonSubexpressionElimination {
    /// Map from value numbers to the canonical value ID.
    value_number_to_id: FxHashMap<ValueNumber, ValueId>,
    /// Map from expressions to their value numbers.
    expression_to_number: FxHashMap<Expression, ValueNumber>,
    /// Map from value IDs to their value numbers.
    id_to_number: FxHashMap<ValueId, ValueNumber>,
    /// Next value number to assign.
    next_number: u32,
    /// Whether to handle memory operations (conservative by default).
    handle_memory: bool,
}

impl CommonSubexpressionElimination {
    /// Creates a new CSE pass.
    pub fn new() -> Self {
        Self {
            value_number_to_id: FxHashMap::default(),
            expression_to_number: FxHashMap::default(),
            id_to_number: FxHashMap::default(),
            next_number: 0,
            handle_memory: false,
        }
    }

    /// Creates a new CSE pass that handles memory operations.
    pub fn with_memory_handling() -> Self {
        Self {
            value_number_to_id: FxHashMap::default(),
            expression_to_number: FxHashMap::default(),
            id_to_number: FxHashMap::default(),
            next_number: 0,
            handle_memory: true,
        }
    }

    /// Run CSE on a function.
    fn eliminate_in_function(&mut self, func: &mut Function) -> PassResult {
        self.value_number_to_id.clear();
        self.expression_to_number.clear();
        self.id_to_number.clear();
        self.next_number = 0;

        let mut changed = false;
        let mut replacements: Vec<(BlockId, usize, ValueId)> = Vec::new();

        // First pass: assign value numbers and find redundancies
        for block in &func.blocks {
            for (inst_idx, inst) in block.instructions.iter().enumerate() {
                if let Some(expr) = self.instruction_to_expression(inst) {
                    if let Some(&existing_number) = self.expression_to_number.get(&expr) {
                        // Found a redundant expression!
                        if let Some(&canonical_id) = self.value_number_to_id.get(&existing_number) {
                            if let Some(result) = inst.result() {
                                // Record replacement: result -> canonical_id
                                replacements.push((block.id, inst_idx, canonical_id));
                                // Map this value to the existing number
                                self.id_to_number.insert(result, existing_number);
                                continue;
                            }
                        }
                    }

                    // New expression - assign a new value number
                    let number = self.new_value_number();
                    self.expression_to_number.insert(expr, number);
                    if let Some(result) = inst.result() {
                        self.value_number_to_id.insert(number, result);
                        self.id_to_number.insert(result, number);
                    }
                }
            }
        }

        // Second pass: apply replacements
        // We need to be careful about the order - process in reverse to maintain indices
        for (block_id, inst_idx, _canonical_id) in replacements.iter().rev() {
            if let Some(block) = func.blocks.iter_mut().find(|b| b.id == *block_id) {
                if let Some(_inst) = block.instructions.get_mut(*inst_idx) {
                    // Replace this instruction with a copy of the canonical value
                    // For now, we just note that this value is equivalent
                    // A more complete implementation would replace uses
                    changed = true;
                }
            }
        }

        // Third pass: replace uses of redundant values
        let mut use_replacements: Vec<(ValueId, ValueId)> = Vec::new();
        for (block_id, inst_idx, canonical_id) in &replacements {
            if let Some(block) = func.blocks.iter().find(|b| b.id == *block_id) {
                if let Some(inst) = block.instructions.get(*inst_idx) {
                    if let Some(redundant_id) = inst.result() {
                        use_replacements.push((redundant_id, *canonical_id));
                    }
                }
            }
        }

        // Apply use replacements throughout the function
        for (redundant, canonical) in use_replacements {
            self.replace_uses(func, redundant, canonical);
            changed = true;
        }

        // Fourth pass: actually remove redundant instructions
        for block in &mut func.blocks {
            let original_len = block.instructions.len();
            block.instructions.retain(|inst| {
                // Keep instructions that don't have a result (side effects)
                // or that weren't marked for replacement
                if let Some(result) = inst.result() {
                    // Check if this result was replaced
                    !replacements.iter().any(|(_, _, canonical)| {
                        self.id_to_number.get(&result) == self.id_to_number.get(canonical)
                    })
                } else {
                    true
                }
            });
            if block.instructions.len() != original_len {
                changed = true;
            }
        }

        if changed {
            PassResult::Changed
        } else {
            PassResult::Unchanged
        }
    }

    /// Convert an instruction to an expression for value numbering.
    fn instruction_to_expression(&self, inst: &Instruction) -> Option<Expression> {
        match inst {
            Instruction::Const { value, .. } => Some(Expression::Const((*value).clone().into())),
            Instruction::Binary { op, lhs, rhs, .. } => {
                let lhs_num = *self.id_to_number.get(lhs)?;
                let rhs_num = *self.id_to_number.get(rhs)?;
                Some(Expression::Binary {
                    op: *op,
                    lhs: lhs_num,
                    rhs: rhs_num,
                })
            }
            Instruction::Unary { op, operand, .. } => {
                let operand_num = *self.id_to_number.get(operand)?;
                Some(Expression::Unary {
                    op: *op,
                    operand: operand_num,
                })
            }
            Instruction::Load { ptr, .. } if self.handle_memory => {
                let ptr_num = *self.id_to_number.get(ptr)?;
                Some(Expression::Load { ptr: ptr_num })
            }
            Instruction::GetElementPtr { ptr, index, .. } if self.handle_memory => {
                let ptr_num = *self.id_to_number.get(ptr)?;
                let index_num = *self.id_to_number.get(index)?;
                Some(Expression::GetElementPtr {
                    ptr: ptr_num,
                    index: index_num,
                })
            }
            Instruction::GetFieldPtr {
                ptr, field_index, ..
            } if self.handle_memory => {
                let ptr_num = *self.id_to_number.get(ptr)?;
                Some(Expression::GetFieldPtr {
                    ptr: ptr_num,
                    field_index: *field_index,
                })
            }
            Instruction::Phi { incoming, .. } => {
                let mut numbered: Vec<_> = incoming
                    .iter()
                    .filter_map(|(block, val)| self.id_to_number.get(val).map(|&num| (*block, num)))
                    .collect();
                // Sort for canonicalization
                numbered.sort_by_key(|(b, _)| b.0);
                Some(Expression::Phi(numbered))
            }
            // Don't CSE instructions with side effects or that we can't analyze
            _ => None,
        }
    }

    /// Generate a new unique value number.
    fn new_value_number(&mut self) -> ValueNumber {
        let num = ValueNumber::new(self.next_number);
        self.next_number += 1;
        num
    }

    /// Replace all uses of `from` with `to` in the function.
    fn replace_uses(&self, func: &mut Function, from: ValueId, to: ValueId) {
        for block in &mut func.blocks {
            // Replace in instructions
            for inst in &mut block.instructions {
                self.replace_in_instruction(inst, from, to);
            }
            // Replace in terminator
            self.replace_in_terminator(&mut block.terminator, from, to);
        }
    }

    /// Replace uses in an instruction.
    fn replace_in_instruction(&self, inst: &mut Instruction, from: ValueId, to: ValueId) {
        let replace = |v: &mut ValueId| {
            if *v == from {
                *v = to;
            }
        };

        match inst {
            Instruction::Binary { lhs, rhs, .. } => {
                replace(lhs);
                replace(rhs);
            }
            Instruction::Unary { operand, .. } => {
                replace(operand);
            }
            Instruction::Load { ptr, .. } => {
                replace(ptr);
            }
            Instruction::Store { ptr, value, .. } => {
                replace(ptr);
                replace(value);
            }
            Instruction::GetFieldPtr { ptr, .. } => {
                replace(ptr);
            }
            Instruction::GetElementPtr { ptr, index, .. } => {
                replace(ptr);
                replace(index);
            }
            Instruction::BitCast { value, .. } => {
                replace(value);
            }
            Instruction::IntCast { value, .. } => {
                replace(value);
            }
            Instruction::FloatCast { value, .. } => {
                replace(value);
            }
            Instruction::IntToFloat { value, .. } => {
                replace(value);
            }
            Instruction::FloatToInt { value, .. } => {
                replace(value);
            }
            Instruction::Call { args, .. } => {
                for arg in args {
                    replace(arg);
                }
            }
            Instruction::CallIndirect { ptr, args, .. } => {
                replace(ptr);
                for arg in args {
                    replace(arg);
                }
            }
            Instruction::Phi { incoming, .. } => {
                for (_, val) in incoming {
                    replace(val);
                }
            }
            Instruction::StructAgg { fields, .. } => {
                for field in fields {
                    replace(field);
                }
            }
            Instruction::ArrayAgg { elements, .. } => {
                for elem in elements {
                    replace(elem);
                }
            }
            Instruction::ExtractField { aggregate, .. } => {
                replace(aggregate);
            }
            Instruction::InsertField {
                aggregate, value, ..
            } => {
                replace(aggregate);
                replace(value);
            }
            Instruction::TryCall {
                args, failure_val, ..
            } => {
                for arg in args {
                    replace(arg);
                }
                replace(failure_val);
            }
            Instruction::Resume { value, .. } => {
                replace(value);
            }
            Instruction::Await { future, .. } => {
                replace(future);
            }
            Instruction::DebugPrint { value, .. } => {
                replace(value);
            }
            _ => {}
        }
    }

    /// Replace uses in a terminator.
    fn replace_in_terminator(&self, term: &mut jet_ir::Terminator, from: ValueId, to: ValueId) {
        use jet_ir::Terminator;

        let replace = |v: &mut ValueId| {
            if *v == from {
                *v = to;
            }
        };

        match term {
            Terminator::Return(Some(v)) => {
                replace(v);
            }
            Terminator::Return(None) => {}
            Terminator::CondBranch { cond, .. } => {
                replace(cond);
            }
            Terminator::Switch { value, .. } => {
                replace(value);
            }
            _ => {}
        }
    }
}

impl Default for CommonSubexpressionElimination {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for CommonSubexpressionElimination {
    fn name(&self) -> &str {
        "common-subexpression-elimination"
    }

    fn run_on_function(&mut self, func: &mut Function) -> PassResult {
        self.eliminate_in_function(func)
    }

    fn run_on_module(&mut self, module: &mut Module) -> PassResult {
        let mut changed = false;

        for func in &mut module.functions {
            if !func.is_external {
                let result = self.eliminate_in_function(func);
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
    use jet_ir::instruction::{BinaryOp, ConstantValue as CV};
    use jet_ir::types::Ty;
    use jet_ir::values::{BlockId, ValueId};
    use jet_ir::BasicBlock;

    #[test]
    fn test_eliminate_redundant_add() {
        let mut func = Function::new("test", vec![], Ty::I32);
        let mut block = BasicBlock::new(BlockId::new(0));

        // %0 = const 10
        let v0 = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result: v0,
            value: CV::Int(10, Ty::I32),
        });

        // %1 = const 20
        let v1 = ValueId::new(1);
        block.add_instruction(Instruction::Const {
            result: v1,
            value: CV::Int(20, Ty::I32),
        });

        // %2 = add %0, %1
        let v2 = ValueId::new(2);
        block.add_instruction(Instruction::Binary {
            result: v2,
            op: BinaryOp::Add,
            lhs: v0,
            rhs: v1,
        });

        // %3 = add %0, %1  (redundant!)
        let v3 = ValueId::new(3);
        block.add_instruction(Instruction::Binary {
            result: v3,
            op: BinaryOp::Add,
            lhs: v0,
            rhs: v1,
        });

        // Return %2
        block.set_terminator(jet_ir::Terminator::Return(Some(v2)));
        func.add_block(block);

        let mut pass = CommonSubexpressionElimination::new();
        let result = pass.run_on_function(&mut func);

        // The redundant add should be eliminated
        assert!(result.changed());
    }

    #[test]
    fn test_no_eliminate_different_ops() {
        let mut func = Function::new("test", vec![], Ty::I32);
        let mut block = BasicBlock::new(BlockId::new(0));

        // %0 = const 10
        let v0 = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result: v0,
            value: CV::Int(10, Ty::I32),
        });

        // %1 = const 20
        let v1 = ValueId::new(1);
        block.add_instruction(Instruction::Const {
            result: v1,
            value: CV::Int(20, Ty::I32),
        });

        // %2 = add %0, %1
        let v2 = ValueId::new(2);
        block.add_instruction(Instruction::Binary {
            result: v2,
            op: BinaryOp::Add,
            lhs: v0,
            rhs: v1,
        });

        // %3 = sub %0, %1  (different operation!)
        let v3 = ValueId::new(3);
        block.add_instruction(Instruction::Binary {
            result: v3,
            op: BinaryOp::Sub,
            lhs: v0,
            rhs: v1,
        });

        block.set_terminator(jet_ir::Terminator::Return(Some(v2)));
        func.add_block(block);

        let mut pass = CommonSubexpressionElimination::new();
        let result = pass.run_on_function(&mut func);

        // Should not eliminate - different operations
        assert!(!result.changed());
    }

    #[test]
    fn test_eliminate_unary() {
        let mut func = Function::new("test", vec![], Ty::I32);
        let mut block = BasicBlock::new(BlockId::new(0));

        // %0 = const 42
        let v0 = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result: v0,
            value: CV::Int(42, Ty::I32),
        });

        // %1 = neg %0
        let v1 = ValueId::new(1);
        block.add_instruction(Instruction::Unary {
            result: v1,
            op: UnaryOp::Neg,
            operand: v0,
        });

        // %2 = neg %0  (redundant!)
        let v2 = ValueId::new(2);
        block.add_instruction(Instruction::Unary {
            result: v2,
            op: UnaryOp::Neg,
            operand: v0,
        });

        block.set_terminator(jet_ir::Terminator::Return(Some(v1)));
        func.add_block(block);

        let mut pass = CommonSubexpressionElimination::new();
        let result = pass.run_on_function(&mut func);

        assert!(result.changed());
    }

    #[test]
    fn test_no_eliminate_store() {
        let mut func = Function::new("test", vec![], Ty::Void);
        let mut block = BasicBlock::new(BlockId::new(0));

        // Two stores to the same location - should NOT be eliminated
        // because stores have side effects
        let ptr = ValueId::new(0);
        let val1 = ValueId::new(1);
        let val2 = ValueId::new(2);

        block.add_instruction(Instruction::Store { ptr, value: val1 });

        block.add_instruction(Instruction::Store { ptr, value: val2 });

        block.set_terminator(jet_ir::Terminator::Return(None));
        func.add_block(block);

        let mut pass = CommonSubexpressionElimination::new();
        let result = pass.run_on_function(&mut func);

        // Should not change - stores have side effects
        assert!(!result.changed());
    }
}
