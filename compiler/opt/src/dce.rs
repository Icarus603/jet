//! Dead Code Elimination (DCE) pass
//!
//! This pass removes unreachable code and unused instructions from functions.
//! It operates in two phases:
//!
//! 1. Mark phase: Mark all reachable instructions starting from exports
//! 2. Sweep phase: Remove unmarked instructions and unreachable blocks

use crate::pass::{Pass, PassResult};
use jet_ir::{BlockId, Function, Instruction, Module, ValueId};
use rustc_hash::FxHashSet;

/// Dead Code Elimination pass.
///
/// Removes unreachable blocks and unused instructions from functions.
/// Preserves instructions with side effects and those whose results are used.
pub struct DeadCodeElimination {
    /// Whether to remove unreachable functions (not just instructions within functions).
    remove_unreachable_functions: bool,
}

impl DeadCodeElimination {
    /// Creates a new DCE pass.
    pub fn new() -> Self {
        Self {
            remove_unreachable_functions: true,
        }
    }

    /// Creates a new DCE pass that doesn't remove unreachable functions.
    pub fn without_function_removal() -> Self {
        Self {
            remove_unreachable_functions: false,
        }
    }

    /// Run DCE on a single function.
    fn eliminate_in_function(&self, func: &mut Function) -> PassResult {
        let mut changed = false;

        // Phase 1: Compute liveness
        let live_values = self.compute_live_values(func);
        let reachable_blocks = self.compute_reachable_blocks(func);

        // Phase 2: Remove dead instructions from reachable blocks
        for block in &mut func.blocks {
            if !reachable_blocks.contains(&block.id) {
                continue;
            }

            let original_len = block.instructions.len();
            block
                .instructions
                .retain(|inst| self.should_keep_instruction(inst, &live_values));

            if block.instructions.len() != original_len {
                changed = true;
            }
        }

        // Phase 3: Remove unreachable blocks
        let original_block_count = func.blocks.len();
        func.blocks.retain(|b| reachable_blocks.contains(&b.id));

        if func.blocks.len() != original_block_count {
            changed = true;
        }

        // Phase 4: Remove unused block parameters
        for block in &mut func.blocks {
            let original_param_count = block.params.len();
            // Keep all params for now - more sophisticated analysis would track usage
            if block.params.len() != original_param_count {
                changed = true;
            }
        }

        if changed {
            PassResult::Changed
        } else {
            PassResult::Unchanged
        }
    }

    /// Compute which values are live (used) in the function.
    fn compute_live_values(&self, func: &Function) -> FxHashSet<ValueId> {
        let mut live = FxHashSet::default();
        let mut worklist = Vec::new();

        // Seed with values used by terminators and side-effecting instructions
        for block in &func.blocks {
            // Add terminator operands
            for val in block.terminator.operands() {
                if live.insert(val) {
                    worklist.push(val);
                }
            }

            // Add operands of instructions with side effects
            for inst in &block.instructions {
                if inst.has_side_effects() {
                    for val in inst.operands() {
                        if live.insert(val) {
                            worklist.push(val);
                        }
                    }
                }
            }
        }

        // Backward propagation: mark instructions that produce live values
        while let Some(val) = worklist.pop() {
            // Find which instruction produces this value
            for block in &func.blocks {
                for inst in &block.instructions {
                    if inst.result() == Some(val) {
                        // Mark all operands of this instruction as live
                        for operand in inst.operands() {
                            if live.insert(operand) {
                                worklist.push(operand);
                            }
                        }
                        break;
                    }
                }
            }
        }

        live
    }

    /// Compute which blocks are reachable from the entry block.
    fn compute_reachable_blocks(&self, func: &Function) -> FxHashSet<BlockId> {
        let mut reachable = FxHashSet::default();
        let mut worklist = Vec::new();

        // Start from entry block
        if let Some(entry) = func.entry_block() {
            reachable.insert(entry.id);
            worklist.push(entry.id);
        }

        // BFS through the CFG
        while let Some(block_id) = worklist.pop() {
            if let Some(block) = func.get_block(block_id) {
                for succ in block.successors() {
                    if reachable.insert(succ) {
                        worklist.push(succ);
                    }
                }
            }
        }

        reachable
    }

    /// Determine if an instruction should be kept.
    fn should_keep_instruction(
        &self,
        inst: &Instruction,
        live_values: &FxHashSet<ValueId>,
    ) -> bool {
        // Always keep instructions with side effects
        if inst.has_side_effects() {
            return true;
        }

        // Keep instructions whose result is used
        if let Some(result) = inst.result() {
            return live_values.contains(&result);
        }

        // Instructions without results and without side effects are dead
        false
    }

    /// Find all functions reachable from exported functions.
    fn find_reachable_functions(&self, module: &Module) -> FxHashSet<String> {
        let mut reachable = FxHashSet::default();
        let mut worklist = Vec::new();

        // Seed with exported functions
        for func in &module.functions {
            if (func.is_exported || func.name == "main") && reachable.insert(func.name.clone()) {
                worklist.push(func.name.clone());
            }
        }

        // Traverse call graph
        while let Some(func_name) = worklist.pop() {
            if let Some(func) = module.get_function(&func_name) {
                for block in &func.blocks {
                    for inst in &block.instructions {
                        if let Instruction::Call { func: callee, .. } = inst {
                            if reachable.insert(callee.clone()) {
                                worklist.push(callee.clone());
                            }
                        }
                    }
                }
            }
        }

        reachable
    }
}

impl Default for DeadCodeElimination {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for DeadCodeElimination {
    fn name(&self) -> &str {
        "dead-code-elimination"
    }

    fn run_on_function(&mut self, func: &mut Function) -> PassResult {
        self.eliminate_in_function(func)
    }

    fn run_on_module(&mut self, module: &mut Module) -> PassResult {
        let mut changed = false;

        // Remove unreachable functions if enabled
        if self.remove_unreachable_functions {
            let reachable = self.find_reachable_functions(module);
            let original_count = module.functions.len();

            module
                .functions
                .retain(|f| f.is_external || reachable.contains(&f.name));

            if module.functions.len() != original_count {
                changed = true;
            }
        }

        // Run DCE on each function
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
    use jet_ir::builder::ModuleBuilder;
    use jet_ir::instruction::{BinaryOp, ConstantValue};
    use jet_ir::types::Ty;
    use jet_ir::values::{BlockId, ValueId};
    use jet_ir::{BasicBlock, Terminator};

    #[test]
    fn test_remove_dead_instruction() {
        let mut func = Function::new("test", vec![], Ty::Void);
        let mut block = BasicBlock::new(BlockId::new(0));

        // Add a dead instruction (no side effects, result unused)
        let result = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result,
            value: ConstantValue::Int(42, Ty::I32),
        });

        // Add a store (has side effects, should be kept)
        let ptr = ValueId::new(1);
        let val = ValueId::new(2);
        block.add_instruction(Instruction::Store { ptr, value: val });

        block.set_terminator(Terminator::Return(None));
        func.add_block(block);

        let mut dce = DeadCodeElimination::new();
        let result = dce.run_on_function(&mut func);

        // The const should be eliminated, store should remain
        assert!(result.changed());
        assert_eq!(func.blocks[0].instructions.len(), 1);
    }

    #[test]
    fn test_keep_live_instruction() {
        let mut func = Function::new("test", vec![], Ty::I32);
        let mut block = BasicBlock::new(BlockId::new(0));

        // Add a const whose result is used by return
        let result = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result,
            value: ConstantValue::Int(42, Ty::I32),
        });

        block.set_terminator(Terminator::Return(Some(result)));
        func.add_block(block);

        let mut dce = DeadCodeElimination::new();
        let result = dce.run_on_function(&mut func);

        // The const should be kept because its result is used
        assert!(!result.changed());
        assert_eq!(func.blocks[0].instructions.len(), 1);
    }

    #[test]
    fn test_remove_unreachable_block() {
        let mut func = Function::new("test", vec![], Ty::Void);

        // Entry block that returns immediately
        let mut entry = BasicBlock::new(BlockId::new(0));
        entry.set_terminator(Terminator::Return(None));
        func.add_block(entry);

        // Unreachable block
        let mut dead = BasicBlock::new(BlockId::new(1));
        dead.set_terminator(Terminator::Return(None));
        func.add_block(dead);

        let mut dce = DeadCodeElimination::new();
        let result = dce.run_on_function(&mut func);

        assert!(result.changed());
        assert_eq!(func.blocks.len(), 1);
    }

    #[test]
    fn test_remove_unused_function() {
        let mut builder = ModuleBuilder::new("test");

        // Exported function
        {
            let mut func = builder.function("main", Vec::<(String, Ty)>::new(), Ty::Void);
            func.block("entry");
            func.ret_void();
            func.export().build();
        }

        // Unused function (not exported, not called)
        {
            let mut func = builder.function("unused", Vec::<(String, Ty)>::new(), Ty::Void);
            func.block("entry");
            func.ret_void();
            func.build();
        }

        let mut module = builder.build();
        assert_eq!(module.functions.len(), 2);

        let mut dce = DeadCodeElimination::new();
        let result = dce.run_on_module(&mut module);

        assert!(result.changed());
        assert_eq!(module.functions.len(), 1);
        assert!(module.get_function("main").is_some());
        assert!(module.get_function("unused").is_none());
    }

    #[test]
    fn test_keep_called_function() {
        let mut builder = ModuleBuilder::new("test");

        // Helper function
        {
            let mut func = builder.function("helper", Vec::<(String, Ty)>::new(), Ty::I32);
            func.block("entry");
            func.const_int(42, Ty::I32);
            func.ret_void();
            func.build();
        }

        // Main function that calls helper
        {
            let mut func = builder.function("main", Vec::<(String, Ty)>::new(), Ty::I32);
            func.block("entry");
            let result = func.call("helper", Vec::<ValueId>::new(), Ty::I32);
            func.ret(result);
            func.export().build();
        }

        let mut module = builder.build();

        let mut dce = DeadCodeElimination::new();
        let _result = dce.run_on_module(&mut module);

        // Both functions should be kept (even if DCE removes dead instructions within them)
        assert_eq!(module.functions.len(), 2);
        assert!(module.get_function("main").is_some());
        assert!(module.get_function("helper").is_some());
    }

    #[test]
    fn test_chain_of_dead_instructions() {
        let mut func = Function::new("test", vec![], Ty::Void);
        let mut block = BasicBlock::new(BlockId::new(0));

        // Chain: %0 = const 1, %1 = const 2, %2 = add %0, %1
        // None of these are used, so all should be removed
        let v0 = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result: v0,
            value: ConstantValue::Int(1, Ty::I32),
        });

        let v1 = ValueId::new(1);
        block.add_instruction(Instruction::Const {
            result: v1,
            value: ConstantValue::Int(2, Ty::I32),
        });

        let v2 = ValueId::new(2);
        block.add_instruction(Instruction::Binary {
            result: v2,
            op: BinaryOp::Add,
            lhs: v0,
            rhs: v1,
        });

        block.set_terminator(Terminator::Return(None));
        func.add_block(block);

        let mut dce = DeadCodeElimination::new();
        let result = dce.run_on_function(&mut func);

        assert!(result.changed());
        assert_eq!(func.blocks[0].instructions.len(), 0);
    }
}
