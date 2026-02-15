//! Control Flow Simplification pass
//!
//! This pass simplifies and optimizes the control flow graph by:
//!
//! - Merging empty blocks with their successors
//! - Removing unreachable blocks
//! - Simplifying single-branch switches
//! - Converting branches to the same target into unconditional branches
//! - Removing redundant jumps

use crate::pass::{Pass, PassResult};
use jet_ir::{BlockId, Function, Module, Terminator};
use rustc_hash::{FxHashMap, FxHashSet};

/// Control Flow Simplification pass.
///
/// Optimizes control flow by simplifying the CFG structure.
pub struct ControlFlowSimplification {
    /// Whether to merge empty blocks.
    merge_empty_blocks: bool,
    /// Whether to remove unreachable blocks.
    remove_unreachable: bool,
    /// Whether to simplify single-branch switches.
    simplify_switches: bool,
}

impl ControlFlowSimplification {
    /// Creates a new CFG simplification pass with all optimizations enabled.
    pub fn new() -> Self {
        Self {
            merge_empty_blocks: true,
            remove_unreachable: true,
            simplify_switches: true,
        }
    }

    /// Creates a new pass with only specific optimizations.
    pub fn with_options(
        merge_empty: bool,
        remove_unreachable: bool,
        simplify_switches: bool,
    ) -> Self {
        Self {
            merge_empty_blocks: merge_empty,
            remove_unreachable,
            simplify_switches,
        }
    }

    /// Run simplification on a function.
    fn simplify_function(&mut self, func: &mut Function) -> PassResult {
        let mut changed = false;
        let mut iterations = 0;
        const MAX_ITERATIONS: usize = 10;

        // Run until fixed point or max iterations
        loop {
            let mut iteration_changed = false;

            if self.merge_empty_blocks && self.merge_empty_blocks_in_function(func) {
                iteration_changed = true;
            }

            if self.simplify_switches && self.simplify_switches_in_function(func) {
                iteration_changed = true;
            }

            if self.simplify_conditional_branches(func) {
                iteration_changed = true;
            }

            if self.remove_redundant_jumps(func) {
                iteration_changed = true;
            }

            if self.remove_empty_blocks(func) {
                iteration_changed = true;
            }

            iterations += 1;

            if iteration_changed {
                changed = true;
            }

            if !iteration_changed || iterations >= MAX_ITERATIONS {
                break;
            }
        }

        // Remove unreachable blocks (do this once at the end)
        if self.remove_unreachable && self.remove_unreachable_blocks(func) {
            changed = true;
        }

        if changed {
            PassResult::Changed
        } else {
            PassResult::Unchanged
        }
    }

    /// Merge empty blocks with their successors.
    fn merge_empty_blocks_in_function(&self, func: &mut Function) -> bool {
        let mut changed = false;
        let mut merges: Vec<(BlockId, BlockId)> = Vec::new();

        // Find empty blocks (blocks with only a terminator)
        for block in &func.blocks {
            if block.instructions.is_empty() {
                if let Terminator::Branch(target) = block.terminator {
                    // Don't merge if this is the entry block or if it has multiple predecessors
                    let pred_count = func
                        .blocks
                        .iter()
                        .filter(|b| b.successors().contains(&block.id))
                        .count();
                    if pred_count <= 1 && func.entry_block().map(|b| b.id) != Some(block.id) {
                        merges.push((block.id, target));
                    }
                }
            }
        }

        // Apply merges
        for (empty_block, target) in merges {
            // Redirect all predecessors of empty_block to target
            for block in &mut func.blocks {
                Self::replace_successor(&mut block.terminator, empty_block, target);
            }
            changed = true;
        }

        changed
    }

    /// Simplify switches with only a default case or single case.
    fn simplify_switches_in_function(&self, func: &mut Function) -> bool {
        let mut changed = false;

        for block in &mut func.blocks {
            if let Terminator::Switch {
                value: _,
                default_block,
                cases,
            } = &block.terminator
            {
                // If no cases, convert to unconditional branch
                if cases.is_empty() {
                    block.terminator = Terminator::Branch(*default_block);
                    changed = true;
                    continue;
                }

                // If only one case that's the same as default, convert to unconditional branch
                if cases.len() == 1 && cases[0].1 == *default_block {
                    block.terminator = Terminator::Branch(*default_block);
                    changed = true;
                    continue;
                }
            }
        }

        changed
    }

    /// Simplify conditional branches where both targets are the same.
    fn simplify_conditional_branches(&self, func: &mut Function) -> bool {
        let mut changed = false;

        for block in &mut func.blocks {
            if let Terminator::CondBranch {
                then_block,
                else_block,
                ..
            } = block.terminator
            {
                if then_block == else_block {
                    block.terminator = Terminator::Branch(then_block);
                    changed = true;
                }
            }
        }

        changed
    }

    /// Remove redundant jumps (branch to block that just branches).
    fn remove_redundant_jumps(&self, func: &mut Function) -> bool {
        let mut changed = false;

        // Build a map of block IDs to their terminators
        let terminator_map: FxHashMap<BlockId, Terminator> = func
            .blocks
            .iter()
            .map(|b| (b.id, b.terminator.clone()))
            .collect();

        for block in &mut func.blocks {
            if let Terminator::Branch(target) = block.terminator {
                // Check if target is just a branch
                if let Some(Terminator::Branch(next_target)) = terminator_map.get(&target) {
                    if target != *next_target {
                        // Skip the intermediate block
                        block.terminator = Terminator::Branch(*next_target);
                        changed = true;
                    }
                }
            }
        }

        changed
    }

    /// Remove truly empty blocks that have no effect.
    fn remove_empty_blocks(&self, func: &mut Function) -> bool {
        let mut changed = false;
        let mut to_remove: FxHashSet<BlockId> = FxHashSet::default();

        // Find blocks that are empty and have a single predecessor
        for block in &func.blocks {
            if block.instructions.is_empty() {
                let preds: Vec<BlockId> = func
                    .blocks
                    .iter()
                    .filter(|b| b.successors().contains(&block.id))
                    .map(|b| b.id)
                    .collect();
                if preds.len() == 1 && preds[0] != block.id {
                    // Only remove if the predecessor has a simple terminator
                    if let Some(pred) = func.blocks.iter().find(|b| b.id == preds[0]) {
                        if matches!(pred.terminator, Terminator::Branch(_)) {
                            to_remove.insert(block.id);
                        }
                    }
                }
            }
        }

        if !to_remove.is_empty() {
            // Pre-compute the rewrite targets to avoid borrow issues
            let mut rewrite_targets: Vec<(BlockId, BlockId)> = Vec::new();
            for block in &func.blocks {
                if let Terminator::Branch(target) = block.terminator {
                    if to_remove.contains(&target) {
                        // Find where the empty block branches to
                        if let Some(empty_block) = func.get_block(target) {
                            if let Terminator::Branch(new_target) = empty_block.terminator {
                                rewrite_targets.push((block.id, new_target));
                            }
                        }
                    }
                }
            }

            // Apply the rewrites
            for (block_id, new_target) in rewrite_targets {
                if let Some(block) = func.blocks.iter_mut().find(|b| b.id == block_id) {
                    block.terminator = Terminator::Branch(new_target);
                }
            }

            // Remove the empty blocks
            func.blocks.retain(|b| !to_remove.contains(&b.id));
            changed = true;
        }

        changed
    }

    /// Remove unreachable blocks from the function.
    fn remove_unreachable_blocks(&self, func: &mut Function) -> bool {
        let mut reachable: FxHashSet<BlockId> = FxHashSet::default();
        let mut worklist: Vec<BlockId> = Vec::new();

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

        let original_count = func.blocks.len();
        func.blocks.retain(|b| reachable.contains(&b.id));

        func.blocks.len() != original_count
    }

    /// Replace a successor in a terminator.
    fn replace_successor(term: &mut Terminator, from: BlockId, to: BlockId) {
        match term {
            Terminator::Branch(target) => {
                if *target == from {
                    *target = to;
                }
            }
            Terminator::CondBranch {
                then_block,
                else_block,
                ..
            } => {
                if *then_block == from {
                    *then_block = to;
                }
                if *else_block == from {
                    *else_block = to;
                }
            }
            Terminator::Switch {
                default_block,
                cases,
                ..
            } => {
                if *default_block == from {
                    *default_block = to;
                }
                for (_, case_block) in cases {
                    if *case_block == from {
                        *case_block = to;
                    }
                }
            }
            _ => {}
        }
    }
}

impl Default for ControlFlowSimplification {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for ControlFlowSimplification {
    fn name(&self) -> &str {
        "control-flow-simplification"
    }

    fn run_on_function(&mut self, func: &mut Function) -> PassResult {
        self.simplify_function(func)
    }

    fn run_on_module(&mut self, module: &mut Module) -> PassResult {
        let mut changed = false;

        for func in &mut module.functions {
            if !func.is_external {
                let result = self.simplify_function(func);
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
    use jet_ir::instruction::ConstantValue;
    use jet_ir::types::Ty;
    use jet_ir::values::{BlockId, ValueId};
    use jet_ir::{BasicBlock, Instruction, Terminator};

    #[test]
    fn test_simplify_switch_no_cases() {
        let mut func = Function::new("test", vec![], Ty::Void);

        let mut entry = BasicBlock::new(BlockId::new(0));
        let val = ValueId::new(0);
        entry.add_instruction(Instruction::Const {
            result: val,
            value: ConstantValue::Int(0, Ty::I32),
        });
        entry.set_terminator(Terminator::Switch {
            value: val,
            default_block: BlockId::new(1),
            cases: vec![],
        });
        func.add_block(entry);

        let mut target = BasicBlock::new(BlockId::new(1));
        target.set_terminator(Terminator::Return(None));
        func.add_block(target);

        let mut pass = ControlFlowSimplification::new();
        let result = pass.run_on_function(&mut func);

        assert!(result.changed());
        // Switch should be converted to unconditional branch
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(_)));
    }

    #[test]
    fn test_simplify_cond_branch_same_target() {
        let mut func = Function::new("test", vec![], Ty::Void);

        let mut entry = BasicBlock::new(BlockId::new(0));
        let cond = ValueId::new(0);
        entry.add_instruction(Instruction::Const {
            result: cond,
            value: ConstantValue::Bool(true),
        });

        let target = BlockId::new(1);
        entry.set_terminator(Terminator::CondBranch {
            cond,
            then_block: target,
            else_block: target,
        });
        func.add_block(entry);

        let mut block1 = BasicBlock::new(target);
        block1.set_terminator(Terminator::Return(None));
        func.add_block(block1);

        let mut pass = ControlFlowSimplification::new();
        let result = pass.run_on_function(&mut func);

        assert!(result.changed());
        // CondBranch should be converted to unconditional branch
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(_)));
    }

    #[test]
    fn test_remove_unreachable_blocks() {
        let mut func = Function::new("test", vec![], Ty::Void);

        // Entry block that returns immediately
        let mut entry = BasicBlock::new(BlockId::new(0));
        entry.set_terminator(Terminator::Return(None));
        func.add_block(entry);

        // Unreachable block
        let mut dead = BasicBlock::new(BlockId::new(1));
        dead.set_terminator(Terminator::Return(None));
        func.add_block(dead);

        let mut pass = ControlFlowSimplification::new();
        let result = pass.run_on_function(&mut func);

        assert!(result.changed());
        assert_eq!(func.blocks.len(), 1);
    }

    #[test]
    fn test_remove_redundant_jump() {
        let mut func = Function::new("test", vec![], Ty::Void);

        // Entry branches to intermediate
        let mut entry = BasicBlock::new(BlockId::new(0));
        entry.set_terminator(Terminator::Branch(BlockId::new(1)));
        func.add_block(entry);

        // Intermediate branches to final
        let mut intermediate = BasicBlock::new(BlockId::new(1));
        intermediate.set_terminator(Terminator::Branch(BlockId::new(2)));
        func.add_block(intermediate);

        // Final returns
        let mut final_block = BasicBlock::new(BlockId::new(2));
        final_block.set_terminator(Terminator::Return(None));
        func.add_block(final_block);

        let mut pass = ControlFlowSimplification::new();
        let result = pass.run_on_function(&mut func);

        // Entry should now branch directly to final
        assert!(result.changed());
        if let Terminator::Branch(target) = func.blocks[0].terminator {
            assert_eq!(target, BlockId::new(2));
        } else {
            panic!("Expected unconditional branch");
        }
    }

    #[test]
    fn test_no_change_needed() {
        let mut func = Function::new("test", vec![], Ty::I32);

        let mut entry = BasicBlock::new(BlockId::new(0));
        let val = ValueId::new(0);
        entry.add_instruction(Instruction::Const {
            result: val,
            value: ConstantValue::Int(42, Ty::I32),
        });
        entry.set_terminator(Terminator::Return(Some(val)));
        func.add_block(entry);

        let mut pass = ControlFlowSimplification::new();
        let result = pass.run_on_function(&mut func);

        assert!(!result.changed());
        assert_eq!(func.blocks.len(), 1);
    }
}
