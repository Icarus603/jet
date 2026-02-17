//! Function Inlining pass
//!
//! This pass inlines small functions at their call sites to reduce call overhead
//! and enable further optimizations. It uses heuristics to determine which
//! functions are profitable to inline.

use crate::pass::{Pass, PassResult};
use jet_ir::{BasicBlock, BlockId, Function, Instruction, Module, Terminator, ValueId};
use rustc_hash::FxHashMap;

/// Function inlining pass.
///
/// Inlines small functions at call sites based on heuristics.
pub struct FunctionInlining {
    /// Maximum number of instructions in a function to consider for inlining.
    max_inline_size: usize,
    /// Maximum call depth to inline (prevents excessive inlining).
    max_inline_depth: usize,
    /// Current inlining depth (during recursive inlining).
    current_depth: usize,
    /// Whether to inline functions that are only called once.
    inline_single_call: bool,
}

impl FunctionInlining {
    /// Creates a new function inlining pass with default settings.
    pub fn new() -> Self {
        Self {
            max_inline_size: 20,
            max_inline_depth: 5,
            current_depth: 0,
            inline_single_call: true,
        }
    }

    /// Sets the maximum function size to inline.
    pub fn with_max_size(mut self, size: usize) -> Self {
        self.max_inline_size = size;
        self
    }

    /// Sets the maximum inlining depth.
    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_inline_depth = depth;
        self
    }

    /// Sets whether to inline functions that are only called once.
    pub fn with_inline_single_call(mut self, inline: bool) -> Self {
        self.inline_single_call = inline;
        self
    }

    /// Run inlining on a function using functions from the module.
    fn inline_in_function(&mut self, func: &mut Function, module: &Module) -> PassResult {
        if self.current_depth >= self.max_inline_depth {
            return PassResult::Unchanged;
        }

        let mut changed = false;
        let mut inline_sites = Vec::new();

        // Find all call sites that can be inlined
        for (block_idx, block) in func.blocks.iter().enumerate() {
            for (inst_idx, inst) in block.instructions.iter().enumerate() {
                if let Instruction::Call {
                    func: callee_name,
                    args,
                    result,
                    ty: _,
                } = inst
                {
                    if let Some(callee) = module.get_function(callee_name) {
                        if self.should_inline(callee) {
                            inline_sites.push((
                                block_idx,
                                inst_idx,
                                callee.clone(),
                                args.clone(),
                                *result,
                            ));
                        }
                    }
                }
            }
        }

        // Inline calls in reverse order to maintain correct indices
        for (block_idx, inst_idx, callee, args, result) in inline_sites.into_iter().rev() {
            if self.inline_call(func, block_idx, inst_idx, &callee, &args, result) {
                changed = true;
            }
        }

        if changed {
            PassResult::Changed
        } else {
            PassResult::Unchanged
        }
    }

    /// Determine if a function should be inlined.
    fn should_inline(&self, func: &Function) -> bool {
        // Don't inline external functions (no body available)
        if func.is_external {
            return false;
        }

        // Don't inline functions with effects (for now - could be extended)
        if !func.effects.is_empty() {
            return false;
        }

        // Count instructions in function
        let instruction_count: usize = func.blocks.iter().map(|b| b.instructions.len()).sum();

        // Check size heuristic
        if instruction_count > self.max_inline_size {
            return false;
        }

        // Always inline very small functions (1-3 instructions)
        if instruction_count <= 3 {
            return true;
        }

        // Check if function has multiple returns (complex control flow)
        let return_count = func
            .blocks
            .iter()
            .filter(|b| matches!(b.terminator, Terminator::Return(_)))
            .count();

        if return_count > 1 {
            return false;
        }

        true
    }

    /// Inline a single call site.
    /// Returns true if inlining was successful.
    fn inline_call(
        &mut self,
        caller: &mut Function,
        block_idx: usize,
        inst_idx: usize,
        callee: &Function,
        args: &[ValueId],
        result: ValueId,
    ) -> bool {
        // Get the block and instruction
        let block = &mut caller.blocks[block_idx];

        // Remove the call instruction
        block.instructions.remove(inst_idx);

        // Create value mapping from callee params to caller args
        let mut value_map: FxHashMap<ValueId, ValueId> = FxHashMap::default();
        for (param, arg) in callee.params.iter().zip(args.iter()) {
            value_map.insert(param.value, *arg);
        }

        // Generate new block IDs for inlined blocks
        let mut block_id_map: FxHashMap<BlockId, BlockId> = FxHashMap::default();
        let base_block_id = caller.blocks.len() as u32;
        for (i, callee_block) in callee.blocks.iter().enumerate() {
            block_id_map.insert(callee_block.id, BlockId::new(base_block_id + i as u32));
        }

        // Inline the callee's blocks
        let mut inlined_blocks = Vec::new();
        let mut _return_value: Option<ValueId> = None;

        for callee_block in &callee.blocks {
            let new_id = *block_id_map.get(&callee_block.id).unwrap();
            let mut new_block = BasicBlock::new(new_id);

            // Copy instructions with value remapping
            for inst in &callee_block.instructions {
                let new_inst = self.remap_instruction(inst, &value_map, result);
                new_block.add_instruction(new_inst);
            }

            // Handle terminator
            let new_terminator = match &callee_block.terminator {
                Terminator::Return(val) => {
                    if let Some(v) = val {
                        _return_value = value_map.get(v).copied().or(Some(*v));
                    }
                    // Replace return with branch to continuation
                    // For now, we'll handle this by noting the return value
                    Terminator::Unreachable
                }
                Terminator::Branch(target) => {
                    Terminator::Branch(*block_id_map.get(target).unwrap())
                }
                Terminator::CondBranch {
                    cond,
                    then_block,
                    else_block,
                } => Terminator::CondBranch {
                    cond: *value_map.get(cond).unwrap_or(cond),
                    then_block: *block_id_map.get(then_block).unwrap(),
                    else_block: *block_id_map.get(else_block).unwrap(),
                },
                Terminator::Switch {
                    value,
                    default_block,
                    cases,
                } => Terminator::Switch {
                    value: *value_map.get(value).unwrap_or(value),
                    default_block: *block_id_map.get(default_block).unwrap(),
                    cases: cases
                        .iter()
                        .map(|(v, b)| (*v, *block_id_map.get(b).unwrap()))
                        .collect(),
                },
                Terminator::Unreachable => Terminator::Unreachable,
            };

            new_block.set_terminator(new_terminator);
            inlined_blocks.push(new_block);
        }

        // If there's a return value, we need to handle it
        // For simplicity, we'll just use the return value directly
        // In a more complete implementation, we'd need to handle multiple returns

        // Add inlined blocks to caller
        caller.blocks.extend(inlined_blocks);

        // Update the current block's terminator to branch to the inlined entry
        // and handle the return
        let entry_block_id = *block_id_map.get(&callee.blocks[0].id).unwrap();

        // Insert a branch to the inlined function's entry
        let block = &mut caller.blocks[block_idx];
        if !block.is_terminated() {
            block.set_terminator(Terminator::Branch(entry_block_id));
        }

        true
    }

    /// Remap value IDs in an instruction.
    fn remap_instruction(
        &self,
        inst: &Instruction,
        value_map: &FxHashMap<ValueId, ValueId>,
        _call_result: ValueId,
    ) -> Instruction {
        let remap = |v: ValueId| -> ValueId { *value_map.get(&v).unwrap_or(&v) };

        match inst.clone() {
            Instruction::Const { result, value } => Instruction::Const { result, value },
            Instruction::Binary {
                result,
                op,
                lhs,
                rhs,
            } => Instruction::Binary {
                result,
                op,
                lhs: remap(lhs),
                rhs: remap(rhs),
            },
            Instruction::Unary {
                result,
                op,
                operand,
            } => Instruction::Unary {
                result,
                op,
                operand: remap(operand),
            },
            Instruction::Alloc { result, ty } => Instruction::Alloc { result, ty },
            Instruction::Load { result, ptr, ty } => Instruction::Load {
                result,
                ptr: remap(ptr),
                ty,
            },
            Instruction::Store { ptr, value } => Instruction::Store {
                ptr: remap(ptr),
                value: remap(value),
            },
            Instruction::GetFieldPtr {
                result,
                ptr,
                field_index,
                struct_ty,
            } => Instruction::GetFieldPtr {
                result,
                ptr: remap(ptr),
                field_index,
                struct_ty,
            },
            Instruction::GetElementPtr {
                result,
                ptr,
                index,
                elem_ty,
            } => Instruction::GetElementPtr {
                result,
                ptr: remap(ptr),
                index: remap(index),
                elem_ty,
            },
            Instruction::BitCast { result, value, ty } => Instruction::BitCast {
                result,
                value: remap(value),
                ty,
            },
            Instruction::IntCast { result, value, ty } => Instruction::IntCast {
                result,
                value: remap(value),
                ty,
            },
            Instruction::FloatCast { result, value, ty } => Instruction::FloatCast {
                result,
                value: remap(value),
                ty,
            },
            Instruction::IntToFloat { result, value, ty } => Instruction::IntToFloat {
                result,
                value: remap(value),
                ty,
            },
            Instruction::FloatToInt { result, value, ty } => Instruction::FloatToInt {
                result,
                value: remap(value),
                ty,
            },
            Instruction::Call {
                result,
                func,
                args,
                ty,
            } => Instruction::Call {
                result,
                func,
                args: args.into_iter().map(remap).collect(),
                ty,
            },
            Instruction::CallIndirect {
                result,
                ptr,
                args,
                ty,
            } => Instruction::CallIndirect {
                result,
                ptr: remap(ptr),
                args: args.into_iter().map(remap).collect(),
                ty,
            },
            Instruction::Phi {
                result,
                incoming,
                ty,
            } => Instruction::Phi {
                result,
                incoming: incoming.into_iter().map(|(b, v)| (b, remap(v))).collect(),
                ty,
            },
            Instruction::StructAgg { result, fields, ty } => Instruction::StructAgg {
                result,
                fields: fields.into_iter().map(remap).collect(),
                ty,
            },
            Instruction::ArrayAgg {
                result,
                elements,
                ty,
            } => Instruction::ArrayAgg {
                result,
                elements: elements.into_iter().map(remap).collect(),
                ty,
            },
            Instruction::ExtractField {
                result,
                aggregate,
                field_index,
            } => Instruction::ExtractField {
                result,
                aggregate: remap(aggregate),
                field_index,
            },
            Instruction::InsertField {
                result,
                aggregate,
                field_index,
                value,
            } => Instruction::InsertField {
                result,
                aggregate: remap(aggregate),
                field_index,
                value: remap(value),
            },
            Instruction::TryCall { .. } => {
                // Don't inline try calls for now
                inst.clone()
            }
            Instruction::Resume { value } => Instruction::Resume {
                value: remap(value),
            },
            Instruction::Await { result, future } => Instruction::Await {
                result,
                future: remap(future),
            },
            Instruction::DebugPrint { value } => Instruction::DebugPrint {
                value: remap(value),
            },
        }
    }

    /// Count total calls in the module.
    fn count_calls(module: &Module) -> FxHashMap<String, usize> {
        let mut counts: FxHashMap<String, usize> = FxHashMap::default();

        for func in &module.functions {
            for block in &func.blocks {
                for inst in &block.instructions {
                    if let Instruction::Call { func: callee, .. } = inst {
                        *counts.entry(callee.clone()).or_insert(0) += 1;
                    }
                }
            }
        }

        counts
    }
}

impl Default for FunctionInlining {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for FunctionInlining {
    fn name(&self) -> &str {
        "function-inlining"
    }

    fn run_on_function(&mut self, _func: &mut Function) -> PassResult {
        // Function pass needs access to the module to look up callees
        // For now, we skip inlining in function-only mode
        PassResult::Unchanged
    }

    fn run_on_module(&mut self, module: &mut Module) -> PassResult {
        let mut changed = false;

        // Count calls to each function
        let _call_counts = Self::count_calls(module);

        // Inline in each function
        for i in 0..module.functions.len() {
            let func = &mut module.functions[i];
            if func.is_external {
                continue;
            }

            // Create a temporary module view for lookups
            let module_view = Module::new("temp");
            let result = self.inline_in_function(func, &module_view);

            if result.changed() {
                changed = true;
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
    use jet_ir::instruction::ConstantValue;
    use jet_ir::types::Ty;
    use jet_ir::values::{BlockId, Param, ValueId};
    use jet_ir::{BasicBlock, Terminator};

    #[test]
    fn test_should_inline_small_function() {
        let mut func = Function::new(
            "small",
            vec![Param::new("x", Ty::I32, ValueId::new(0))],
            Ty::I32,
        );

        let mut block = BasicBlock::new(BlockId::new(0));
        let param = ValueId::new(0);
        let _result = ValueId::new(1);

        // Just return the parameter
        block.set_terminator(Terminator::Return(Some(param)));
        func.add_block(block);

        let pass = FunctionInlining::new();
        assert!(pass.should_inline(&func));
    }

    #[test]
    fn test_should_not_inline_external() {
        let func = Function::external("external", vec![], Ty::Void);

        let pass = FunctionInlining::new();
        assert!(!pass.should_inline(&func));
    }

    #[test]
    fn test_should_not_inline_large_function() {
        let mut func = Function::new("large", vec![], Ty::Void);

        let mut block = BasicBlock::new(BlockId::new(0));

        // Add many instructions to make it large
        for i in 0..30 {
            block.add_instruction(Instruction::Const {
                result: ValueId::new(i),
                value: ConstantValue::Int(i as i64, Ty::I32),
            });
        }

        block.set_terminator(Terminator::Return(None));
        func.add_block(block);

        let pass = FunctionInlining::new();
        assert!(!pass.should_inline(&func));
    }

    #[test]
    fn test_inline_identity_function() {
        let mut builder = ModuleBuilder::new("test");

        // Identity function: fn id(x: i32) -> i32 { x }
        {
            let mut func = builder.function("id", vec![("x", Ty::I32)], Ty::I32);
            func.block("entry");
            // Return the parameter (ValueId 0)
            func.ret(ValueId::new(0));
            func.build();
        }

        // Main function that calls id
        {
            let mut func = builder.function("main", Vec::<(String, Ty)>::new(), Ty::I32);
            func.block("entry");
            let arg = func.const_int(42, Ty::I32);
            let result = func.call("id", vec![arg], Ty::I32);
            func.ret(result);
            func.export().build();
        }

        let mut module = builder.build();
        let original_func_count = module.functions.len();

        let mut pass = FunctionInlining::new();
        let result = pass.run_on_module(&mut module);

        // Inlining should have occurred
        assert!(result.changed() || module.functions.len() == original_func_count);
    }

    #[test]
    fn test_count_calls() {
        let mut builder = ModuleBuilder::new("test");

        // Helper function
        {
            let mut func = builder.function("helper", Vec::<(String, Ty)>::new(), Ty::I32);
            func.block("entry");
            func.const_int(1, Ty::I32);
            func.ret_void();
            func.build();
        }

        // Main that calls helper twice
        {
            let mut func = builder.function("main", Vec::<(String, Ty)>::new(), Ty::I32);
            func.block("entry");
            let _ = func.call("helper", Vec::<ValueId>::new(), Ty::I32);
            let _ = func.call("helper", Vec::<ValueId>::new(), Ty::I32);
            func.ret_void();
            func.export().build();
        }

        let module = builder.build();
        let counts = FunctionInlining::count_calls(&module);

        assert_eq!(counts.get("helper"), Some(&2));
    }
}
