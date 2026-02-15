//! IR builder for constructing Jet IR programmatically.
//!
//! This module provides a convenient API for building IR modules,
//! functions, and basic blocks.

use crate::function::{BasicBlock, Effect, Function};
use crate::instruction::{BinaryOp, ConstantValue, Instruction, UnaryOp};
use crate::terminator::Terminator;
use crate::types::{Ty, TypeDef, TypeKind};
use crate::values::{BlockId, Param, ValueId, ValueInfo, ValueTable};
use crate::{Global, Module};

/// Builder for constructing IR modules.
pub struct ModuleBuilder {
    module: Module,
    value_table: ValueTable,
    next_block_id: u32,
    next_value_id: u32,
}

impl ModuleBuilder {
    /// Creates a new module builder with the given module name.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            module: Module::new(name),
            value_table: ValueTable::new(),
            next_block_id: 0,
            next_value_id: 0,
        }
    }

    /// Returns the built module.
    pub fn build(self) -> Module {
        self.module
    }

    /// Gets a reference to the value table.
    pub fn value_table(&self) -> &ValueTable {
        &self.value_table
    }

    /// Allocates a new value ID.
    pub fn alloc_value(&mut self, ty: Ty) -> ValueId {
        let id = ValueId(self.next_value_id);
        self.next_value_id += 1;
        self.value_table.alloc(ValueInfo::new(ty));
        id
    }

    /// Allocates a new block ID.
    pub fn alloc_block_id(&mut self) -> BlockId {
        let id = BlockId(self.next_block_id);
        self.next_block_id += 1;
        id
    }

    /// Adds a type definition to the module.
    pub fn add_type(&mut self, name: impl Into<String>, kind: TypeKind) {
        self.module.add_type_def(TypeDef {
            name: name.into(),
            kind,
        });
    }

    /// Adds a global variable to the module.
    pub fn add_global(&mut self, global: Global) {
        self.module.add_global(global);
    }

    /// Creates a new function builder.
    pub fn function(
        &mut self,
        name: impl Into<String>,
        params: Vec<(impl Into<String>, Ty)>,
        return_ty: Ty,
    ) -> FunctionBuilder<'_> {
        let name = name.into();

        // Allocate value IDs for parameters
        let mut func_params = Vec::new();
        for (param_name, param_ty) in params {
            let value = self.alloc_value(param_ty.clone());
            func_params.push(Param::new(param_name, param_ty, value));
        }

        FunctionBuilder::new(self, name, func_params, return_ty)
    }

    /// Creates a new external function.
    pub fn external_function(
        &mut self,
        name: impl Into<String>,
        params: Vec<(impl Into<String>, Ty)>,
        return_ty: Ty,
    ) {
        let name = name.into();

        // Allocate value IDs for parameters
        let mut func_params = Vec::new();
        for (param_name, param_ty) in params {
            let value = self.alloc_value(param_ty.clone());
            func_params.push(Param::new(param_name, param_ty, value));
        }

        let func = Function::external(name, func_params, return_ty);
        self.module.add_function(func);
    }
}

/// Builder for constructing functions.
pub struct FunctionBuilder<'a> {
    module_builder: &'a mut ModuleBuilder,
    function: Function,
    current_block: Option<BlockId>,
}

impl<'a> FunctionBuilder<'a> {
    /// Creates a new function builder.
    fn new(
        module_builder: &'a mut ModuleBuilder,
        name: String,
        params: Vec<Param>,
        return_ty: Ty,
    ) -> Self {
        Self {
            module_builder,
            function: Function::new(name, params, return_ty),
            current_block: None,
        }
    }

    /// Marks this function as async.
    pub fn async_(mut self) -> Self {
        self.function.is_async = true;
        self.function.effects.push(Effect::Async);
        self
    }

    /// Adds an effect to this function.
    pub fn effect(mut self, effect: Effect) -> Self {
        self.function.effects.push(effect);
        self
    }

    /// Marks this function as exported.
    pub fn export(mut self) -> Self {
        self.function.is_exported = true;
        self
    }

    /// Creates a new basic block and sets it as current.
    pub fn block(&mut self, name: impl Into<String>) -> BlockId {
        let id = self.module_builder.alloc_block_id();
        let block = BasicBlock::with_name(id, name);
        self.function.add_block(block);
        self.current_block = Some(id);
        id
    }

    /// Sets the current block.
    pub fn set_block(&mut self, id: BlockId) -> &mut Self {
        self.current_block = Some(id);
        self
    }

    /// Gets the current block ID.
    pub fn current_block(&self) -> Option<BlockId> {
        self.current_block
    }

    /// Adds an instruction to the current block.
    fn add_instruction(&mut self, inst: Instruction) {
        let block_id = self.current_block.expect("no current block");
        let block = self.function.get_block_mut(block_id).unwrap();
        block.add_instruction(inst);
    }

    /// Sets the terminator for the current block.
    fn set_terminator(&mut self, terminator: Terminator) {
        let block_id = self.current_block.expect("no current block");
        let block = self.function.get_block_mut(block_id).unwrap();
        block.set_terminator(terminator);
    }

    // === Instruction builders ===

    /// Creates a constant integer.
    pub fn const_int(&mut self, value: i64, ty: Ty) -> ValueId {
        let result = self.module_builder.alloc_value(ty.clone());
        self.add_instruction(Instruction::Const {
            result,
            value: ConstantValue::Int(value, ty),
        });
        result
    }

    /// Creates a constant boolean.
    pub fn const_bool(&mut self, value: bool) -> ValueId {
        let result = self.module_builder.alloc_value(Ty::Bool);
        self.add_instruction(Instruction::Const {
            result,
            value: ConstantValue::Bool(value),
        });
        result
    }

    /// Creates a constant float.
    pub fn const_float(&mut self, value: f64, ty: Ty) -> ValueId {
        let result = self.module_builder.alloc_value(ty.clone());
        self.add_instruction(Instruction::Const {
            result,
            value: ConstantValue::Float(value, ty),
        });
        result
    }

    /// Creates a constant null pointer.
    pub fn const_null(&mut self, ty: Ty) -> ValueId {
        let result = self.module_builder.alloc_value(ty.clone());
        self.add_instruction(Instruction::Const {
            result,
            value: ConstantValue::Null(ty),
        });
        result
    }

    /// Binary operation.
    pub fn binary(&mut self, op: BinaryOp, lhs: ValueId, rhs: ValueId, ty: Ty) -> ValueId {
        let result = self.module_builder.alloc_value(ty);
        self.add_instruction(Instruction::Binary {
            result,
            op,
            lhs,
            rhs,
        });
        result
    }

    /// Unary operation.
    pub fn unary(&mut self, op: UnaryOp, operand: ValueId, ty: Ty) -> ValueId {
        let result = self.module_builder.alloc_value(ty);
        self.add_instruction(Instruction::Unary {
            result,
            op,
            operand,
        });
        result
    }

    /// Allocate stack space.
    pub fn alloc(&mut self, ty: Ty) -> ValueId {
        let result = self
            .module_builder
            .alloc_value(Ty::Ptr(Box::new(ty.clone())));
        self.add_instruction(Instruction::Alloc { result, ty });
        result
    }

    /// Load from a pointer.
    pub fn load(&mut self, ptr: ValueId, ty: Ty) -> ValueId {
        let result = self.module_builder.alloc_value(ty);
        self.add_instruction(Instruction::Load { result, ptr });
        result
    }

    /// Store to a pointer.
    pub fn store(&mut self, ptr: ValueId, value: ValueId) {
        self.add_instruction(Instruction::Store { ptr, value });
    }

    /// Get pointer to struct field.
    pub fn get_field_ptr(&mut self, ptr: ValueId, field_index: usize, result_ty: Ty) -> ValueId {
        let result = self.module_builder.alloc_value(result_ty);
        self.add_instruction(Instruction::GetFieldPtr {
            result,
            ptr,
            field_index,
        });
        result
    }

    /// Get pointer to array element.
    pub fn get_element_ptr(&mut self, ptr: ValueId, index: ValueId, result_ty: Ty) -> ValueId {
        let result = self.module_builder.alloc_value(result_ty);
        self.add_instruction(Instruction::GetElementPtr { result, ptr, index });
        result
    }

    /// Call a function.
    pub fn call(&mut self, func: impl Into<String>, args: Vec<ValueId>, result_ty: Ty) -> ValueId {
        let result = self.module_builder.alloc_value(result_ty);
        self.add_instruction(Instruction::Call {
            result,
            func: func.into(),
            args,
        });
        result
    }

    /// Call a function pointer.
    pub fn call_indirect(&mut self, ptr: ValueId, args: Vec<ValueId>, result_ty: Ty) -> ValueId {
        let result = self.module_builder.alloc_value(result_ty);
        self.add_instruction(Instruction::CallIndirect { result, ptr, args });
        result
    }

    /// Phi node.
    pub fn phi(&mut self, incoming: Vec<(BlockId, ValueId)>, ty: Ty) -> ValueId {
        let result = self.module_builder.alloc_value(ty);
        self.add_instruction(Instruction::Phi { result, incoming });
        result
    }

    // === Terminators ===

    /// Return void.
    pub fn ret_void(&mut self) {
        self.set_terminator(Terminator::Return(None));
    }

    /// Return a value.
    pub fn ret(&mut self, value: ValueId) {
        self.set_terminator(Terminator::Return(Some(value)));
    }

    /// Unconditional branch.
    pub fn br(&mut self, block: BlockId) {
        self.set_terminator(Terminator::Branch(block));
    }

    /// Conditional branch.
    pub fn cond_br(&mut self, cond: ValueId, then_block: BlockId, else_block: BlockId) {
        self.set_terminator(Terminator::CondBranch {
            cond,
            then_block,
            else_block,
        });
    }

    /// Switch terminator.
    pub fn switch(&mut self, value: ValueId, default_block: BlockId, cases: Vec<(i64, BlockId)>) {
        self.set_terminator(Terminator::Switch {
            value,
            default_block,
            cases,
        });
    }

    /// Unreachable.
    pub fn unreachable(&mut self) {
        self.set_terminator(Terminator::Unreachable);
    }

    /// Builds and adds the function to the module.
    pub fn build(self) {
        self.module_builder.module.add_function(self.function);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Constant;

    #[test]
    fn test_builder_basic() {
        let mut builder = ModuleBuilder::new("test");

        {
            let mut func = builder.function("main", Vec::<(&str, Ty)>::new(), Ty::I32);
            func.block("entry");
            func.const_int(42, Ty::I32);
            func.ret_void();
            func.build();
        }

        let module = builder.build();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, "main");
    }

    #[test]
    fn test_builder_arithmetic() {
        let mut builder = ModuleBuilder::new("test");

        {
            let mut func = builder.function("add", vec![("a", Ty::I32), ("b", Ty::I32)], Ty::I32);
            let _entry = func.block("entry");

            // Get parameter values
            let a = ValueId(0);
            let b = ValueId(1);

            let sum = func.binary(BinaryOp::Add, a, b, Ty::I32);
            func.ret(sum);

            func.build();
        }

        let module = builder.build();
        let func = &module.functions[0];
        assert_eq!(func.blocks[0].instructions.len(), 1);
        assert!(func.blocks[0].is_terminated());
    }

    #[test]
    fn test_builder_control_flow() {
        let mut builder = ModuleBuilder::new("test");

        {
            let mut func = builder.function("max", vec![("a", Ty::I32), ("b", Ty::I32)], Ty::I32);

            let entry = func.block("entry");
            let a = ValueId(0);
            let b = ValueId(1);
            let cond = func.binary(BinaryOp::Gt, a, b, Ty::Bool);

            let then_bb = func.block("then");
            let else_bb = func.block("else");
            let merge_bb = func.block("merge");

            // Return to entry block to add the conditional branch
            func.set_block(entry);
            func.cond_br(cond, then_bb, else_bb);

            func.set_block(then_bb);
            func.br(merge_bb);

            func.set_block(else_bb);
            func.br(merge_bb);

            func.set_block(merge_bb);
            let result = func.phi(vec![(then_bb, a), (else_bb, b)], Ty::I32);
            func.ret(result);

            func.build();
        }

        let module = builder.build();
        let func = &module.functions[0];
        assert_eq!(func.blocks.len(), 4);
        assert!(func.validate().is_ok());
    }

    #[test]
    fn test_builder_memory() {
        let mut builder = ModuleBuilder::new("test");

        {
            let mut func = builder.function("test", Vec::<(&str, Ty)>::new(), Ty::Void);
            let _entry = func.block("entry");

            let ptr = func.alloc(Ty::I32);
            let val = func.const_int(42, Ty::I32);
            func.store(ptr, val);
            let _loaded = func.load(ptr, Ty::I32);
            func.ret_void();

            func.build();
        }

        let module = builder.build();
        let func = &module.functions[0];
        assert_eq!(func.blocks[0].instructions.len(), 4);
    }

    #[test]
    fn test_builder_external_function() {
        let mut builder = ModuleBuilder::new("test");

        builder.external_function(
            "printf",
            vec![("format", Ty::Ptr(Box::new(Ty::I8)))],
            Ty::I32,
        );

        let module = builder.build();
        assert_eq!(module.functions.len(), 1);
        assert!(module.functions[0].is_external);
    }

    #[test]
    fn test_builder_global() {
        let mut builder = ModuleBuilder::new("test");

        builder.add_global(Global::new("counter", Ty::I32).with_initial_value(Constant::i32(0)));

        let module = builder.build();
        assert_eq!(module.globals.len(), 1);
        assert_eq!(module.globals[0].name, "counter");
    }
}
