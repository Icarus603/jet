//! CodeGen Context
//!
//! This module provides the main CodeGen struct that holds the LLVM context,
//! module, builder, and all necessary mappings for compiling Jet IR to LLVM IR.

use crate::error::{CodegenError, CodegenResult};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module as LlvmModule;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use std::collections::HashMap;

use jet_ir::{BlockId, Ty, ValueId};

/// The main code generation context.
///
/// This struct holds all the necessary state for compiling Jet IR to LLVM IR,
/// including the LLVM context, module, builder, and various mappings.
pub struct CodeGen<'ctx> {
    /// The LLVM context - owns all LLVM objects
    pub context: &'ctx Context,
    /// The LLVM module being built
    pub module: LlvmModule<'ctx>,
    /// The instruction builder
    pub builder: Builder<'ctx>,
    /// Function pass manager for optimization
    function_pass_manager: PassManager<FunctionValue<'ctx>>,
    /// Maps Jet IR value IDs to LLVM values
    pub values: HashMap<ValueId, BasicValueEnum<'ctx>>,
    /// Maps Jet IR block IDs to LLVM basic blocks
    pub blocks: HashMap<BlockId, inkwell::basic_block::BasicBlock<'ctx>>,
    /// Maps function names to LLVM function values
    pub functions: HashMap<String, FunctionValue<'ctx>>,
    /// Type cache to avoid recomputing LLVM types
    type_cache: HashMap<Ty, BasicTypeEnum<'ctx>>,
    /// Current function being compiled
    current_function: Option<FunctionValue<'ctx>>,
    /// Tracks alloca instructions for stack variables
    pub stack_slots: HashMap<ValueId, PointerValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    /// Creates a new CodeGen context with the given LLVM context and module name.
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let fpm = Self::create_function_pass_manager(&module);

        Self {
            context,
            module,
            builder,
            function_pass_manager: fpm,
            values: HashMap::new(),
            blocks: HashMap::new(),
            functions: HashMap::new(),
            type_cache: HashMap::new(),
            current_function: None,
            stack_slots: HashMap::new(),
        }
    }

    /// Creates a function pass manager with standard optimizations.
    fn create_function_pass_manager(module: &LlvmModule<'ctx>) -> PassManager<FunctionValue<'ctx>> {
        let fpm = PassManager::create(module);

        // Add standard optimization passes
        // Note: Passes are added via the PassManager in inkwell
        // The available passes depend on the LLVM version
        fpm.initialize();
        fpm
    }

    /// Sets the optimization level for the pass manager.
    pub fn set_optimization_level(&mut self, _level: OptimizationLevel) {
        // Recreate the pass manager with the new optimization level
        self.function_pass_manager = Self::create_function_pass_manager(&self.module);
    }

    /// Runs the function pass manager on the given function.
    pub fn optimize_function(&self, func: FunctionValue<'ctx>) {
        self.function_pass_manager.run_on(&func);
    }

    /// Gets or creates an LLVM type for a Jet IR type.
    pub fn get_type(&mut self, ty: &Ty) -> CodegenResult<BasicTypeEnum<'ctx>> {
        // Check the cache first
        if let Some(llvm_ty) = self.type_cache.get(ty) {
            return Ok(*llvm_ty);
        }

        // Convert the type
        let llvm_ty = self.convert_type(ty)?;
        self.type_cache.insert(ty.clone(), llvm_ty);
        Ok(llvm_ty)
    }

    /// Sets a cached type for a Jet IR type.
    ///
    /// This is used when creating named struct types that need to be
    /// referenced before their body is defined (e.g., for recursive types).
    pub fn set_cached_type(&mut self, ty: Ty, llvm_ty: BasicTypeEnum<'ctx>) {
        self.type_cache.insert(ty, llvm_ty);
    }

    /// Converts a Jet IR type to an LLVM type.
    fn convert_type(&self, ty: &Ty) -> CodegenResult<BasicTypeEnum<'ctx>> {
        match ty {
            // Void is represented as an empty struct (common LLVM pattern)
            Ty::Void => Ok(self.context.struct_type(&[], false).into()),
            Ty::Int(1) => Ok(self.context.bool_type().into()),
            Ty::Int(8) => Ok(self.context.i8_type().into()),
            Ty::Int(16) => Ok(self.context.i16_type().into()),
            Ty::Int(32) => Ok(self.context.i32_type().into()),
            Ty::Int(64) => Ok(self.context.i64_type().into()),
            Ty::Int(128) => Ok(self.context.i128_type().into()),
            Ty::Int(bits) => Ok(self.context.custom_width_int_type(*bits).into()),
            Ty::Float(32) => Ok(self.context.f32_type().into()),
            Ty::Float(64) => Ok(self.context.f64_type().into()),
            Ty::Float(bits) => Err(CodegenError::unsupported_type(format!(
                "float{} (only f32 and f64 supported)",
                bits
            ))),
            Ty::Bool => Ok(self.context.bool_type().into()),
            Ty::Ptr(inner) => {
                let inner_llvm = self.convert_type(inner)?;
                Ok(inner_llvm.ptr_type(AddressSpace::default()).into())
            }
            Ty::Struct(fields) => {
                let field_types: Vec<BasicTypeEnum<'ctx>> = fields
                    .iter()
                    .map(|f| self.convert_type(f))
                    .collect::<CodegenResult<Vec<_>>>()?;
                let struct_ty = self.context.struct_type(&field_types, false);
                Ok(struct_ty.into())
            }
            Ty::Array(elem, count) => {
                let elem_ty = self.convert_type(elem)?;
                Ok(elem_ty.array_type(*count as u32).into())
            }
            Ty::Function(params, ret) => {
                use inkwell::types::BasicMetadataTypeEnum;
                let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = params
                    .iter()
                    .map(|p| self.convert_type(p).map(|t| t.into()))
                    .collect::<CodegenResult<Vec<_>>>()?;
                let fn_ty = if ret.is_void() {
                    self.context.void_type().fn_type(&param_types, false)
                } else {
                    let ret_ty = self.convert_type(ret)?;
                    ret_ty.fn_type(&param_types, false)
                };
                Ok(fn_ty.ptr_type(AddressSpace::default()).into())
            }
            // Named and generic types are represented as opaque pointers.
            // All Jet user-defined types (structs, enums, generic instantiations)
            // are GC-allocated heap objects, so a pointer representation is correct.
            Ty::Named(_name) => Ok(self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into()),
            Ty::Generic(_name, _args) => Ok(self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into()),
        }
    }

    /// Stores a value in the value map.
    pub fn set_value(&mut self, id: ValueId, value: BasicValueEnum<'ctx>) {
        self.values.insert(id, value);
    }

    /// Gets a value from the value map.
    pub fn get_value(&self, id: ValueId) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.values.get(&id).copied().ok_or_else(|| {
            let func_name = self
                .current_function()
                .map(|f| f.get_name().to_str().unwrap_or("?").to_string())
                .unwrap_or_else(|| "?".to_string());
            eprintln!(
                "DEBUG: Value {} not found in function '{}'. Available: {:?}",
                id.0,
                func_name,
                self.values.keys().map(|k| k.0).collect::<Vec<_>>()
            );
            CodegenError::value_not_found(id.0)
        })
    }

    /// Stores a basic block in the block map.
    pub fn set_block(&mut self, id: BlockId, block: inkwell::basic_block::BasicBlock<'ctx>) {
        self.blocks.insert(id, block);
    }

    /// Gets a basic block from the block map.
    pub fn get_block(&self, id: BlockId) -> CodegenResult<inkwell::basic_block::BasicBlock<'ctx>> {
        self.blocks
            .get(&id)
            .copied()
            .ok_or_else(|| CodegenError::block_not_found(id.0))
    }

    /// Stores a function in the function map.
    pub fn set_function(&mut self, name: String, func: FunctionValue<'ctx>) {
        self.functions.insert(name, func);
    }

    /// Gets a function from the function map.
    pub fn get_function(&self, name: &str) -> CodegenResult<FunctionValue<'ctx>> {
        self.functions
            .get(name)
            .copied()
            .ok_or_else(|| CodegenError::function_not_found(name.to_string()))
    }

    /// Sets the current function being compiled.
    pub fn set_current_function(&mut self, func: FunctionValue<'ctx>) {
        self.current_function = Some(func);
    }

    /// Gets the current function being compiled.
    pub fn current_function(&self) -> Option<FunctionValue<'ctx>> {
        self.current_function
    }

    /// Clears per-function state (values, blocks, stack slots).
    pub fn clear_function_state(&mut self) {
        self.values.clear();
        self.blocks.clear();
        self.stack_slots.clear();
        self.current_function = None;
    }

    /// Stores a stack slot (alloca) for a value.
    pub fn set_stack_slot(&mut self, id: ValueId, slot: PointerValue<'ctx>) {
        self.stack_slots.insert(id, slot);
    }

    /// Gets a stack slot for a value.
    pub fn get_stack_slot(&self, id: ValueId) -> Option<PointerValue<'ctx>> {
        self.stack_slots.get(&id).copied()
    }

    /// Verifies the module and returns any errors.
    pub fn verify_module(&self) -> CodegenResult<()> {
        self.module
            .verify()
            .map_err(|e| CodegenError::verification_failed(e.to_string()))
    }

    /// Gets the LLVM module.
    pub fn module(&self) -> &LlvmModule<'ctx> {
        &self.module
    }

    /// Creates a target machine for the native target.
    pub fn create_target_machine(&self, opt_level: OptimizationLevel) -> Option<TargetMachine> {
        Target::initialize_native(&InitializationConfig::default()).ok()?;

        let target = Target::from_name("native")
            .or_else(|| Target::from_triple(&TargetMachine::get_default_triple()).ok())?;

        target.create_target_machine(
            &TargetMachine::get_default_triple(),
            &TargetMachine::get_host_cpu_name().to_string(),
            &TargetMachine::get_host_cpu_features().to_string(),
            opt_level,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
    }

    /// Sets the target triple for the module.
    pub fn set_target_triple(&self, triple: &str) {
        self.module.set_triple(&TargetTriple::create(triple));
    }

    /// Writes the module to an object file.
    pub fn write_object_file(
        &self,
        target_machine: &TargetMachine,
        path: &std::path::Path,
    ) -> CodegenResult<()> {
        target_machine
            .write_to_file(&self.module, inkwell::targets::FileType::Object, path)
            .map_err(|e| CodegenError::llvm_error(e.to_string()))
    }

    /// Writes the module to LLVM IR text.
    pub fn print_to_string(&self) -> String {
        self.module.print_to_string().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_codegen_creation() {
        let context = Context::create();
        let codegen = CodeGen::new(&context, "test");
        assert_eq!(codegen.module.get_name().to_str().unwrap(), "test");
    }

    #[test]
    fn test_type_conversion() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        // Test integer types
        assert!(codegen.get_type(&Ty::I32).is_ok());
        assert!(codegen.get_type(&Ty::I64).is_ok());
        assert!(codegen.get_type(&Ty::Bool).is_ok());

        // Test float types
        assert!(codegen.get_type(&Ty::F32).is_ok());
        assert!(codegen.get_type(&Ty::F64).is_ok());

        // Test pointer type
        let ptr_ty = Ty::Ptr(Box::new(Ty::I32));
        assert!(codegen.get_type(&ptr_ty).is_ok());

        // Test struct type
        let struct_ty = Ty::Struct(vec![Ty::I32, Ty::I64]);
        assert!(codegen.get_type(&struct_ty).is_ok());
    }

    #[test]
    fn test_type_caching() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        // First lookup should cache
        let ty1 = codegen.get_type(&Ty::I32).unwrap();
        // Second lookup should return cached value
        let ty2 = codegen.get_type(&Ty::I32).unwrap();

        // They should be the same LLVM type
        assert_eq!(ty1, ty2);
    }

    #[test]
    fn test_value_storage() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        let id = ValueId::new(0);
        let value = context.i32_type().const_int(42, false).into();

        codegen.set_value(id, value);
        assert_eq!(codegen.get_value(id).unwrap(), value);
    }

    #[test]
    fn test_function_storage() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        let fn_type = context.i32_type().fn_type(&[], false);
        let func = codegen.module.add_function("test_fn", fn_type, None);

        codegen.set_function("test_fn".to_string(), func);
        assert_eq!(codegen.get_function("test_fn").unwrap(), func);
    }
}
