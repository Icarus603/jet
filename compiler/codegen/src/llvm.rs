//! LLVM Backend for Jet IR.
//!
//! This module provides the LLVM code generation backend for the Jet compiler.
//! It translates Jet IR into LLVM IR using the inkwell crate.

use crate::error::{CodegenError, CodegenResult};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use std::collections::HashMap;

/// The LLVM backend for code generation.
///
/// This struct manages the LLVM context and provides methods for compiling
/// Jet IR modules to LLVM IR.
pub struct LLVMBackend {
    context: Context,
}

impl LLVMBackend {
    /// Creates a new LLVM backend with a fresh context.
    pub fn new() -> Self {
        Self {
            context: Context::create(),
        }
    }

    /// Initializes LLVM targets.
    ///
    /// This must be called before any target-specific operations.
    pub fn initialize_targets() {
        Target::initialize_native(&InitializationConfig::default())
            .expect("Failed to initialize native target");
    }

    /// Compiles a Jet IR module to an LLVM module.
    ///
    /// # Arguments
    ///
    /// * `ir_module` - The Jet IR module to compile
    ///
    /// # Returns
    ///
    /// Returns the compiled LLVM module on success, or a `CodegenError` on failure.
    pub fn compile_module<'ctx>(
        &'ctx self,
        ir_module: &jet_ir::Module,
    ) -> CodegenResult<Module<'ctx>> {
        let module = self.context.create_module(&ir_module.name);
        let builder = self.context.create_builder();

        // Create a function compiler to handle all function compilation
        let mut func_compiler = FunctionCompiler::new(&self.context, &module, &builder);

        // Compile all functions
        for func in &ir_module.functions {
            func_compiler.compile_function(func)?;
        }

        // Verify the module
        if let Err(err) = module.verify() {
            return Err(CodegenError::verification_failed(err.to_string()));
        }

        Ok(module)
    }

    /// Gets a reference to the LLVM context.
    pub fn context(&self) -> &Context {
        &self.context
    }

    /// Creates a target machine for the native target.
    pub fn create_target_machine(&self) -> Option<TargetMachine> {
        let target = Target::from_name("native")
            .or_else(|| Target::from_triple(&TargetMachine::get_default_triple()).ok())?;

        target.create_target_machine(
            &TargetMachine::get_default_triple(),
            &TargetMachine::get_host_cpu_name().to_string(),
            &TargetMachine::get_host_cpu_features().to_string(),
            OptimizationLevel::Default,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
    }
}

impl Default for LLVMBackend {
    fn default() -> Self {
        Self::new()
    }
}

/// Compiles Jet IR functions to LLVM functions.
struct FunctionCompiler<'ctx, 'module> {
    context: &'ctx Context,
    module: &'module Module<'ctx>,
    builder: &'module Builder<'ctx>,
    /// Maps Jet IR value IDs to LLVM values
    value_map: HashMap<jet_ir::ValueId, BasicValueEnum<'ctx>>,
    /// Maps Jet IR block IDs to LLVM basic blocks
    block_map: HashMap<jet_ir::BlockId, inkwell::basic_block::BasicBlock<'ctx>>,
    /// Maps Jet IR function names to LLVM function values
    function_map: HashMap<String, FunctionValue<'ctx>>,
    /// Current function being compiled
    current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx, 'module> FunctionCompiler<'ctx, 'module> {
    fn new(
        context: &'ctx Context,
        module: &'module Module<'ctx>,
        builder: &'module Builder<'ctx>,
    ) -> Self {
        Self {
            context,
            module,
            builder,
            value_map: HashMap::new(),
            block_map: HashMap::new(),
            function_map: HashMap::new(),
            current_function: None,
        }
    }

    /// Compiles a Jet IR function to an LLVM function.
    fn compile_function(&mut self, func: &jet_ir::Function) -> CodegenResult<FunctionValue<'ctx>> {
        // Create function type
        let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = func
            .params
            .iter()
            .map(|p| {
                self.jet_type_to_llvm(&p.ty)
                    .map(|t| t.as_basic_type_enum().into())
            })
            .collect::<CodegenResult<Vec<_>>>()?;

        let fn_type = if func.return_ty.is_void() {
            self.context.void_type().fn_type(&param_types, false)
        } else {
            let ret_type = self.jet_type_to_llvm(&func.return_ty)?;
            ret_type.fn_type(&param_types, false)
        };

        let fn_val = self.module.add_function(&func.name, fn_type, None);
        self.function_map.insert(func.name.clone(), fn_val);
        self.current_function = Some(fn_val);

        // If external, no need to compile body
        if func.is_external {
            return Ok(fn_val);
        }

        // Clear per-function maps
        self.value_map.clear();
        self.block_map.clear();

        // Map function parameters to value IDs
        for (i, param) in func.params.iter().enumerate() {
            let llvm_param = fn_val.get_nth_param(i as u32).ok_or_else(|| {
                CodegenError::invalid_operand("function parameter", format!("param {}", i))
            })?;
            self.value_map.insert(param.value, llvm_param);
        }

        // Create basic blocks first (so we can reference them in terminators)
        for (i, block) in func.blocks.iter().enumerate() {
            let llvm_block = if i == 0 {
                // First block is the entry block
                fn_val
                    .get_first_basic_block()
                    .unwrap_or_else(|| self.context.append_basic_block(fn_val, "entry"))
            } else {
                let block_name = block.name.as_deref().unwrap_or("bb");
                self.context.append_basic_block(fn_val, block_name)
            };
            self.block_map.insert(block.id, llvm_block);
        }

        // Compile each basic block
        for block in &func.blocks {
            let llvm_block = *self
                .block_map
                .get(&block.id)
                .ok_or_else(|| CodegenError::block_not_found(block.id.0))?;
            self.builder.position_at_end(llvm_block);

            // Compile block parameters
            for param in &block.params {
                let param_ty = self.jet_type_to_llvm(&param.ty)?;
                let alloca = self
                    .builder
                    .build_alloca(param_ty, &param.name)
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                self.value_map.insert(param.value, alloca.into());
            }

            // Compile instructions
            for inst in &block.instructions {
                self.compile_instruction(inst)?;
            }

            // Compile terminator
            self.compile_terminator(&block.terminator)?;
        }

        Ok(fn_val)
    }

    /// Compiles a Jet IR instruction to LLVM.
    fn compile_instruction(
        &mut self,
        inst: &jet_ir::Instruction,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        match inst {
            jet_ir::Instruction::Const { result, value } => {
                let val = self.compile_constant(value)?;
                self.value_map.insert(*result, val);
                Ok(Some(val))
            }

            jet_ir::Instruction::Binary {
                result,
                op,
                lhs,
                rhs,
            } => {
                let lhs_val = self.get_value(*lhs)?;
                let rhs_val = self.get_value(*rhs)?;

                let result_val = match op {
                    // Arithmetic operations
                    jet_ir::BinaryOp::Add => {
                        if lhs_val.is_int_value() {
                            self.builder
                                .build_int_add(
                                    lhs_val.into_int_value(),
                                    rhs_val.into_int_value(),
                                    "add",
                                )
                                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                                .into()
                        } else {
                            self.builder
                                .build_float_add(
                                    lhs_val.into_float_value(),
                                    rhs_val.into_float_value(),
                                    "fadd",
                                )
                                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                                .into()
                        }
                    }
                    jet_ir::BinaryOp::Sub => {
                        if lhs_val.is_int_value() {
                            self.builder
                                .build_int_sub(
                                    lhs_val.into_int_value(),
                                    rhs_val.into_int_value(),
                                    "sub",
                                )
                                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                                .into()
                        } else {
                            self.builder
                                .build_float_sub(
                                    lhs_val.into_float_value(),
                                    rhs_val.into_float_value(),
                                    "fsub",
                                )
                                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                                .into()
                        }
                    }
                    jet_ir::BinaryOp::Mul => {
                        if lhs_val.is_int_value() {
                            self.builder
                                .build_int_mul(
                                    lhs_val.into_int_value(),
                                    rhs_val.into_int_value(),
                                    "mul",
                                )
                                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                                .into()
                        } else {
                            self.builder
                                .build_float_mul(
                                    lhs_val.into_float_value(),
                                    rhs_val.into_float_value(),
                                    "fmul",
                                )
                                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                                .into()
                        }
                    }
                    jet_ir::BinaryOp::Div => {
                        if lhs_val.is_int_value() {
                            self.builder
                                .build_int_signed_div(
                                    lhs_val.into_int_value(),
                                    rhs_val.into_int_value(),
                                    "sdiv",
                                )
                                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                                .into()
                        } else {
                            self.builder
                                .build_float_div(
                                    lhs_val.into_float_value(),
                                    rhs_val.into_float_value(),
                                    "fdiv",
                                )
                                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                                .into()
                        }
                    }
                    jet_ir::BinaryOp::Rem => {
                        if lhs_val.is_int_value() {
                            self.builder
                                .build_int_signed_rem(
                                    lhs_val.into_int_value(),
                                    rhs_val.into_int_value(),
                                    "srem",
                                )
                                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                                .into()
                        } else {
                            return Err(CodegenError::unsupported_instruction("float remainder"));
                        }
                    }

                    // Bitwise operations
                    jet_ir::BinaryOp::And => self
                        .builder
                        .build_and(lhs_val.into_int_value(), rhs_val.into_int_value(), "and")
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                        .into(),
                    jet_ir::BinaryOp::Or => self
                        .builder
                        .build_or(lhs_val.into_int_value(), rhs_val.into_int_value(), "or")
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                        .into(),
                    jet_ir::BinaryOp::Xor => self
                        .builder
                        .build_xor(lhs_val.into_int_value(), rhs_val.into_int_value(), "xor")
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                        .into(),
                    jet_ir::BinaryOp::Shl => self
                        .builder
                        .build_left_shift(lhs_val.into_int_value(), rhs_val.into_int_value(), "shl")
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                        .into(),
                    jet_ir::BinaryOp::Shr => self
                        .builder
                        .build_right_shift(
                            lhs_val.into_int_value(),
                            rhs_val.into_int_value(),
                            false, // logical shift
                            "shr",
                        )
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                        .into(),

                    // Comparison operations
                    jet_ir::BinaryOp::Eq
                    | jet_ir::BinaryOp::Ne
                    | jet_ir::BinaryOp::Lt
                    | jet_ir::BinaryOp::Le
                    | jet_ir::BinaryOp::Gt
                    | jet_ir::BinaryOp::Ge => self.compile_comparison(*op, lhs_val, rhs_val)?,
                };

                self.value_map.insert(*result, result_val);
                Ok(Some(result_val))
            }

            jet_ir::Instruction::Unary {
                result,
                op,
                operand,
            } => {
                let operand_val = self.get_value(*operand)?;

                let result_val = match op {
                    jet_ir::UnaryOp::Neg => {
                        if operand_val.is_int_value() {
                            self.builder
                                .build_int_neg(operand_val.into_int_value(), "neg")
                                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                                .into()
                        } else {
                            self.builder
                                .build_float_neg(operand_val.into_float_value(), "fneg")
                                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                                .into()
                        }
                    }
                    jet_ir::UnaryOp::Not => self
                        .builder
                        .build_not(operand_val.into_int_value(), "not")
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                        .into(),
                };

                self.value_map.insert(*result, result_val);
                Ok(Some(result_val))
            }

            jet_ir::Instruction::Alloc { result, ty } => {
                let llvm_ty = self.jet_type_to_llvm(ty)?;
                let alloca = self
                    .builder
                    .build_alloca(llvm_ty, "alloca")
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                self.value_map.insert(*result, alloca.into());
                Ok(Some(alloca.into()))
            }

            jet_ir::Instruction::Load { result, ptr } => {
                let ptr_val = self.get_value(*ptr)?.into_pointer_value();
                // Get the pointee type from the pointer
                let pointee_ty = self.get_pointee_type(*ptr)?;
                let loaded = self
                    .builder
                    .build_load(pointee_ty, ptr_val, "load")
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                self.value_map.insert(*result, loaded);
                Ok(Some(loaded))
            }

            jet_ir::Instruction::Store { ptr, value } => {
                let ptr_val = self.get_value(*ptr)?.into_pointer_value();
                let val = self.get_value(*value)?;
                self.builder
                    .build_store(ptr_val, val)
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                Ok(None)
            }

            jet_ir::Instruction::Call { result, func, args } => {
                let fn_val = *self
                    .function_map
                    .get(func)
                    .ok_or_else(|| CodegenError::function_not_found(func.clone()))?;

                let arg_values: Vec<BasicMetadataValueEnum<'ctx>> = args
                    .iter()
                    .map(|arg| self.get_value(*arg).map(|v| v.into()))
                    .collect::<CodegenResult<Vec<_>>>()?;

                let call_val = self
                    .builder
                    .build_call(fn_val, &arg_values, "call")
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

                let result_val = call_val.try_as_basic_value().basic();
                if let Some(val) = result_val {
                    self.value_map.insert(*result, val);
                }
                Ok(result_val)
            }

            jet_ir::Instruction::Phi { result, incoming } => {
                let phi_ty = self.get_value(incoming[0].1)?;
                let phi = self
                    .builder
                    .build_phi(phi_ty.get_type(), "phi")
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

                for (block_id, value_id) in incoming {
                    let block = *self
                        .block_map
                        .get(block_id)
                        .ok_or_else(|| CodegenError::block_not_found(block_id.0))?;
                    let value = self.get_value(*value_id)?;
                    phi.add_incoming(&[(&value, block)]);
                }

                let phi_val: BasicValueEnum<'ctx> = phi.as_basic_value();
                self.value_map.insert(*result, phi_val);
                Ok(Some(phi_val))
            }

            jet_ir::Instruction::IntCast { result, value, ty } => {
                let val = self.get_value(*value)?.into_int_value();
                let target_ty = self.jet_type_to_llvm(ty)?.into_int_type();
                let target_width = target_ty.get_bit_width();
                let source_width = val.get_type().get_bit_width();

                let cast_val = if target_width > source_width {
                    // Sign extend
                    self.builder
                        .build_int_s_extend(val, target_ty, "sext")
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                        .into()
                } else if target_width < source_width {
                    // Truncate
                    self.builder
                        .build_int_truncate(val, target_ty, "trunc")
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                        .into()
                } else {
                    val.into()
                };

                self.value_map.insert(*result, cast_val);
                Ok(Some(cast_val))
            }

            jet_ir::Instruction::BitCast { result, value, ty } => {
                let val = self.get_value(*value)?;
                let target_ty = self.jet_type_to_llvm(ty)?;

                let cast_val = if val.is_pointer_value() && target_ty.is_pointer_type() {
                    self.builder
                        .build_pointer_cast(
                            val.into_pointer_value(),
                            target_ty.into_pointer_type(),
                            "bitcast",
                        )
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                        .into()
                } else {
                    return Err(CodegenError::unsupported_instruction(
                        "bitcast for non-pointers",
                    ));
                };

                self.value_map.insert(*result, cast_val);
                Ok(Some(cast_val))
            }

            jet_ir::Instruction::GetFieldPtr {
                result,
                ptr,
                field_index,
            } => {
                let ptr_val = self.get_value(*ptr)?.into_pointer_value();
                // We need the struct type to use build_struct_gep
                // For now, we'll need to track the pointee type
                let pointee_ty = self.get_pointee_type(*ptr)?;
                let field_ptr = self
                    .builder
                    .build_struct_gep(pointee_ty, ptr_val, *field_index as u32, "fieldptr")
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                self.value_map.insert(*result, field_ptr.into());
                Ok(Some(field_ptr.into()))
            }

            // TODO: Implement more instructions
            _ => Err(CodegenError::unsupported_instruction(format!("{:?}", inst))),
        }
    }

    /// Gets the pointee type for a pointer value.
    /// This is a helper to track types for operations like load and gep.
    fn get_pointee_type(&self, ptr_id: jet_ir::ValueId) -> CodegenResult<BasicTypeEnum<'ctx>> {
        // For now, we need to infer the type from the context
        // In a full implementation, we'd track the type of each value
        // This is a simplified version that tries to get the type from the value
        let ptr_val = self.get_value(ptr_id)?;
        if ptr_val.is_pointer_value() {
            // Try to get element type from the pointer type
            let _ptr_type = ptr_val.into_pointer_value().get_type();
            // We can't easily get the element type from the pointer type in inkwell
            // In a real implementation, we'd track this in a type map
            Err(CodegenError::unsupported_type(
                "cannot determine pointee type - type tracking not implemented",
            ))
        } else {
            Err(CodegenError::type_mismatch(
                "pointer",
                format!("{:?}", ptr_val),
            ))
        }
    }

    /// Compiles a Jet IR terminator to LLVM.
    fn compile_terminator(&mut self, term: &jet_ir::Terminator) -> CodegenResult<()> {
        match term {
            jet_ir::Terminator::Return(val) => {
                if let Some(val_id) = val {
                    let val = self.get_value(*val_id)?;
                    self.builder
                        .build_return(Some(&val))
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                } else {
                    self.builder
                        .build_return(None)
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                }
                Ok(())
            }

            jet_ir::Terminator::Branch(block_id) => {
                let target = *self
                    .block_map
                    .get(block_id)
                    .ok_or_else(|| CodegenError::block_not_found(block_id.0))?;
                self.builder
                    .build_unconditional_branch(target)
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                Ok(())
            }

            jet_ir::Terminator::CondBranch {
                cond,
                then_block,
                else_block,
            } => {
                let cond_val = self.get_value(*cond)?.into_int_value();
                let then_bb = *self
                    .block_map
                    .get(then_block)
                    .ok_or_else(|| CodegenError::block_not_found(then_block.0))?;
                let else_bb = *self
                    .block_map
                    .get(else_block)
                    .ok_or_else(|| CodegenError::block_not_found(else_block.0))?;

                self.builder
                    .build_conditional_branch(cond_val, then_bb, else_bb)
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                Ok(())
            }

            jet_ir::Terminator::Unreachable => {
                self.builder
                    .build_unreachable()
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                Ok(())
            }

            jet_ir::Terminator::Switch {
                value,
                default_block,
                cases,
            } => {
                let val = self.get_value(*value)?.into_int_value();
                let default_bb = *self
                    .block_map
                    .get(default_block)
                    .ok_or_else(|| CodegenError::block_not_found(default_block.0))?;

                // Build cases slice
                let llvm_cases: Vec<_> = cases
                    .iter()
                    .map(|(case_val, case_block)| {
                        let case_bb = *self
                            .block_map
                            .get(case_block)
                            .ok_or_else(|| CodegenError::block_not_found(case_block.0))?;
                        let llvm_case_val = val.get_type().const_int(*case_val as u64, true);
                        Ok((llvm_case_val, case_bb))
                    })
                    .collect::<CodegenResult<Vec<_>>>()?;

                // Create the switch instruction
                self.builder
                    .build_switch(val, default_bb, &llvm_cases)
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

                Ok(())
            }
        }
    }

    /// Compiles a constant value to LLVM.
    fn compile_constant(
        &self,
        value: &jet_ir::ConstantValue,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match value {
            jet_ir::ConstantValue::Int(val, ty) => {
                let int_ty = self.jet_type_to_llvm(ty)?.into_int_type();
                Ok(int_ty.const_int(*val as u64, true).into())
            }
            jet_ir::ConstantValue::Float(val, ty) => {
                let float_ty = self.jet_type_to_llvm(ty)?.into_float_type();
                Ok(float_ty.const_float(*val).into())
            }
            jet_ir::ConstantValue::Bool(val) => Ok(self
                .context
                .bool_type()
                .const_int(*val as u64, false)
                .into()),
            jet_ir::ConstantValue::Null(ty) => {
                let ptr_ty = self.jet_type_to_llvm(ty)?.into_pointer_type();
                Ok(ptr_ty.const_null().into())
            }
            jet_ir::ConstantValue::Undef(ty) => {
                // For undef values, we create a zero value as a placeholder
                // In a full implementation, we'd use LLVM's undef value
                let llvm_ty = self.jet_type_to_llvm(ty)?;
                Ok(llvm_ty.const_zero())
            }
            jet_ir::ConstantValue::Zero(ty) => {
                let llvm_ty = self.jet_type_to_llvm(ty)?;
                Ok(llvm_ty.const_zero())
            }
            _ => Err(CodegenError::invalid_constant(format!("{:?}", value))),
        }
    }

    /// Compiles a comparison operation.
    fn compile_comparison(
        &self,
        op: jet_ir::BinaryOp,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let pred = if lhs.is_int_value() {
            // Integer comparison
            let int_pred = match op {
                jet_ir::BinaryOp::Eq => inkwell::IntPredicate::EQ,
                jet_ir::BinaryOp::Ne => inkwell::IntPredicate::NE,
                jet_ir::BinaryOp::Lt => inkwell::IntPredicate::SLT,
                jet_ir::BinaryOp::Le => inkwell::IntPredicate::SLE,
                jet_ir::BinaryOp::Gt => inkwell::IntPredicate::SGT,
                jet_ir::BinaryOp::Ge => inkwell::IntPredicate::SGE,
                _ => unreachable!(),
            };
            self.builder
                .build_int_compare(int_pred, lhs.into_int_value(), rhs.into_int_value(), "icmp")
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                .into()
        } else {
            // Float comparison
            let float_pred = match op {
                jet_ir::BinaryOp::Eq => inkwell::FloatPredicate::OEQ,
                jet_ir::BinaryOp::Ne => inkwell::FloatPredicate::ONE,
                jet_ir::BinaryOp::Lt => inkwell::FloatPredicate::OLT,
                jet_ir::BinaryOp::Le => inkwell::FloatPredicate::OLE,
                jet_ir::BinaryOp::Gt => inkwell::FloatPredicate::OGT,
                jet_ir::BinaryOp::Ge => inkwell::FloatPredicate::OGE,
                _ => unreachable!(),
            };
            self.builder
                .build_float_compare(
                    float_pred,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "fcmp",
                )
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
                .into()
        };

        Ok(pred)
    }

    /// Gets a value from the value map.
    fn get_value(&self, id: jet_ir::ValueId) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.value_map
            .get(&id)
            .copied()
            .ok_or_else(|| CodegenError::value_not_found(id.0))
    }

    /// Converts a Jet IR type to an LLVM type.
    fn jet_type_to_llvm(&self, ty: &jet_ir::Ty) -> CodegenResult<BasicTypeEnum<'ctx>> {
        match ty {
            jet_ir::Ty::Void => {
                // Map Void to an empty struct (like Rust's unit type)
                // This allows void to be used as a BasicType
                Ok(self.context.struct_type(&[], false).into())
            }
            jet_ir::Ty::Int(1) => Ok(self.context.bool_type().into()),
            jet_ir::Ty::Int(8) => Ok(self.context.i8_type().into()),
            jet_ir::Ty::Int(16) => Ok(self.context.i16_type().into()),
            jet_ir::Ty::Int(32) => Ok(self.context.i32_type().into()),
            jet_ir::Ty::Int(64) => Ok(self.context.i64_type().into()),
            jet_ir::Ty::Int(128) => Ok(self.context.i128_type().into()),
            jet_ir::Ty::Int(bits) => {
                // For other integer sizes, use a custom bit width
                Ok(self.context.custom_width_int_type(*bits).into())
            }
            jet_ir::Ty::Float(32) => Ok(self.context.f32_type().into()),
            jet_ir::Ty::Float(64) => Ok(self.context.f64_type().into()),
            jet_ir::Ty::Float(bits) => Err(CodegenError::unsupported_type(format!(
                "float{} (only f32 and f64 supported)",
                bits
            ))),
            jet_ir::Ty::Bool => Ok(self.context.bool_type().into()),
            jet_ir::Ty::Ptr(inner) => {
                let inner_llvm = self.jet_type_to_llvm(inner)?;
                Ok(inner_llvm.ptr_type(AddressSpace::default()).into())
            }
            jet_ir::Ty::Struct(fields) => {
                let field_types: Vec<BasicTypeEnum<'ctx>> = fields
                    .iter()
                    .map(|f| self.jet_type_to_llvm(f))
                    .collect::<CodegenResult<Vec<_>>>()?;
                let struct_ty = self.context.struct_type(&field_types, false);
                Ok(struct_ty.into())
            }
            jet_ir::Ty::Array(elem, count) => {
                let elem_ty = self.jet_type_to_llvm(elem)?;
                Ok(elem_ty.array_type(*count as u32).into())
            }
            jet_ir::Ty::Function(params, ret) => {
                let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = params
                    .iter()
                    .map(|p| self.jet_type_to_llvm(p).map(|t| t.into()))
                    .collect::<CodegenResult<Vec<_>>>()?;
                let fn_ty = if ret.is_void() {
                    self.context.void_type().fn_type(&param_types, false)
                } else {
                    let ret_ty = self.jet_type_to_llvm(ret)?;
                    ret_ty.fn_type(&param_types, false)
                };
                Ok(fn_ty.ptr_type(AddressSpace::default()).into())
            }
            jet_ir::Ty::Named(name) => {
                // Named types would need type definition lookup
                Err(CodegenError::unsupported_type(format!(
                    "named type: {}",
                    name
                )))
            }
            jet_ir::Ty::Generic(name, _args) => Err(CodegenError::unsupported_type(format!(
                "generic type: {}",
                name
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_ir::{
        BasicBlock, BinaryOp, BlockId, ConstantValue, Function, Instruction, Param, Terminator, Ty,
        ValueId,
    };
    use std::f64::consts::PI;

    fn create_simple_add_function() -> Function {
        let mut func = Function::new(
            "add",
            vec![
                Param::new("a", Ty::I32, ValueId::new(0)),
                Param::new("b", Ty::I32, ValueId::new(1)),
            ],
            Ty::I32,
        );

        let mut block = BasicBlock::new(BlockId::new(0));
        block.name = Some("entry".to_string());

        // %2 = add %0, %1
        block.add_instruction(Instruction::Binary {
            result: ValueId::new(2),
            op: BinaryOp::Add,
            lhs: ValueId::new(0),
            rhs: ValueId::new(1),
        });

        // ret %2
        block.set_terminator(Terminator::Return(Some(ValueId::new(2))));

        func.add_block(block);
        func
    }

    #[test]
    fn test_llvm_backend_creation() {
        let backend = LLVMBackend::new();
        // Just verify it creates successfully
        let _ctx = backend.context();
    }

    #[test]
    fn test_compile_simple_function() {
        let backend = LLVMBackend::new();
        let func = create_simple_add_function();

        let mut module = jet_ir::Module::new("test");
        module.add_function(func);

        let llvm_module = backend.compile_module(&module);
        assert!(
            llvm_module.is_ok(),
            "Failed to compile module: {:?}",
            llvm_module.err()
        );

        let llvm_module = llvm_module.unwrap();
        assert!(llvm_module.verify().is_ok());
    }

    #[test]
    fn test_type_conversion() {
        let backend = LLVMBackend::new();
        let module = jet_ir::Module::new("test");
        let llvm_module = backend.compile_module(&module).unwrap();
        let builder = backend.context().create_builder();

        let func_compiler = FunctionCompiler::new(backend.context(), &llvm_module, &builder);

        // Test integer types
        assert!(func_compiler.jet_type_to_llvm(&Ty::I32).is_ok());
        assert!(func_compiler.jet_type_to_llvm(&Ty::I64).is_ok());
        assert!(func_compiler.jet_type_to_llvm(&Ty::Bool).is_ok());

        // Test float types
        assert!(func_compiler.jet_type_to_llvm(&Ty::F32).is_ok());
        assert!(func_compiler.jet_type_to_llvm(&Ty::F64).is_ok());

        // Test pointer type
        let ptr_ty = Ty::Ptr(Box::new(Ty::I32));
        assert!(func_compiler.jet_type_to_llvm(&ptr_ty).is_ok());
    }

    #[test]
    fn test_constant_compilation() {
        let backend = LLVMBackend::new();
        let module = jet_ir::Module::new("test");
        let llvm_module = backend.compile_module(&module).unwrap();
        let builder = backend.context().create_builder();

        let func_compiler = FunctionCompiler::new(backend.context(), &llvm_module, &builder);

        // Test integer constant
        let int_const = ConstantValue::Int(42, Ty::I32);
        let result = func_compiler.compile_constant(&int_const);
        assert!(result.is_ok());

        // Test float constant
        let float_const = ConstantValue::Float(PI, Ty::F64);
        let result = func_compiler.compile_constant(&float_const);
        assert!(result.is_ok());

        // Test bool constant
        let bool_const = ConstantValue::Bool(true);
        let result = func_compiler.compile_constant(&bool_const);
        assert!(result.is_ok());
    }

    #[test]
    fn test_switch_terminator() {
        let backend = LLVMBackend::new();

        let mut func = Function::new("test_switch", vec![], Ty::Void);

        // Block 0: entry - switch
        let mut block0 = BasicBlock::new(BlockId::new(0));
        block0.name = Some("entry".to_string());
        block0.add_instruction(Instruction::Const {
            result: ValueId::new(0),
            value: ConstantValue::Int(1, Ty::I32),
        });
        block0.set_terminator(Terminator::Switch {
            value: ValueId::new(0),
            default_block: BlockId::new(3),
            cases: vec![(0, BlockId::new(1)), (1, BlockId::new(2))],
        });
        func.add_block(block0);

        // Block 1: case 0
        let mut block1 = BasicBlock::new(BlockId::new(1));
        block1.set_terminator(Terminator::Return(None));
        func.add_block(block1);

        // Block 2: case 1
        let mut block2 = BasicBlock::new(BlockId::new(2));
        block2.set_terminator(Terminator::Return(None));
        func.add_block(block2);

        // Block 3: default
        let mut block3 = BasicBlock::new(BlockId::new(3));
        block3.set_terminator(Terminator::Return(None));
        func.add_block(block3);

        let mut module = jet_ir::Module::new("test");
        module.add_function(func);

        let llvm_module = backend.compile_module(&module);
        assert!(
            llvm_module.is_ok(),
            "Failed to compile switch: {:?}",
            llvm_module.err()
        );

        let llvm_module = llvm_module.unwrap();
        assert!(llvm_module.verify().is_ok());
    }
}
