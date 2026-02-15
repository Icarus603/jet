//! Instruction Compilation
//!
//! This module compiles Jet IR instructions to LLVM IR instructions.
//! It handles arithmetic, memory, control flow, and aggregate operations.

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use crate::types::TypeMapping;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue};
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use jet_ir::{BinaryOp, ConstantValue, Instruction, Ty, UnaryOp, ValueId};

/// Compiles a single Jet IR instruction to LLVM.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `inst` - The instruction to compile
///
/// # Returns
///
/// Returns the result value if the instruction produces one, or None.
pub fn compile_instruction<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    inst: &Instruction,
) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
    match inst {
        // Literals
        Instruction::Const { result, value } => {
            let val = compile_constant(codegen, value)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        // Arithmetic
        Instruction::Binary {
            result,
            op,
            lhs,
            rhs,
        } => {
            let val = compile_binary_op(codegen, *op, *lhs, *rhs)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        Instruction::Unary {
            result,
            op,
            operand,
        } => {
            let val = compile_unary_op(codegen, *op, *operand)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        // Memory operations
        Instruction::Alloc { result, ty } => {
            let val = compile_alloc(codegen, ty)?;
            codegen.set_value(*result, val.into());
            codegen.set_stack_slot(*result, val);
            Ok(Some(val.into()))
        }

        Instruction::Load { result, ptr } => {
            let val = compile_load(codegen, *result, *ptr)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        Instruction::Store { ptr, value } => {
            compile_store(codegen, *ptr, *value)?;
            Ok(None)
        }

        Instruction::GetFieldPtr {
            result,
            ptr,
            field_index,
        } => {
            let val = compile_get_field_ptr(codegen, *ptr, *field_index)?;
            codegen.set_value(*result, val.into());
            Ok(Some(val.into()))
        }

        Instruction::GetElementPtr { result, ptr, index } => {
            let val = compile_get_element_ptr(codegen, *ptr, *index)?;
            codegen.set_value(*result, val.into());
            Ok(Some(val.into()))
        }

        // Casts
        Instruction::BitCast { result, value, ty } => {
            let val = compile_bitcast(codegen, *value, ty)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        Instruction::IntCast { result, value, ty } => {
            let val = compile_int_cast(codegen, *value, ty)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        Instruction::FloatCast { result, value, ty } => {
            let val = compile_float_cast(codegen, *value, ty)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        Instruction::IntToFloat { result, value, ty } => {
            let val = compile_int_to_float(codegen, *value, ty)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        Instruction::FloatToInt { result, value, ty } => {
            let val = compile_float_to_int(codegen, *value, ty)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        // Calls
        Instruction::Call { result, func, args } => {
            let val = compile_call(codegen, func, args)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        Instruction::CallIndirect { result, ptr, args } => {
            let val = compile_call_indirect(codegen, *ptr, args)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        // SSA
        Instruction::Phi { result, incoming } => {
            let val = compile_phi(codegen, *result, incoming)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        // Aggregates
        Instruction::StructAgg { result, fields, ty } => {
            let val = compile_struct_agg(codegen, fields, ty)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        Instruction::ArrayAgg {
            result,
            elements,
            ty,
        } => {
            let val = compile_array_agg(codegen, elements, ty)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        Instruction::ExtractField {
            result,
            aggregate,
            field_index,
        } => {
            let val = compile_extract_field(codegen, *aggregate, *field_index)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        Instruction::InsertField {
            result,
            aggregate,
            field_index,
            value,
        } => {
            let val = compile_insert_field(codegen, *aggregate, *field_index, *value)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        // Effects
        Instruction::TryCall {
            result,
            func,
            args,
            success_block,
            failure_block,
            failure_val,
        } => {
            compile_try_call(
                codegen,
                *result,
                func,
                args,
                *success_block,
                *failure_block,
                *failure_val,
            )?;
            Ok(None)
        }

        Instruction::Resume { value } => {
            compile_resume(codegen, *value)?;
            Ok(None)
        }

        Instruction::Await { result, future } => {
            let val = compile_await(codegen, *future)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        // Debug
        Instruction::DebugPrint { value } => {
            compile_debug_print(codegen, *value)?;
            Ok(None)
        }
    }
}

/// Compiles a constant value to LLVM.
fn compile_constant<'ctx>(
    codegen: &CodeGen<'ctx>,
    value: &ConstantValue,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    match value {
        ConstantValue::Int(val, ty) => {
            let int_ty = codegen.jet_to_llvm(ty)?.into_int_type();
            Ok(int_ty.const_int(*val as u64, true).into())
        }
        ConstantValue::Float(val, ty) => {
            let float_ty = if ty == &Ty::F32 {
                codegen.context.f32_type()
            } else {
                codegen.context.f64_type()
            };
            Ok(float_ty.const_float(*val).into())
        }
        ConstantValue::Bool(val) => Ok(codegen
            .context
            .bool_type()
            .const_int(*val as u64, false)
            .into()),
        ConstantValue::String(s) => {
            // Create a global string constant
            let string_ptr = codegen
                .builder
                .build_global_string_ptr(s, "str")
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
            Ok(string_ptr.as_pointer_value().into())
        }
        ConstantValue::Null(ty) => {
            let ptr_ty = codegen.jet_to_llvm(ty)?.into_pointer_type();
            Ok(ptr_ty.const_null().into())
        }
        ConstantValue::Undef(ty) | ConstantValue::Zero(ty) => {
            let llvm_ty = codegen.jet_to_llvm(ty)?;
            Ok(llvm_ty.const_zero())
        }
    }
}

/// Compiles a binary operation.
fn compile_binary_op<'ctx>(
    codegen: &CodeGen<'ctx>,
    op: BinaryOp,
    lhs: ValueId,
    rhs: ValueId,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let lhs_val = codegen.get_value(lhs)?;
    let rhs_val = codegen.get_value(rhs)?;

    match op {
        // Arithmetic
        BinaryOp::Add => {
            if lhs_val.is_int_value() {
                codegen
                    .builder
                    .build_int_add(lhs_val.into_int_value(), rhs_val.into_int_value(), "add")
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else {
                codegen
                    .builder
                    .build_float_add(
                        lhs_val.into_float_value(),
                        rhs_val.into_float_value(),
                        "fadd",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            }
        }
        BinaryOp::Sub => {
            if lhs_val.is_int_value() {
                codegen
                    .builder
                    .build_int_sub(lhs_val.into_int_value(), rhs_val.into_int_value(), "sub")
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else {
                codegen
                    .builder
                    .build_float_sub(
                        lhs_val.into_float_value(),
                        rhs_val.into_float_value(),
                        "fsub",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            }
        }
        BinaryOp::Mul => {
            if lhs_val.is_int_value() {
                codegen
                    .builder
                    .build_int_mul(lhs_val.into_int_value(), rhs_val.into_int_value(), "mul")
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else {
                codegen
                    .builder
                    .build_float_mul(
                        lhs_val.into_float_value(),
                        rhs_val.into_float_value(),
                        "fmul",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            }
        }
        BinaryOp::Div => {
            if lhs_val.is_int_value() {
                codegen
                    .builder
                    .build_int_signed_div(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        "sdiv",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else {
                codegen
                    .builder
                    .build_float_div(
                        lhs_val.into_float_value(),
                        rhs_val.into_float_value(),
                        "fdiv",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            }
        }
        BinaryOp::Rem => {
            if lhs_val.is_int_value() {
                codegen
                    .builder
                    .build_int_signed_rem(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        "srem",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else {
                Err(CodegenError::unsupported_instruction("float remainder"))
            }
        }

        // Bitwise
        BinaryOp::And => codegen
            .builder
            .build_and(lhs_val.into_int_value(), rhs_val.into_int_value(), "and")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into()),
        BinaryOp::Or => codegen
            .builder
            .build_or(lhs_val.into_int_value(), rhs_val.into_int_value(), "or")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into()),
        BinaryOp::Xor => codegen
            .builder
            .build_xor(lhs_val.into_int_value(), rhs_val.into_int_value(), "xor")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into()),
        BinaryOp::Shl => codegen
            .builder
            .build_left_shift(lhs_val.into_int_value(), rhs_val.into_int_value(), "shl")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into()),
        BinaryOp::Shr => codegen
            .builder
            .build_right_shift(
                lhs_val.into_int_value(),
                rhs_val.into_int_value(),
                false,
                "shr",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into()),

        // Comparisons
        BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
            compile_comparison(codegen, op, lhs_val, rhs_val)
        }
    }
}

/// Compiles a comparison operation.
fn compile_comparison<'ctx>(
    codegen: &CodeGen<'ctx>,
    op: BinaryOp,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    if lhs.is_int_value() {
        let pred = match op {
            BinaryOp::Eq => IntPredicate::EQ,
            BinaryOp::Ne => IntPredicate::NE,
            BinaryOp::Lt => IntPredicate::SLT,
            BinaryOp::Le => IntPredicate::SLE,
            BinaryOp::Gt => IntPredicate::SGT,
            BinaryOp::Ge => IntPredicate::SGE,
            _ => unreachable!(),
        };
        codegen
            .builder
            .build_int_compare(pred, lhs.into_int_value(), rhs.into_int_value(), "icmp")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into())
    } else {
        let pred = match op {
            BinaryOp::Eq => FloatPredicate::OEQ,
            BinaryOp::Ne => FloatPredicate::ONE,
            BinaryOp::Lt => FloatPredicate::OLT,
            BinaryOp::Le => FloatPredicate::OLE,
            BinaryOp::Gt => FloatPredicate::OGT,
            BinaryOp::Ge => FloatPredicate::OGE,
            _ => unreachable!(),
        };
        codegen
            .builder
            .build_float_compare(pred, lhs.into_float_value(), rhs.into_float_value(), "fcmp")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into())
    }
}

/// Compiles a unary operation.
fn compile_unary_op<'ctx>(
    codegen: &CodeGen<'ctx>,
    op: UnaryOp,
    operand: ValueId,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let operand_val = codegen.get_value(operand)?;

    match op {
        UnaryOp::Neg => {
            if operand_val.is_int_value() {
                codegen
                    .builder
                    .build_int_neg(operand_val.into_int_value(), "neg")
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else {
                codegen
                    .builder
                    .build_float_neg(operand_val.into_float_value(), "fneg")
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            }
        }
        UnaryOp::Not => codegen
            .builder
            .build_not(operand_val.into_int_value(), "not")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into()),
    }
}

/// Compiles an alloca instruction.
fn compile_alloc<'ctx>(codegen: &CodeGen<'ctx>, ty: &Ty) -> CodegenResult<PointerValue<'ctx>> {
    let llvm_ty = codegen.jet_to_llvm(ty)?;
    codegen
        .builder
        .build_alloca(llvm_ty, "alloca")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))
}

/// Compiles a load instruction.
fn compile_load<'ctx>(
    codegen: &CodeGen<'ctx>,
    _result: ValueId,
    ptr: ValueId,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let ptr_val = codegen.get_value(ptr)?.into_pointer_value();

    // We need to know the type being loaded
    // In a full implementation, we'd track types for all values
    // For now, we'll try to infer from the stack slot or use a default
    let pointee_ty: inkwell::types::BasicTypeEnum = if codegen.get_stack_slot(ptr).is_some() {
        // If it's a stack slot, we need to track the type
        // For now, use i32 as a fallback
        codegen.context.i32_type().into()
    } else {
        codegen.context.i32_type().into()
    };

    codegen
        .builder
        .build_load(pointee_ty, ptr_val, "load")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))
}

/// Compiles a store instruction.
fn compile_store<'ctx>(codegen: &CodeGen<'ctx>, ptr: ValueId, value: ValueId) -> CodegenResult<()> {
    let ptr_val = codegen.get_value(ptr)?.into_pointer_value();
    let val = codegen.get_value(value)?;

    codegen
        .builder
        .build_store(ptr_val, val)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(())
}

/// Compiles a get field pointer instruction.
fn compile_get_field_ptr<'ctx>(
    codegen: &CodeGen<'ctx>,
    ptr: ValueId,
    field_index: usize,
) -> CodegenResult<PointerValue<'ctx>> {
    let ptr_val = codegen.get_value(ptr)?.into_pointer_value();

    // We need the struct type to use build_struct_gep
    // For now, we'll use a placeholder approach
    // In a full implementation, we'd track the pointee type
    let pointee_ty = codegen.context.i32_type();

    codegen
        .builder
        .build_struct_gep(pointee_ty, ptr_val, field_index as u32, "fieldptr")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))
}

/// Compiles a get element pointer instruction.
fn compile_get_element_ptr<'ctx>(
    codegen: &CodeGen<'ctx>,
    ptr: ValueId,
    index: ValueId,
) -> CodegenResult<PointerValue<'ctx>> {
    let ptr_val = codegen.get_value(ptr)?.into_pointer_value();
    let index_val = codegen.get_value(index)?.into_int_value();

    // We need the element type
    let elem_ty = codegen.context.i32_type();

    unsafe {
        codegen
            .builder
            .build_gep(elem_ty, ptr_val, &[index_val], "gep")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
    }
}

/// Compiles a bitcast instruction.
fn compile_bitcast<'ctx>(
    codegen: &CodeGen<'ctx>,
    value: ValueId,
    ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let val = codegen.get_value(value)?;
    let target_ty = codegen.jet_to_llvm(ty)?;

    if val.is_pointer_value() && target_ty.is_pointer_type() {
        codegen
            .builder
            .build_pointer_cast(
                val.into_pointer_value(),
                target_ty.into_pointer_type(),
                "bitcast",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into())
    } else {
        Err(CodegenError::unsupported_instruction(
            "bitcast for non-pointers",
        ))
    }
}

/// Compiles an integer cast instruction.
fn compile_int_cast<'ctx>(
    codegen: &CodeGen<'ctx>,
    value: ValueId,
    ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let val = codegen.get_value(value)?.into_int_value();
    let target_ty = codegen.jet_to_llvm(ty)?.into_int_type();
    let target_width = target_ty.get_bit_width();
    let source_width = val.get_type().get_bit_width();

    if target_width > source_width {
        // Sign extend
        codegen
            .builder
            .build_int_s_extend(val, target_ty, "sext")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into())
    } else if target_width < source_width {
        // Truncate
        codegen
            .builder
            .build_int_truncate(val, target_ty, "trunc")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into())
    } else {
        Ok(val.into())
    }
}

/// Compiles a float cast instruction.
fn compile_float_cast<'ctx>(
    codegen: &CodeGen<'ctx>,
    value: ValueId,
    ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let val = codegen.get_value(value)?.into_float_value();
    let target_ty = codegen.jet_to_llvm(ty)?.into_float_type();

    // Only f32 -> f64 or f64 -> f32 are valid
    let source_bits = if val.get_type() == codegen.context.f32_type() {
        32
    } else {
        64
    };
    let target_bits = if target_ty == codegen.context.f32_type() {
        32
    } else {
        64
    };

    if target_bits > source_bits {
        // Extend
        codegen
            .builder
            .build_float_ext(val, target_ty, "fpext")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into())
    } else if target_bits < source_bits {
        // Truncate
        codegen
            .builder
            .build_float_trunc(val, target_ty, "fptrunc")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into())
    } else {
        Ok(val.into())
    }
}

/// Compiles an int to float cast instruction.
fn compile_int_to_float<'ctx>(
    codegen: &CodeGen<'ctx>,
    value: ValueId,
    ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let val = codegen.get_value(value)?.into_int_value();
    let target_ty = codegen.jet_to_llvm(ty)?.into_float_type();

    codegen
        .builder
        .build_signed_int_to_float(val, target_ty, "sitofp")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))
        .map(|v| v.into())
}

/// Compiles a float to int cast instruction.
fn compile_float_to_int<'ctx>(
    codegen: &CodeGen<'ctx>,
    value: ValueId,
    ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let val = codegen.get_value(value)?.into_float_value();
    let target_ty = codegen.jet_to_llvm(ty)?.into_int_type();

    codegen
        .builder
        .build_float_to_signed_int(val, target_ty, "fptosi")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))
        .map(|v| v.into())
}

/// Compiles a function call instruction.
fn compile_call<'ctx>(
    codegen: &CodeGen<'ctx>,
    func: &str,
    args: &[ValueId],
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let mut arg_values: Vec<BasicMetadataValueEnum<'ctx>> = args
        .iter()
        .map(|arg| codegen.get_value(*arg).map(|v| v.into()))
        .collect::<CodegenResult<Vec<_>>>()?;

    let mut actual_func = func.to_string();
    // Route polymorphic print/println calls to concrete runtime builtins.
    if (func == "jet_print" || func == "jet_println") && arg_values.len() == 1 {
        actual_func = if matches!(arg_values[0], BasicMetadataValueEnum::IntValue(_)) {
            "jet_print_int".to_string()
        } else if matches!(arg_values[0], BasicMetadataValueEnum::FloatValue(_)) {
            "jet_print_float".to_string()
        } else if matches!(arg_values[0], BasicMetadataValueEnum::PointerValue(_)) {
            if func == "jet_println" {
                "jet_println".to_string()
            } else {
                "jet_print".to_string()
            }
        } else {
            "jet_print_bool".to_string()
        };
    }

    let fn_val = codegen.get_function(&actual_func)?;
    let fn_ty = fn_val.get_type();
    let expected = fn_ty.get_param_types();
    if expected.len() == arg_values.len() {
        for (i, exp) in expected.iter().enumerate() {
            match (*exp, arg_values[i]) {
                (
                    BasicMetadataTypeEnum::PointerType(ptr_ty),
                    BasicMetadataValueEnum::IntValue(int_val),
                ) => {
                    let cast = codegen
                        .builder
                        .build_int_to_ptr(int_val, ptr_ty, "int_to_ptr")
                        .map_err(|e| CodegenError::instruction_error(format!("{e:?}")))?;
                    arg_values[i] = cast.into();
                }
                (
                    BasicMetadataTypeEnum::IntType(exp_int_ty),
                    BasicMetadataValueEnum::IntValue(int_val),
                ) if int_val.get_type().get_bit_width() != exp_int_ty.get_bit_width() => {
                    let cast = codegen
                        .builder
                        .build_int_s_extend(int_val, exp_int_ty, "int_sext")
                        .map_err(|e| CodegenError::instruction_error(format!("{e:?}")))?;
                    arg_values[i] = cast.into();
                }
                _ => {}
            }
        }
    }

    let call_val = codegen
        .builder
        .build_call(fn_val, &arg_values, "call")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // If the function returns void, we need to handle that
    let either_result = call_val.try_as_basic_value();
    if either_result.is_basic() {
        Ok(either_result.unwrap_basic())
    } else {
        // Void return - return a dummy value
        Ok(codegen.context.i32_type().const_zero().into())
    }
}

/// Compiles an indirect function call instruction.
fn compile_call_indirect<'ctx>(
    codegen: &CodeGen<'ctx>,
    ptr: ValueId,
    args: &[ValueId],
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let fn_ptr = codegen.get_value(ptr)?.into_pointer_value();

    let arg_values: Vec<BasicMetadataValueEnum<'ctx>> = args
        .iter()
        .map(|arg| codegen.get_value(*arg).map(|v| v.into()))
        .collect::<CodegenResult<Vec<_>>>()?;

    // We need to know the function type
    // For now, use a generic i32() type
    let fn_ty = codegen.context.i32_type().fn_type(&[], false);

    let call_val = codegen
        .builder
        .build_indirect_call(fn_ty, fn_ptr, &arg_values, "callind")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    let either_result = call_val.try_as_basic_value();
    if either_result.is_basic() {
        Ok(either_result.unwrap_basic())
    } else {
        Ok(codegen.context.i32_type().const_zero().into())
    }
}

/// Compiles a phi instruction.
fn compile_phi<'ctx>(
    codegen: &CodeGen<'ctx>,
    _result: ValueId,
    incoming: &[(jet_ir::BlockId, ValueId)],
) -> CodegenResult<BasicValueEnum<'ctx>> {
    if incoming.is_empty() {
        return Err(CodegenError::invalid_operand("phi", "no incoming values"));
    }

    // Get the type from the first incoming value
    let first_val = codegen.get_value(incoming[0].1)?;
    let phi_ty = first_val.get_type();

    let phi = codegen
        .builder
        .build_phi(phi_ty, "phi")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    for (block_id, value_id) in incoming {
        let block = codegen.get_block(*block_id)?;
        let value = codegen.get_value(*value_id)?;
        phi.add_incoming(&[(&value, block)]);
    }

    Ok(phi.as_basic_value())
}

/// Compiles a struct aggregate instruction.
fn compile_struct_agg<'ctx>(
    codegen: &CodeGen<'ctx>,
    fields: &[ValueId],
    ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let llvm_ty = codegen.jet_to_llvm(ty)?.into_struct_type();
    let mut struct_val = llvm_ty.const_zero();

    for (i, field_id) in fields.iter().enumerate() {
        let field_val = codegen.get_value(*field_id)?;
        struct_val = codegen
            .builder
            .build_insert_value(struct_val, field_val, i as u32, "structagg")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?
            .into_struct_value();
    }

    Ok(struct_val.into())
}

/// Compiles an array aggregate instruction.
fn compile_array_agg<'ctx>(
    codegen: &CodeGen<'ctx>,
    elements: &[ValueId],
    ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let llvm_ty = codegen.jet_to_llvm(ty)?.into_array_type();
    let mut arr_val = llvm_ty.const_zero();

    for (i, elem_id) in elements.iter().enumerate() {
        let elem_val = codegen.get_value(*elem_id)?;
        arr_val = codegen
            .builder
            .build_insert_value(arr_val, elem_val, i as u32, "arrayagg")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?
            .into_array_value();
    }

    Ok(arr_val.into())
}

/// Compiles an extract field instruction.
fn compile_extract_field<'ctx>(
    codegen: &CodeGen<'ctx>,
    aggregate: ValueId,
    field_index: usize,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let agg_val = codegen.get_value(aggregate)?;

    codegen
        .builder
        .build_extract_value(
            agg_val.into_struct_value(),
            field_index as u32,
            "extractfield",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))
}

/// Compiles an insert field instruction.
fn compile_insert_field<'ctx>(
    codegen: &CodeGen<'ctx>,
    aggregate: ValueId,
    field_index: usize,
    value: ValueId,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let agg_val = codegen.get_value(aggregate)?;
    let field_val = codegen.get_value(value)?;

    let result = codegen
        .builder
        .build_insert_value(
            agg_val.into_struct_value(),
            field_val,
            field_index as u32,
            "insertfield",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Convert AggregateValueEnum to BasicValueEnum
    Ok(result.into_struct_value().into())
}

/// Compiles a try call instruction (effect handling).
fn compile_try_call<'ctx>(
    _codegen: &CodeGen<'ctx>,
    _result: ValueId,
    _func: &str,
    _args: &[ValueId],
    _success_block: jet_ir::BlockId,
    _failure_block: jet_ir::BlockId,
    _failure_val: ValueId,
) -> CodegenResult<()> {
    // Effect handling is implemented separately
    // This is a placeholder for the basic instruction compilation
    Err(CodegenError::unsupported_instruction(
        "try_call - use effect module",
    ))
}

/// Compiles a resume instruction.
fn compile_resume<'ctx>(_codegen: &CodeGen<'ctx>, _value: ValueId) -> CodegenResult<()> {
    // Effect handling is implemented separately
    Err(CodegenError::unsupported_instruction(
        "resume - use effect module",
    ))
}

/// Compiles an await instruction.
fn compile_await<'ctx>(
    _codegen: &CodeGen<'ctx>,
    _future: ValueId,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    // Async handling is implemented separately
    Err(CodegenError::unsupported_instruction(
        "await - use async runtime",
    ))
}

/// Compiles a debug print instruction.
fn compile_debug_print<'ctx>(codegen: &CodeGen<'ctx>, value: ValueId) -> CodegenResult<()> {
    let val = codegen.get_value(value)?;

    // Create or get the printf function
    let printf_ty = codegen.context.i32_type().fn_type(
        &[codegen
            .context
            .i8_type()
            .ptr_type(inkwell::AddressSpace::default())
            .into()],
        true,
    );
    let printf = codegen
        .module
        .get_function("printf")
        .unwrap_or_else(|| codegen.module.add_function("printf", printf_ty, None));

    // Create a format string based on the value type
    let format_str = if val.is_int_value() {
        "%d\n"
    } else if val.is_float_value() {
        "%f\n"
    } else {
        "%p\n"
    };

    let format_global = codegen
        .builder
        .build_global_string_ptr(format_str, "debug_fmt")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    codegen
        .builder
        .build_call(
            printf,
            &[format_global.as_pointer_value().into(), val.into()],
            "debug_print",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(())
}
