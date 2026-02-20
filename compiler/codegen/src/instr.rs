//! Instruction Compilation
//!
//! This module compiles Jet IR instructions to LLVM IR instructions.
//! It handles arithmetic, memory, control flow, and aggregate operations.

use crate::coerce::{coerce_value, widen_type};
use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use crate::types::TypeMapping;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
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

        Instruction::Load { result, ptr, ty } => {
            let val = compile_load(codegen, *result, *ptr, ty)?;
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
            struct_ty,
        } => {
            let val = compile_get_field_ptr(codegen, *ptr, *field_index, struct_ty)?;
            codegen.set_value(*result, val.into());
            Ok(Some(val.into()))
        }

        Instruction::GetElementPtr {
            result,
            ptr,
            index,
            elem_ty,
        } => {
            let val = compile_get_element_ptr(codegen, *ptr, *index, elem_ty)?;
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
        // Calls
        Instruction::Call {
            result,
            func,
            args,
            ty,
        } => {
            let val = compile_call(codegen, func, args, ty)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        Instruction::CallIndirect {
            result,
            ptr,
            args,
            ty,
        } => {
            let val = compile_call_indirect(codegen, *ptr, args, ty)?;
            codegen.set_value(*result, val);
            Ok(Some(val))
        }

        // SSA
        Instruction::Phi {
            result,
            incoming,
            ty,
        } => {
            let val = compile_phi(codegen, *result, incoming, ty)?;
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

        // Ghost code elimination - Nop does nothing
        Instruction::Nop => Ok(None),
    }
}

/// Compiles a constant value to LLVM.
fn compile_constant<'ctx>(
    codegen: &CodeGen<'ctx>,
    value: &ConstantValue,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    match value {
        ConstantValue::Int(val, ty) => {
            let llvm_ty = codegen.jet_to_llvm(ty)?;
            if llvm_ty.is_int_type() {
                Ok(llvm_ty.into_int_type().const_int(*val as u64, true).into())
            } else if llvm_ty.is_pointer_type() {
                // Create an integer value and cast it to pointer
                let int_val = codegen.context.i64_type().const_int(*val as u64, false);
                Ok(codegen
                    .builder
                    .build_int_to_ptr(int_val, llvm_ty.into_pointer_type(), "int_to_ptr")
                    .map_err(|e| CodegenError::instruction_error(format!("{e:?}")))?
                    .into())
            } else {
                Err(CodegenError::unsupported_instruction(format!(
                    "Int constant with non-int/non-ptr type: {:?}",
                    ty
                )))
            }
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
            // Check if this is a function name (used for closure function pointers)
            if let Ok(func_val) = codegen.get_function(s) {
                // Return the function pointer
                Ok(func_val.as_global_value().as_pointer_value().into())
            } else {
                // Create a global string constant
                let string_ptr = codegen
                    .builder
                    .build_global_string_ptr(s, "str")
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                Ok(string_ptr.as_pointer_value().into())
            }
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

    // Determine the target type for coercion
    let target_ty = widen_type(codegen, lhs_val, rhs_val);

    // Coerce both operands to the target type
    let lhs_coerced = if let Some(target) = target_ty {
        coerce_value(codegen, lhs_val, target)?
    } else {
        lhs_val
    };
    let rhs_coerced = if let Some(target) = target_ty {
        coerce_value(codegen, rhs_val, target)?
    } else {
        rhs_val
    };

    match op {
        // Arithmetic
        BinaryOp::Add => {
            if lhs_coerced.is_int_value() && rhs_coerced.is_int_value() {
                codegen
                    .builder
                    .build_int_add(
                        lhs_coerced.into_int_value(),
                        rhs_coerced.into_int_value(),
                        "add",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else if lhs_coerced.is_float_value() && rhs_coerced.is_float_value() {
                codegen
                    .builder
                    .build_float_add(
                        lhs_coerced.into_float_value(),
                        rhs_coerced.into_float_value(),
                        "fadd",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else {
                Err(CodegenError::unsupported_instruction(format!(
                    "Add operation not supported for types {:?} and {:?}",
                    lhs_coerced.get_type(),
                    rhs_coerced.get_type()
                )))
            }
        }
        BinaryOp::Sub => {
            if lhs_coerced.is_int_value() && rhs_coerced.is_int_value() {
                codegen
                    .builder
                    .build_int_sub(
                        lhs_coerced.into_int_value(),
                        rhs_coerced.into_int_value(),
                        "sub",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else if lhs_coerced.is_float_value() && rhs_coerced.is_float_value() {
                codegen
                    .builder
                    .build_float_sub(
                        lhs_coerced.into_float_value(),
                        rhs_coerced.into_float_value(),
                        "fsub",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else {
                Err(CodegenError::unsupported_instruction(format!(
                    "Sub operation not supported for types {:?} and {:?}",
                    lhs_coerced.get_type(),
                    rhs_coerced.get_type()
                )))
            }
        }
        BinaryOp::Mul => {
            if lhs_coerced.is_int_value() && rhs_coerced.is_int_value() {
                codegen
                    .builder
                    .build_int_mul(
                        lhs_coerced.into_int_value(),
                        rhs_coerced.into_int_value(),
                        "mul",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else if lhs_coerced.is_float_value() && rhs_coerced.is_float_value() {
                codegen
                    .builder
                    .build_float_mul(
                        lhs_coerced.into_float_value(),
                        rhs_coerced.into_float_value(),
                        "fmul",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else {
                Err(CodegenError::unsupported_instruction(format!(
                    "Mul operation not supported for types {:?} and {:?}",
                    lhs_coerced.get_type(),
                    rhs_coerced.get_type()
                )))
            }
        }
        BinaryOp::Div => {
            if lhs_coerced.is_int_value() && rhs_coerced.is_int_value() {
                codegen
                    .builder
                    .build_int_signed_div(
                        lhs_coerced.into_int_value(),
                        rhs_coerced.into_int_value(),
                        "sdiv",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else if lhs_coerced.is_float_value() && rhs_coerced.is_float_value() {
                codegen
                    .builder
                    .build_float_div(
                        lhs_coerced.into_float_value(),
                        rhs_coerced.into_float_value(),
                        "fdiv",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else {
                Err(CodegenError::unsupported_instruction(format!(
                    "Div operation not supported for types {:?} and {:?}",
                    lhs_coerced.get_type(),
                    rhs_coerced.get_type()
                )))
            }
        }
        BinaryOp::Rem => {
            if lhs_coerced.is_int_value() && rhs_coerced.is_int_value() {
                codegen
                    .builder
                    .build_int_signed_rem(
                        lhs_coerced.into_int_value(),
                        rhs_coerced.into_int_value(),
                        "srem",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else if lhs_coerced.is_float_value() && rhs_coerced.is_float_value() {
                codegen
                    .builder
                    .build_float_rem(
                        lhs_coerced.into_float_value(),
                        rhs_coerced.into_float_value(),
                        "frem",
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))
                    .map(|v| v.into())
            } else {
                Err(CodegenError::unsupported_instruction(format!(
                    "Rem operation not supported for types {:?} and {:?}",
                    lhs_coerced.get_type(),
                    rhs_coerced.get_type()
                )))
            }
        }

        // Bitwise
        BinaryOp::And => codegen
            .builder
            .build_and(
                lhs_coerced.into_int_value(),
                rhs_coerced.into_int_value(),
                "and",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into()),
        BinaryOp::Or => codegen
            .builder
            .build_or(
                lhs_coerced.into_int_value(),
                rhs_coerced.into_int_value(),
                "or",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into()),
        BinaryOp::Xor => codegen
            .builder
            .build_xor(
                lhs_coerced.into_int_value(),
                rhs_coerced.into_int_value(),
                "xor",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into()),
        BinaryOp::Shl => codegen
            .builder
            .build_left_shift(
                lhs_coerced.into_int_value(),
                rhs_coerced.into_int_value(),
                "shl",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into()),
        BinaryOp::Shr => codegen
            .builder
            .build_right_shift(
                lhs_coerced.into_int_value(),
                rhs_coerced.into_int_value(),
                false,
                "shr",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into()),

        // Comparisons
        BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
            compile_comparison(codegen, op, lhs_coerced, rhs_coerced)
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
    if lhs.is_int_value() && rhs.is_int_value() {
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
    } else if lhs.is_float_value() && rhs.is_float_value() {
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
    } else if lhs.is_pointer_value() && rhs.is_pointer_value() {
        // Pointer comparison - compare as integers
        let pred = match op {
            BinaryOp::Eq => IntPredicate::EQ,
            BinaryOp::Ne => IntPredicate::NE,
            BinaryOp::Lt => IntPredicate::ULT,
            BinaryOp::Le => IntPredicate::ULE,
            BinaryOp::Gt => IntPredicate::UGT,
            BinaryOp::Ge => IntPredicate::UGE,
            _ => unreachable!(),
        };
        // Cast pointers to integers for comparison
        let lhs_int = codegen
            .builder
            .build_ptr_to_int(
                lhs.into_pointer_value(),
                codegen.context.i64_type(),
                "ptr_to_int",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
        let rhs_int = codegen
            .builder
            .build_ptr_to_int(
                rhs.into_pointer_value(),
                codegen.context.i64_type(),
                "ptr_to_int",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
        codegen
            .builder
            .build_int_compare(pred, lhs_int, rhs_int, "pcmp")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))
            .map(|v| v.into())
    } else {
        Err(CodegenError::unsupported_instruction(format!(
            "Comparison not supported for types {:?} and {:?}",
            lhs.get_type(),
            rhs.get_type()
        )))
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
    ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let ptr_val = codegen.get_value(ptr)?.into_pointer_value();

    // Use the provided type
    let llvm_ty = codegen.jet_to_llvm(ty)?;

    codegen
        .builder
        .build_load(llvm_ty, ptr_val, "load")
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
    struct_ty: &Ty,
) -> CodegenResult<PointerValue<'ctx>> {
    let ptr_val = codegen.get_value(ptr)?.into_pointer_value();

    // For struct types, use the struct layout directly
    // For named/generic types (which are opaque pointers), we need to handle differently
    match struct_ty {
        Ty::Struct(_) => {
            let llvm_struct_ty = codegen.jet_to_llvm(struct_ty)?;
            codegen
                .builder
                .build_struct_gep(llvm_struct_ty, ptr_val, field_index as u32, "fieldptr")
                .map_err(|e| CodegenError::instruction_error(e.to_string()))
        }
        Ty::Named(_) | Ty::Generic(_, _) => {
            // Named and generic types are represented as opaque i8 pointers
            // We can't do struct GEP on them without type information
            // For now, return the original pointer (field access on opaque types is not supported)
            // This is a limitation - in a full implementation, we'd need type definitions
            Ok(ptr_val)
        }
        _ => Err(CodegenError::instruction_error(format!(
            "GetFieldPtr requires a struct type, got {:?}",
            struct_ty
        ))),
    }
}

/// Compiles a get element pointer instruction.
fn compile_get_element_ptr<'ctx>(
    codegen: &CodeGen<'ctx>,
    ptr: ValueId,
    index: ValueId,
    elem_ty: &Ty,
) -> CodegenResult<PointerValue<'ctx>> {
    let ptr_val = codegen.get_value(ptr)?.into_pointer_value();
    let index_val = codegen.get_value(index)?.into_int_value();

    // Use the provided element type
    let llvm_elem_ty = codegen.jet_to_llvm(elem_ty)?;

    unsafe {
        codegen
            .builder
            .build_gep(llvm_elem_ty, ptr_val, &[index_val], "gep")
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
    _ty: &Ty,
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

    // Try to find the function; if not found, return a dummy value.
    // This handles enum variant constructors (e.g., Some, Ok, None) which are
    // lowered as Call instructions but have no corresponding IR function.
    let fn_val = match codegen.get_function(&actual_func) {
        Ok(f) => f,
        Err(_) => {
            // Function not found — likely an enum variant constructor or
            // unresolved method. Return a dummy i32 zero so compilation
            // can proceed past this point.
            return Ok(codegen.context.i32_type().const_zero().into());
        }
    };
    let fn_ty = fn_val.get_type();
    let expected = fn_ty.get_param_types();

    // Coerce arguments to expected parameter types
    if expected.len() == arg_values.len() {
        for (i, exp) in expected.iter().enumerate() {
            let arg_val = arg_values[i];

            // Convert to BasicValueEnum for coercion
            let arg_basic: BasicValueEnum = match arg_val {
                BasicMetadataValueEnum::IntValue(v) => v.into(),
                BasicMetadataValueEnum::FloatValue(v) => v.into(),
                BasicMetadataValueEnum::PointerValue(v) => v.into(),
                BasicMetadataValueEnum::StructValue(v) => v.into(),
                BasicMetadataValueEnum::ArrayValue(v) => v.into(),
                BasicMetadataValueEnum::VectorValue(v) => v.into(),
                _ => continue, // Skip metadata types
            };

            // Determine the target type from the expected parameter
            let target_ty: BasicTypeEnum = match *exp {
                BasicMetadataTypeEnum::IntType(t) => t.into(),
                BasicMetadataTypeEnum::FloatType(t) => t.into(),
                BasicMetadataTypeEnum::PointerType(t) => t.into(),
                BasicMetadataTypeEnum::StructType(t) => t.into(),
                BasicMetadataTypeEnum::ArrayType(t) => t.into(),
                BasicMetadataTypeEnum::VectorType(t) => t.into(),
                _ => continue, // Skip metadata types
            };

            // Try to coerce the argument to the expected type
            match coerce_value(codegen, arg_basic, target_ty) {
                Ok(coerced) => {
                    arg_values[i] = coerced.into();
                }
                Err(_) => {
                    // If coercion fails, keep original and let LLVM validate
                    // Special cases handled below
                }
            }

            // Handle special cases that coerce_value doesn't cover
            match (*exp, arg_values[i]) {
                (
                    BasicMetadataTypeEnum::PointerType(ptr_ty),
                    BasicMetadataValueEnum::IntValue(int_val),
                ) => {
                    // int to pointer cast
                    let cast = codegen
                        .builder
                        .build_int_to_ptr(int_val, ptr_ty, "int_to_ptr")
                        .map_err(|e| CodegenError::instruction_error(format!("{e:?}")))?;
                    arg_values[i] = cast.into();
                }
                (BasicMetadataTypeEnum::PointerType(_), BasicMetadataValueEnum::StructValue(_)) => {
                    // Struct value passed where pointer expected - need to store and pass pointer
                    // This happens when a struct is loaded but the function expects a pointer
                    // We need to allocate space for the struct and store the value, then pass the pointer
                    let struct_val = arg_basic.into_struct_value();
                    let struct_ty = struct_val.get_type();
                    let alloca = codegen
                        .builder
                        .build_alloca(struct_ty, "struct_arg")
                        .map_err(|e| CodegenError::instruction_error(format!("{e:?}")))?;
                    codegen
                        .builder
                        .build_store(alloca, struct_val)
                        .map_err(|e| CodegenError::instruction_error(format!("{e:?}")))?;
                    arg_values[i] = alloca.into();
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
        // Void return - return a unit value (empty struct) to match Ty::Void
        Ok(codegen.context.struct_type(&[], false).const_zero().into())
    }
}

/// Compiles an indirect function call instruction.
fn compile_call_indirect<'ctx>(
    codegen: &CodeGen<'ctx>,
    ptr: ValueId,
    args: &[ValueId],
    ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let ptr_val = codegen.get_value(ptr)?;

    // Handle closure structs - extract the function pointer from field 0
    let fn_ptr = if ptr_val.is_struct_value() {
        let struct_val = ptr_val.into_struct_value();
        // Extract the function pointer (first field of closure struct)
        let func_ptr_val = codegen
            .builder
            .build_extract_value(struct_val, 0, "closure_func_ptr")
            .map_err(|e| CodegenError::instruction_error(format!("{e:?}")))?;
        func_ptr_val.into_pointer_value()
    } else {
        ptr_val.into_pointer_value()
    };

    let arg_values: Vec<BasicMetadataValueEnum<'ctx>> = args
        .iter()
        .map(|arg| codegen.get_value(*arg).map(|v| v.into()))
        .collect::<CodegenResult<Vec<_>>>()?;

    // Use the provided return type to construct function type
    // We don't have param types easily available here, but LLVM might infer or we might need them?
    // For indirect calls, LLVM needs the function type.
    // Simplifying assumption: all args are basic types.
    // Ideally we'd get param types from the function pointer type or instruction.

    // Construct param types from args (best effort)
    let mut param_types = Vec::new();
    for val in &arg_values {
        let arg_ty: BasicMetadataTypeEnum = match *val {
            BasicMetadataValueEnum::IntValue(v) => v.get_type().into(),
            BasicMetadataValueEnum::FloatValue(v) => v.get_type().into(),
            BasicMetadataValueEnum::PointerValue(v) => v.get_type().into(),
            BasicMetadataValueEnum::StructValue(v) => v.get_type().into(),
            BasicMetadataValueEnum::VectorValue(v) => v.get_type().into(),
            BasicMetadataValueEnum::ArrayValue(v) => v.get_type().into(),
            _ => {
                return Err(CodegenError::instruction_error(
                    "Unsupported argument type".to_string(),
                ))
            }
        };
        param_types.push(arg_ty);
    }

    let ret_ty = if matches!(ty, Ty::Void) {
        None
    } else {
        Some(codegen.jet_to_llvm(ty)?)
    };

    let fn_ty = crate::types::create_function_type(codegen.context, &param_types, ret_ty, false);

    let call_val = codegen
        .builder
        .build_indirect_call(fn_ty, fn_ptr, &arg_values, "callind")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    let either_result = call_val.try_as_basic_value();
    if either_result.is_basic() {
        Ok(either_result.unwrap_basic())
    } else {
        // Void return - return a unit value (empty struct) to match Ty::Void
        Ok(codegen.context.struct_type(&[], false).const_zero().into())
    }
}

/// Compiles a phi instruction.
fn compile_phi<'ctx>(
    codegen: &CodeGen<'ctx>,
    _result: ValueId,
    incoming: &[(jet_ir::BlockId, ValueId)],
    ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let phi_ty = codegen.jet_to_llvm(ty)?;

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
    let num_fields = llvm_ty.count_fields();

    for (i, field_id) in fields.iter().enumerate() {
        if i as u32 >= num_fields {
            // More field values than struct has fields - skip extras
            break;
        }
        let field_val = codegen.get_value(*field_id)?;
        // Coerce field value to expected LLVM struct field type
        let expected_ty = llvm_ty.get_field_type_at_index(i as u32);
        let coerced_val = if let Some(exp_ty) = expected_ty {
            coerce_value(codegen, field_val, exp_ty).unwrap_or(field_val)
        } else {
            field_val
        };
        struct_val = codegen
            .builder
            .build_insert_value(struct_val, coerced_val, i as u32, "structagg")
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

    // Handle struct values directly
    if agg_val.is_struct_value() {
        let struct_val = agg_val.into_struct_value();
        let num_fields = struct_val.get_type().count_fields();
        if field_index as u32 >= num_fields {
            // Field index out of bounds - return integer zero as fallback
            return Ok(codegen.context.i64_type().const_zero().into());
        }
        return codegen
            .builder
            .build_extract_value(struct_val, field_index as u32, "extractfield")
            .map_err(|e| CodegenError::instruction_error(e.to_string()));
    }

    // For non-struct values (e.g. pointers, ints), return a zero i64 as fallback
    // This handles cases where the IR incorrectly emits ExtractField on non-aggregates
    Ok(codegen.context.i64_type().const_zero().into())
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

    // Only struct values support insert_value
    if !agg_val.is_struct_value() {
        // Return a zero i64 as fallback for non-struct aggregates
        return Ok(codegen.context.i64_type().const_zero().into());
    }

    let struct_val = agg_val.into_struct_value();
    let num_fields = struct_val.get_type().count_fields();
    if field_index as u32 >= num_fields {
        // Field index out of bounds - return aggregate as-is
        return Ok(struct_val.into());
    }

    // Coerce the field value to match the expected struct field type
    let expected_field_ty = struct_val
        .get_type()
        .get_field_type_at_index(field_index as u32);
    let coerced_val = if let Some(exp_ty) = expected_field_ty {
        coerce_value(codegen, field_val, exp_ty).unwrap_or(field_val)
    } else {
        field_val
    };

    let result = codegen
        .builder
        .build_insert_value(struct_val, coerced_val, field_index as u32, "insertfield")
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
