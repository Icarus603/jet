//! Type Coercion Utilities
//!
//! This module provides utilities for coercing LLVM values between different types.
//! It handles integer widening/narrowing, float extension/truncation, pointer casts,
//! and general value coercion to match expected types.

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use inkwell::types::{BasicTypeEnum, FloatType, IntType, PointerType};
use inkwell::values::{BasicValueEnum, FloatValue, IntValue, PointerValue};

/// Coerces an integer value to a target integer type.
///
/// This handles sign extension (widening), truncation (narrowing), and no-op cases.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `val` - The integer value to coerce
/// * `target_ty` - The target integer type
///
/// # Returns
///
/// Returns the coerced integer value.
pub fn coerce_int_to_int<'ctx>(
    codegen: &CodeGen<'ctx>,
    val: IntValue<'ctx>,
    target_ty: IntType<'ctx>,
) -> CodegenResult<IntValue<'ctx>> {
    let source_width = val.get_type().get_bit_width();
    let target_width = target_ty.get_bit_width();

    if target_width > source_width {
        // Sign extend
        codegen
            .builder
            .build_int_s_extend(val, target_ty, "sext")
            .map_err(|e| CodegenError::instruction_error(format!("sext failed: {}", e)))
    } else if target_width < source_width {
        // Truncate
        codegen
            .builder
            .build_int_truncate(val, target_ty, "trunc")
            .map_err(|e| CodegenError::instruction_error(format!("trunc failed: {}", e)))
    } else {
        // Same width, no coercion needed
        Ok(val)
    }
}

/// Coerces a float value to a target float type.
///
/// This handles extension (f32 -> f64) and truncation (f64 -> f32).
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `val` - The float value to coerce
/// * `target_ty` - The target float type
///
/// # Returns
///
/// Returns the coerced float value.
pub fn coerce_float_to_float<'ctx>(
    codegen: &CodeGen<'ctx>,
    val: FloatValue<'ctx>,
    target_ty: FloatType<'ctx>,
) -> CodegenResult<FloatValue<'ctx>> {
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
            .map_err(|e| CodegenError::instruction_error(format!("fpext failed: {}", e)))
    } else if target_bits < source_bits {
        // Truncate
        codegen
            .builder
            .build_float_trunc(val, target_ty, "fptrunc")
            .map_err(|e| CodegenError::instruction_error(format!("fptrunc failed: {}", e)))
    } else {
        // Same size, no coercion needed
        Ok(val)
    }
}

/// Coerces a pointer value to a target pointer type.
///
/// This uses bitcast for pointer-to-pointer conversions.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `val` - The pointer value to coerce
/// * `target_ty` - The target pointer type
///
/// # Returns
///
/// Returns the coerced pointer value.
pub fn coerce_pointer_to_pointer<'ctx>(
    codegen: &CodeGen<'ctx>,
    val: PointerValue<'ctx>,
    target_ty: PointerType<'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    codegen
        .builder
        .build_pointer_cast(val, target_ty, "bitcast")
        .map_err(|e| CodegenError::instruction_error(format!("pointer cast failed: {}", e)))
}

/// Determines the wider of two integer types.
///
/// Returns the type with the larger bit width.
pub fn wider_int_type<'ctx>(
    _context: &'ctx inkwell::context::Context,
    a: IntType<'ctx>,
    b: IntType<'ctx>,
) -> IntType<'ctx> {
    let a_width = a.get_bit_width();
    let b_width = b.get_bit_width();
    if a_width >= b_width {
        a
    } else {
        b
    }
}

/// Determines the wider of two float types.
///
/// Returns f64 if either type is f64, otherwise f32.
pub fn wider_float_type<'ctx>(
    _context: &'ctx inkwell::context::Context,
    a: FloatType<'ctx>,
    b: FloatType<'ctx>,
) -> FloatType<'ctx> {
    // Determine bit width by comparing to known types
    let a_is_f32 = a.get_bit_width() == 32;
    let b_is_f32 = b.get_bit_width() == 32;
    let a_bits = if a_is_f32 { 32 } else { 64 };
    let b_bits = if b_is_f32 { 32 } else { 64 };
    if a_bits >= b_bits {
        a
    } else {
        b
    }
}

/// Determines the wider/common type for two values.
///
/// This is used for binary operations where both operands need to be
/// converted to a common type before the operation.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `a` - The first value
/// * `b` - The second value
///
/// # Returns
///
/// Returns the wider BasicTypeEnum that both values should be coerced to.
/// Returns None if the values are incompatible (e.g., int vs pointer).
pub fn widen_type<'ctx>(
    codegen: &CodeGen<'ctx>,
    a: BasicValueEnum<'ctx>,
    b: BasicValueEnum<'ctx>,
) -> Option<BasicTypeEnum<'ctx>> {
    match (a, b) {
        (BasicValueEnum::IntValue(a_int), BasicValueEnum::IntValue(b_int)) => {
            let wider = wider_int_type(codegen.context, a_int.get_type(), b_int.get_type());
            Some(wider.into())
        }
        (BasicValueEnum::FloatValue(a_float), BasicValueEnum::FloatValue(b_float)) => {
            let wider = wider_float_type(codegen.context, a_float.get_type(), b_float.get_type());
            Some(wider.into())
        }
        (BasicValueEnum::PointerValue(a_ptr), BasicValueEnum::PointerValue(_)) => {
            // For pointers, use the first pointer's type (they should be compatible)
            Some(a_ptr.get_type().into())
        }
        // Mixed int/float: promote to float (typically f64)
        (BasicValueEnum::IntValue(_), BasicValueEnum::FloatValue(b_float)) => {
            Some(b_float.get_type().into())
        }
        (BasicValueEnum::FloatValue(a_float), BasicValueEnum::IntValue(_)) => {
            Some(a_float.get_type().into())
        }
        // Struct types: must be identical
        (BasicValueEnum::StructValue(a_struct), BasicValueEnum::StructValue(b_struct)) => {
            if a_struct.get_type() == b_struct.get_type() {
                Some(a_struct.get_type().into())
            } else {
                None
            }
        }
        // Array types: must be identical
        (BasicValueEnum::ArrayValue(a_arr), BasicValueEnum::ArrayValue(b_arr)) => {
            if a_arr.get_type() == b_arr.get_type() {
                Some(a_arr.get_type().into())
            } else {
                None
            }
        }
        // Incompatible types
        _ => None,
    }
}

/// Coerces a value to a target type.
///
/// This is the main entry point for type coercion. It handles:
/// - Integer to integer (widening/narrowing)
/// - Float to float (extension/truncation)
/// - Pointer to pointer (bitcast)
/// - Integer to float (conversion)
/// - Float to integer (conversion)
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `val` - The value to coerce
/// * `target_ty` - The target type
///
/// # Returns
///
/// Returns the coerced value, or an error if coercion is not possible.
pub fn coerce_value<'ctx>(
    codegen: &CodeGen<'ctx>,
    val: BasicValueEnum<'ctx>,
    target_ty: BasicTypeEnum<'ctx>,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    // If already the right type, return as-is
    if val.get_type() == target_ty {
        return Ok(val);
    }

    match (val, target_ty) {
        // Integer to integer
        (BasicValueEnum::IntValue(int_val), BasicTypeEnum::IntType(target_int_ty)) => {
            coerce_int_to_int(codegen, int_val, target_int_ty).map(|v| v.into())
        }

        // Float to float
        (BasicValueEnum::FloatValue(float_val), BasicTypeEnum::FloatType(target_float_ty)) => {
            coerce_float_to_float(codegen, float_val, target_float_ty).map(|v| v.into())
        }

        // Pointer to pointer
        (BasicValueEnum::PointerValue(ptr_val), BasicTypeEnum::PointerType(target_ptr_ty)) => {
            coerce_pointer_to_pointer(codegen, ptr_val, target_ptr_ty).map(|v| v.into())
        }

        // Integer to float
        (BasicValueEnum::IntValue(int_val), BasicTypeEnum::FloatType(target_float_ty)) => codegen
            .builder
            .build_signed_int_to_float(int_val, target_float_ty, "sitofp")
            .map_err(|e| CodegenError::instruction_error(format!("int to float failed: {}", e)))
            .map(|v| v.into()),

        // Float to integer
        (BasicValueEnum::FloatValue(float_val), BasicTypeEnum::IntType(target_int_ty)) => codegen
            .builder
            .build_float_to_signed_int(float_val, target_int_ty, "fptosi")
            .map_err(|e| CodegenError::instruction_error(format!("float to int failed: {}", e)))
            .map(|v| v.into()),

        // Struct to struct (must be identical, no coercion possible)
        (BasicValueEnum::StructValue(_), BasicTypeEnum::StructType(_)) => {
            // Struct types must match exactly - if they don't, we can't coerce
            Err(CodegenError::unsupported_instruction(
                "cannot coerce between different struct types",
            ))
        }

        // Array to array (must be identical)
        (BasicValueEnum::ArrayValue(_), BasicTypeEnum::ArrayType(_)) => Err(
            CodegenError::unsupported_instruction("cannot coerce between different array types"),
        ),

        // Vector types
        (BasicValueEnum::VectorValue(_), BasicTypeEnum::VectorType(_)) => Err(
            CodegenError::unsupported_instruction("vector coercion not supported"),
        ),

        // Incompatible type combinations
        _ => Err(CodegenError::unsupported_instruction(format!(
            "cannot coerce {:?} to {:?}",
            val.get_type(),
            target_ty
        ))),
    }
}

/// Checks if two types are compatible for coercion.
///
/// Returns true if coerce_value can convert from source to target.
pub fn types_compatible(source: BasicTypeEnum<'_>, target: BasicTypeEnum<'_>) -> bool {
    use BasicTypeEnum::*;

    matches!(
        (source, target),
        (IntType(_), IntType(_))
            | (FloatType(_), FloatType(_))
            | (PointerType(_), PointerType(_))
            | (IntType(_), FloatType(_))
            | (FloatType(_), IntType(_))
    )
}

/// Gets the bit width of an integer value.
pub fn int_bit_width(val: IntValue<'_>) -> u32 {
    val.get_type().get_bit_width()
}

/// Gets the bit width of a float value (32 or 64).
pub fn float_bit_width<'ctx>(
    context: &'ctx inkwell::context::Context,
    val: FloatValue<'ctx>,
) -> u32 {
    if val.get_type() == context.f32_type() {
        32
    } else {
        64
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::CodeGen;
    use inkwell::context::Context;

    fn create_test_codegen<'ctx>(context: &'ctx Context) -> CodeGen<'ctx> {
        CodeGen::new(context, "test")
    }

    fn setup_builder_position<'ctx>(codegen: &CodeGen<'ctx>) {
        // Create a dummy function and basic block to position the builder
        let fn_type = codegen.context.void_type().fn_type(&[], false);
        let func = codegen.module.add_function("test_dummy", fn_type, None);
        let block = codegen.context.append_basic_block(func, "entry");
        codegen.builder.position_at_end(block);
    }

    #[test]
    fn test_coerce_int_widening() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);
        setup_builder_position(&codegen);

        let i32_val = context.i32_type().const_int(42, false);
        let i64_ty = context.i64_type();

        let result = coerce_int_to_int(&codegen, i32_val, i64_ty).unwrap();
        assert_eq!(result.get_type().get_bit_width(), 64);
    }

    #[test]
    fn test_coerce_int_truncation() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);
        setup_builder_position(&codegen);

        let i64_val = context.i64_type().const_int(42, false);
        let i32_ty = context.i32_type();

        let result = coerce_int_to_int(&codegen, i64_val, i32_ty).unwrap();
        assert_eq!(result.get_type().get_bit_width(), 32);
    }

    #[test]
    fn test_coerce_int_no_change() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);
        setup_builder_position(&codegen);

        let i32_val = context.i32_type().const_int(42, false);
        let i32_ty = context.i32_type();

        let result = coerce_int_to_int(&codegen, i32_val, i32_ty).unwrap();
        assert_eq!(result.get_type().get_bit_width(), 32);
    }

    #[test]
    fn test_coerce_float_extension() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);
        setup_builder_position(&codegen);

        let f32_val = context.f32_type().const_float(std::f64::consts::PI);
        let f64_ty = context.f64_type();

        let result = coerce_float_to_float(&codegen, f32_val, f64_ty).unwrap();
        // f64 type check
        assert!(result.get_type() == context.f64_type());
    }

    #[test]
    fn test_coerce_float_truncation() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);
        setup_builder_position(&codegen);

        let f64_val = context.f64_type().const_float(std::f64::consts::PI);
        let f32_ty = context.f32_type();

        let result = coerce_float_to_float(&codegen, f64_val, f32_ty).unwrap();
        assert!(result.get_type() == context.f32_type());
    }

    #[test]
    fn test_wider_int_type() {
        let context = Context::create();

        let i32_ty = context.i32_type();
        let i64_ty = context.i64_type();

        assert_eq!(wider_int_type(&context, i32_ty, i64_ty), i64_ty);
        assert_eq!(wider_int_type(&context, i64_ty, i32_ty), i64_ty);
        assert_eq!(wider_int_type(&context, i32_ty, i32_ty), i32_ty);
    }

    #[test]
    fn test_wider_float_type() {
        let context = Context::create();

        let f32_ty = context.f32_type();
        let f64_ty = context.f64_type();

        assert_eq!(wider_float_type(&context, f32_ty, f64_ty), f64_ty);
        assert_eq!(wider_float_type(&context, f64_ty, f32_ty), f64_ty);
        assert_eq!(wider_float_type(&context, f32_ty, f32_ty), f32_ty);
    }

    #[test]
    fn test_widen_type_integers() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let i32_val = context.i32_type().const_int(1, false).into();
        let i64_val = context.i64_type().const_int(2, false).into();

        let wider = widen_type(&codegen, i32_val, i64_val).unwrap();
        assert!(wider.is_int_type());
        assert_eq!(wider.into_int_type().get_bit_width(), 64);
    }

    #[test]
    fn test_widen_type_floats() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let f32_val = context.f32_type().const_float(1.0).into();
        let f64_val = context.f64_type().const_float(2.0).into();

        let wider = widen_type(&codegen, f32_val, f64_val).unwrap();
        assert!(wider.is_float_type());
        assert!(wider.into_float_type() == context.f64_type());
    }

    #[test]
    fn test_types_compatible() {
        let context = Context::create();

        let i32_ty = context.i32_type().into();
        let i64_ty = context.i64_type().into();
        let f32_ty = context.f32_type().into();
        let ptr_ty = context
            .i8_type()
            .ptr_type(inkwell::AddressSpace::default())
            .into();

        assert!(types_compatible(i32_ty, i64_ty));
        assert!(types_compatible(i32_ty, f32_ty)); // int to float
        assert!(types_compatible(f32_ty, i32_ty)); // float to int
        assert!(types_compatible(ptr_ty, ptr_ty));
    }
}
