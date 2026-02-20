//! Closure Compilation
//!
//! This module provides LLVM code generation for Jet closures.
//!
//! # Closure Representation
//!
//! A closure in Jet is represented as a pair:
//! - **Function pointer**: Points to the closure's code
//! - **Environment pointer**: Points to the captured values
//!
//! The environment is heap-allocated and reference-counted (or GC'd).
//!
//! # Memory Layout
//!
//! ```text
//! Closure:
//!   +------------------+
//!   | function pointer |  8 bytes
//!   +------------------+
//!   | environment ptr  |  8 bytes
//!   +------------------+
//!
//! Environment:
//!   +------------------+
//!   | refcount / GC    |  8 bytes
//!   +------------------+
//!   | captured value 0 |  size varies
//!   +------------------+
//!   | captured value 1 |
//!   +------------------+
//!   | ...              |
//!   +------------------+
//! ```
//!
//! # Calling Convention
//!
//! Closures are called with the environment as the first implicit parameter:
//! ```text
//! fn closure_body(env: *mut u8, arg1: T1, arg2: T2) -> R
//! ```

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use crate::gc::{emit_gc_alloc, AllocationSite};
use crate::types::TypeMapping;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use jet_ir::{Ty, ValueId};
/// Information about a closure being compiled.
#[derive(Debug, Clone)]
pub struct ClosureInfo {
    /// The name of the closure (for debugging).
    pub name: String,
    /// The captured values (variable IDs and their types).
    pub captures: Vec<(ValueId, Ty)>,
    /// The function type (parameters and return type).
    pub param_types: Vec<Ty>,
    pub return_type: Ty,
}

/// Creates the LLVM struct type for a closure.
///
/// A closure is a struct containing:
/// - function pointer
/// - environment pointer
pub fn closure_type<'ctx>(context: &'ctx inkwell::context::Context) -> StructType<'ctx> {
    let fn_ptr_type = context
        .i8_type()
        .fn_type(
            &[context.i8_type().ptr_type(AddressSpace::default()).into()],
            false,
        )
        .ptr_type(AddressSpace::default());
    let env_ptr_type = context.i8_type().ptr_type(AddressSpace::default());

    context.struct_type(&[fn_ptr_type.into(), env_ptr_type.into()], false)
}

/// Compiles a closure literal.
///
/// This creates:
/// 1. An environment structure with captured values
/// 2. A closure struct containing the function pointer and environment
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `info` - Information about the closure
/// * `captured_values` - The LLVM values being captured
/// * `function` - The LLVM function that implements the closure body
///
/// # Returns
///
/// Returns the closure struct value.
pub fn compile_closure<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    info: &ClosureInfo,
    captured_values: &[BasicValueEnum<'ctx>],
    function: FunctionValue<'ctx>,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    // Create the environment
    let env_ptr = create_environment(codegen, info, captured_values)?;

    // Create the closure struct
    let closure_ty = closure_type(codegen.context);
    let mut closure_val = closure_ty.const_zero();

    // Store function pointer
    let fn_ptr = function.as_global_value().as_pointer_value();
    closure_val = codegen
        .builder
        .build_insert_value(closure_val, fn_ptr, 0, "closure_fn_ptr")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
        .into_struct_value();

    // Store environment pointer
    closure_val = codegen
        .builder
        .build_insert_value(closure_val, env_ptr, 1, "closure_env_ptr")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
        .into_struct_value();

    Ok(closure_val.into())
}

/// Creates an environment for captured values.
///
/// The environment is heap-allocated and contains:
/// - Reference count (for GC)
/// - Captured values
fn create_environment<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    info: &ClosureInfo,
    captured_values: &[BasicValueEnum<'ctx>],
) -> CodegenResult<PointerValue<'ctx>> {
    // Calculate environment size
    let mut env_size = 8u64; // Reference count
    let mut field_offsets = Vec::new();

    for (_, ty) in info.captures.iter() {
        let field_size = crate::gc::calculate_type_size(ty)?;
        let field_align = crate::gc::calculate_type_align(ty)?;

        // Align offset
        env_size = env_size.next_multiple_of(field_align);
        field_offsets.push(env_size);
        env_size += field_size;
    }

    // Allocate environment on GC heap
    let env_ptr = emit_gc_alloc(
        codegen,
        AllocationSite {
            size: env_size,
            align: 8,
            type_id: 0, // Special type ID for closures
            location: Some(format!("env_{}", info.name)),
        },
    )?;

    // Initialize reference count to 1
    let refcount_ptr = env_ptr;
    let refcount_val = codegen.context.i64_type().const_int(1, false);
    codegen
        .builder
        .build_store(refcount_ptr, refcount_val)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Store captured values
    for (i, (value, offset)) in captured_values.iter().zip(field_offsets.iter()).enumerate() {
        let field_ptr = unsafe {
            codegen
                .builder
                .build_gep(
                    codegen.context.i8_type(),
                    env_ptr,
                    &[codegen.context.i64_type().const_int(*offset, false)],
                    &format!("env_field_{}", i),
                )
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?
        };

        // Cast to the correct type and store
        let value_ty = value.get_type();
        let typed_ptr = codegen
            .builder
            .build_pointer_cast(
                field_ptr,
                value_ty.ptr_type(AddressSpace::default()),
                &format!("env_field_{}_typed", i),
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

        codegen
            .builder
            .build_store(typed_ptr, *value)
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
    }

    Ok(env_ptr)
}

/// Extracts the function pointer from a closure.
pub fn get_closure_function<'ctx>(
    codegen: &CodeGen<'ctx>,
    closure: BasicValueEnum<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let fn_ptr_val = codegen
        .builder
        .build_extract_value(closure.into_struct_value(), 0, "closure_fn")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // We can't easily convert a PointerValue back to a FunctionValue
    // In practice, the caller should track the function separately
    let _ = fn_ptr_val; // Silence unused variable warning
    Err(CodegenError::unsupported_instruction(
        "cannot extract function from closure at runtime - use indirect call",
    ))
}

/// Extracts the environment pointer from a closure.
pub fn get_closure_environment<'ctx>(
    codegen: &CodeGen<'ctx>,
    closure: BasicValueEnum<'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    let env_ptr = codegen
        .builder
        .build_extract_value(closure.into_struct_value(), 1, "closure_env")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(env_ptr.into_pointer_value())
}

/// Generates a call to a closure.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `closure` - The closure value
/// * `args` - The arguments to pass (not including environment)
/// * `return_type` - The expected return type
///
/// # Returns
///
/// Returns the result of the call.
pub fn compile_closure_call<'ctx>(
    codegen: &CodeGen<'ctx>,
    closure: BasicValueEnum<'ctx>,
    args: &[BasicValueEnum<'ctx>],
    return_type: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    // Extract environment and function pointer
    let env_ptr = get_closure_environment(codegen, closure)?;
    let fn_ptr_val = codegen
        .builder
        .build_extract_value(closure.into_struct_value(), 0, "closure_fn_ptr")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Build argument list: env first, then actual args
    let mut call_args: Vec<BasicMetadataValueEnum<'ctx>> = vec![env_ptr.into()];
    for arg in args {
        call_args.push((*arg).into());
    }

    // Create function type for indirect call
    let i8_ptr = codegen.context.i8_type().ptr_type(AddressSpace::default());
    let fn_type = if return_type.is_void() {
        codegen.context.void_type().fn_type(&[i8_ptr.into()], false)
    } else {
        // Use i8* as generic return, caller will cast
        i8_ptr.fn_type(&[i8_ptr.into()], false)
    };

    // Cast function pointer to the right type
    let fn_ptr = fn_ptr_val.into_pointer_value();

    // Make indirect call
    let result = codegen
        .builder
        .build_indirect_call(fn_type, fn_ptr, &call_args, "closure_call")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    if return_type.is_void() {
        Ok(codegen.context.i32_type().const_zero().into())
    } else {
        result
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::instruction_error("closure call returned void"))
    }
}

/// Loads a captured value from the environment.
///
/// This is used in the closure body to access captured variables.
pub fn load_captured_value<'ctx>(
    codegen: &CodeGen<'ctx>,
    env_ptr: PointerValue<'ctx>,
    index: usize,
    ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    // Calculate offset (skip refcount)
    let mut offset = 8u64;
    // In a real implementation, we'd look up the actual offset from the capture info
    // For now, we assume all captures are pointer-sized
    offset += (index as u64) * 8;

    // Get field pointer
    let field_ptr = unsafe {
        codegen
            .builder
            .build_gep(
                codegen.context.i8_type(),
                env_ptr,
                &[codegen.context.i64_type().const_int(offset, false)],
                &format!("capture_{}", index),
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?
    };

    // Cast to the right type
    let llvm_ty = codegen.jet_to_llvm(ty)?;
    let typed_ptr = codegen
        .builder
        .build_pointer_cast(
            field_ptr,
            llvm_ty.ptr_type(AddressSpace::default()),
            &format!("capture_{}_ptr", index),
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Load the value
    codegen
        .builder
        .build_load(llvm_ty, typed_ptr, &format!("capture_{}_val", index))
        .map_err(|e| CodegenError::instruction_error(e.to_string()))
}

/// Creates a trampoline function for calling a closure with a standard calling convention.
///
/// A trampoline is a small wrapper function that adapts between the standard C calling
/// convention and Jet's closure calling convention. This is essential when passing
/// closures to C code or the runtime system.
///
/// The trampoline has a standard C calling convention:
///   fn trampoline(arg1: T1, arg2: T2, ...) -> R
///
/// It internally calls the closure with Jet's convention:
///   fn closure(env: *mut u8, arg1: T1, arg2: T2, ...) -> R
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `closure` - The closure value (struct containing function pointer and environment)
/// * `name` - The name for the trampoline function
/// * `param_types` - The types of the parameters (excluding environment)
/// * `return_type` - The return type of the closure
///
/// # Returns
///
/// Returns the created trampoline function with standard C calling convention.
pub fn create_closure_trampoline<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    closure: BasicValueEnum<'ctx>,
    name: &str,
    param_types: &[Ty],
    return_type: &Ty,
) -> CodegenResult<FunctionValue<'ctx>> {
    // Build the parameter types for the trampoline (standard C calling convention)
    let llvm_param_types: Vec<BasicMetadataTypeEnum<'ctx>> = param_types
        .iter()
        .map(|ty| codegen.jet_to_llvm_metadata(ty))
        .collect::<CodegenResult<Vec<_>>>()?;

    // Build the return type
    let llvm_ret_type = if return_type.is_void() {
        None
    } else {
        Some(codegen.jet_to_llvm(return_type)?)
    };

    // Create the trampoline function type with standard calling convention
    let fn_type = match llvm_ret_type {
        Some(ret_ty) => ret_ty.fn_type(&llvm_param_types, false),
        None => codegen
            .context
            .void_type()
            .fn_type(&llvm_param_types, false),
    };

    let trampoline_fn = codegen
        .module
        .add_function(&format!("trampoline_{}", name), fn_type, None);

    // Create the entry basic block
    let entry_block = codegen.context.append_basic_block(trampoline_fn, "entry");
    let builder = codegen.context.create_builder();
    builder.position_at_end(entry_block);

    // Extract the function pointer and environment from the closure
    let closure_struct = closure.into_struct_value();

    // Extract function pointer (field 0)
    let fn_ptr_val = builder
        .build_extract_value(closure_struct, 0, "closure_fn_ptr")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Extract environment pointer (field 1)
    let env_ptr = builder
        .build_extract_value(closure_struct, 1, "closure_env_ptr")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Build the argument list for the closure call
    // First argument is always the environment pointer
    let mut call_args: Vec<BasicMetadataValueEnum<'ctx>> = vec![env_ptr.into()];

    // Add the trampoline parameters as subsequent arguments
    for (i, _) in param_types.iter().enumerate() {
        let param = trampoline_fn
            .get_nth_param(i as u32)
            .ok_or_else(|| CodegenError::instruction_error(format!("missing param {}", i)))?;
        call_args.push(param.into());
    }

    // Cast the function pointer to the correct type for the closure call
    // Closure type: fn(env: *mut u8, ...) -> ret
    let i8_ptr = codegen.context.i8_type().ptr_type(AddressSpace::default());

    // Build the closure function type (with environment as first param)
    let mut closure_param_types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![i8_ptr.into()];
    closure_param_types.extend(llvm_param_types.iter().cloned());

    let closure_fn_type = match llvm_ret_type {
        Some(ret_ty) => ret_ty.fn_type(&closure_param_types, false),
        None => codegen
            .context
            .void_type()
            .fn_type(&closure_param_types, false),
    };

    let fn_ptr = fn_ptr_val.into_pointer_value();

    // Make the indirect call to the closure
    let call_result = builder
        .build_indirect_call(closure_fn_type, fn_ptr, &call_args, "closure_call")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Return the result (or void)
    if return_type.is_void() {
        builder
            .build_return(None)
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
    } else {
        let result = call_result
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::instruction_error("closure call returned void"))?;
        builder
            .build_return(Some(&result))
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
    }

    Ok(trampoline_fn)
}

/// Creates a trampoline for a closure with no arguments.
///
/// This is a convenience function for creating trampolines for closures
/// that take no arguments (other than the implicit environment).
pub fn create_closure_trampoline_0<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    closure: BasicValueEnum<'ctx>,
    name: &str,
    return_type: &Ty,
) -> CodegenResult<FunctionValue<'ctx>> {
    create_closure_trampoline(codegen, closure, name, &[], return_type)
}

/// Creates a trampoline for a closure with one argument.
pub fn create_closure_trampoline_1<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    closure: BasicValueEnum<'ctx>,
    name: &str,
    param_type: &Ty,
    return_type: &Ty,
) -> CodegenResult<FunctionValue<'ctx>> {
    create_closure_trampoline(
        codegen,
        closure,
        name,
        std::slice::from_ref(param_type),
        return_type,
    )
}

/// Creates a trampoline for a closure with two arguments.
pub fn create_closure_trampoline_2<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    closure: BasicValueEnum<'ctx>,
    name: &str,
    param1_type: &Ty,
    param2_type: &Ty,
    return_type: &Ty,
) -> CodegenResult<FunctionValue<'ctx>> {
    create_closure_trampoline(
        codegen,
        closure,
        name,
        &[param1_type.clone(), param2_type.clone()],
        return_type,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::context::Context;

    #[test]
    fn test_closure_type() {
        let context = Context::create();
        let ty = closure_type(&context);

        assert_eq!(ty.count_fields(), 2);
        // First field is function pointer
        // Second field is environment pointer
    }

    #[test]
    fn test_closure_info() {
        let info = ClosureInfo {
            name: "test_closure".to_string(),
            captures: vec![(ValueId::new(0), Ty::I32), (ValueId::new(1), Ty::I64)],
            param_types: vec![Ty::I32],
            return_type: Ty::I32,
        };

        assert_eq!(info.captures.len(), 2);
        assert_eq!(info.param_types.len(), 1);
    }

    #[test]
    fn test_create_closure_trampoline_0() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        // Create a simple closure value
        let closure_ty = closure_type(&context);
        let closure_val = closure_ty.const_zero();

        // Create a trampoline with no arguments
        let trampoline =
            create_closure_trampoline_0(&mut codegen, closure_val.into(), "test", &Ty::I32)
                .unwrap();

        // Check function name
        assert_eq!(trampoline.get_name().to_str().unwrap(), "trampoline_test");

        // Check that the function has an entry block
        assert!(!trampoline.get_basic_blocks().is_empty());

        // Verify the function
        assert!(trampoline.verify(true));
    }

    #[test]
    fn test_create_closure_trampoline_1() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        // Create a simple closure value
        let closure_ty = closure_type(&context);
        let closure_val = closure_ty.const_zero();

        // Create a trampoline with one argument
        let trampoline = create_closure_trampoline_1(
            &mut codegen,
            closure_val.into(),
            "test",
            &Ty::I32,
            &Ty::I32,
        )
        .unwrap();

        // Check function name
        assert_eq!(trampoline.get_name().to_str().unwrap(), "trampoline_test");

        // Check that the function has one parameter (plus env is internal)
        assert_eq!(trampoline.count_params(), 1);

        // Check that the function has an entry block
        assert!(!trampoline.get_basic_blocks().is_empty());

        // Verify the function
        assert!(trampoline.verify(true));
    }

    #[test]
    fn test_create_closure_trampoline_2() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        // Create a simple closure value
        let closure_ty = closure_type(&context);
        let closure_val = closure_ty.const_zero();

        // Create a trampoline with two arguments
        let trampoline = create_closure_trampoline_2(
            &mut codegen,
            closure_val.into(),
            "test",
            &Ty::I32,
            &Ty::I32,
            &Ty::I32,
        )
        .unwrap();

        // Check function name
        assert_eq!(trampoline.get_name().to_str().unwrap(), "trampoline_test");

        // Check that the function has two parameters
        assert_eq!(trampoline.count_params(), 2);

        // Check that the function has an entry block
        assert!(!trampoline.get_basic_blocks().is_empty());

        // Verify the function
        assert!(trampoline.verify(true));
    }
}
