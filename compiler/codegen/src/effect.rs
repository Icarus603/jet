//! Effect Handling
//!
//! This module provides support for compiling Jet's effect system to LLVM.
//! Effects are compiled using a combination of:
//! - CPS transformation for effectful functions
//! - Handler dispatch tables for dynamically handled effects
//! - Runtime calls for perform and resume operations

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use jet_ir::{BlockId, Effect, Function, ValueId};

/// Information about an effect handler.
#[derive(Debug, Clone)]
pub struct HandlerInfo {
    /// The effect type being handled
    pub effect: Effect,
    /// The handler function
    pub handler_fn: String,
    /// The resume block
    pub resume_block: BlockId,
    /// The handler block
    pub handler_block: BlockId,
}

/// Compiles effect-related function attributes.
pub fn compile_effect_attributes<'ctx>(
    _codegen: &CodeGen<'ctx>,
    func: FunctionValue<'ctx>,
    effects: &[Effect],
) {
    // Mark the function with effect attributes
    // This is used for optimization and analysis
    for effect in effects {
        match effect {
            Effect::Async => {
                // Async functions have special calling convention
                func.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMCCallConv as u32);
            }
            Effect::Diverges => {
                // Mark as noreturn
                func.add_attribute(
                    inkwell::attributes::AttributeLoc::Function,
                    _codegen.context.create_enum_attribute(
                        inkwell::attributes::Attribute::get_named_enum_kind_id("noreturn"),
                        0,
                    ),
                );
            }
            _ => {}
        }
    }
}

/// Creates a handler dispatch table for effect operations.
///
/// The dispatch table is a struct containing function pointers
/// for each operation in an effect.
pub fn create_handler_dispatch_table<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    handlers: &[HandlerInfo],
) -> CodegenResult<PointerValue<'ctx>> {
    // Create a struct type for the dispatch table
    // Each entry is a function pointer
    let fn_ptr_type = codegen
        .context
        .i8_type()
        .fn_type(&[], false)
        .ptr_type(AddressSpace::default());

    let mut field_types: Vec<_> = Vec::new();
    for _ in handlers {
        field_types.push(fn_ptr_type.into());
    }

    let table_type = codegen.context.struct_type(&field_types, false);

    // Allocate the table on the stack
    let table_alloca = codegen
        .builder
        .build_alloca(table_type, "handler_table")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Fill in the function pointers
    for (i, handler) in handlers.iter().enumerate() {
        let fn_val = codegen.get_function(&handler.handler_fn)?;
        let fn_ptr = fn_val.as_global_value().as_pointer_value();

        // Get pointer to the field
        let field_ptr = codegen
            .builder
            .build_struct_gep(table_type, table_alloca, i as u32, "handler_entry")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

        // Store the function pointer
        codegen
            .builder
            .build_store(field_ptr, fn_ptr)
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
    }

    Ok(table_alloca)
}

/// Compiles a perform operation.
///
/// Performing an effect involves:
/// 1. Looking up the handler in the current context
/// 2. Calling the handler function
/// 3. Setting up the continuation for resume
pub fn compile_perform<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    result: ValueId,
    effect_id: u32,
    operation_id: u32,
    args: &[ValueId],
) -> CodegenResult<BasicValueEnum<'ctx>> {
    // Get or create the runtime perform function
    let perform_fn = get_or_create_perform_fn(codegen)?;

    // Pack the arguments
    let llvm_args: Vec<BasicMetadataValueEnum<'ctx>> = args
        .iter()
        .map(|arg| codegen.get_value(*arg).map(|v| v.into()))
        .collect::<CodegenResult<Vec<_>>>()?;

    // Build the effect tag and operation tag
    let effect_tag = codegen
        .context
        .i32_type()
        .const_int(effect_id as u64, false);
    let op_tag = codegen
        .context
        .i32_type()
        .const_int(operation_id as u64, false);

    // Get the current handler context (passed as implicit parameter)
    // For now, we use a null pointer as a placeholder
    let handler_ctx = codegen
        .context
        .i8_type()
        .ptr_type(AddressSpace::default())
        .const_null();

    // Call the runtime perform function
    let call_args: Vec<BasicMetadataValueEnum<'ctx>> =
        vec![effect_tag.into(), op_tag.into(), handler_ctx.into()];
    // Add the actual arguments
    let call_args: Vec<BasicMetadataValueEnum<'ctx>> =
        call_args.into_iter().chain(llvm_args).collect();

    let result_val = codegen
        .builder
        .build_call(perform_fn, &call_args, "perform_result")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    let value = result_val
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::instruction_error("perform returned void"))?;

    codegen.set_value(result, value);
    Ok(value)
}

/// Compiles a resume operation.
///
/// Resume continues execution from an effect handler with a value.
pub fn compile_resume<'ctx>(codegen: &CodeGen<'ctx>, value: ValueId) -> CodegenResult<()> {
    let val = codegen.get_value(value)?;

    // Get or create the runtime resume function
    let resume_fn = get_or_create_resume_fn(codegen)?;

    // Get the continuation context
    let cont_ctx = codegen
        .context
        .i8_type()
        .ptr_type(AddressSpace::default())
        .const_null();

    codegen
        .builder
        .build_call(resume_fn, &[cont_ctx.into(), val.into()], "resume")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(())
}

/// Gets or creates the runtime perform function.
fn get_or_create_perform_fn<'ctx>(codegen: &CodeGen<'ctx>) -> CodegenResult<FunctionValue<'ctx>> {
    let name = "jet_rt_perform";

    if let Some(func) = codegen.module.get_function(name) {
        return Ok(func);
    }

    // Create the function type: i32 effect_id, i32 op_id, i8* ctx, ... -> i8*
    let fn_type = codegen
        .context
        .i8_type()
        .ptr_type(AddressSpace::default())
        .fn_type(
            &[
                codegen.context.i32_type().into(),
                codegen.context.i32_type().into(),
                codegen
                    .context
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .into(),
            ],
            true, // varargs
        );

    Ok(codegen.module.add_function(name, fn_type, None))
}

/// Gets or creates the runtime resume function.
fn get_or_create_resume_fn<'ctx>(codegen: &CodeGen<'ctx>) -> CodegenResult<FunctionValue<'ctx>> {
    let name = "jet_rt_resume";

    if let Some(func) = codegen.module.get_function(name) {
        return Ok(func);
    }

    // Create the function type: i8* ctx, i8* value -> void
    let fn_type = codegen.context.void_type().fn_type(
        &[
            codegen
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            codegen
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
        ],
        false,
    );

    Ok(codegen.module.add_function(name, fn_type, None))
}

/// Compiles a try-catch style error handling.
///
/// This generates code that:
/// 1. Sets up a landing pad for errors
/// 2. Calls the function
/// 3. Branches to success or failure based on result
pub fn compile_try_catch<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    func: FunctionValue<'ctx>,
    args: &[BasicMetadataValueEnum<'ctx>],
    success_block: inkwell::basic_block::BasicBlock<'ctx>,
    failure_block: inkwell::basic_block::BasicBlock<'ctx>,
) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
    // Call the function
    let result = codegen
        .builder
        .build_call(func, args, "try_call")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Check if the call succeeded (this depends on the calling convention)
    // For now, we assume the function returns a Result-like structure
    let value = result.try_as_basic_value().basic();

    // Branch based on success/failure
    // In a real implementation, we'd check a status flag
    let cond = codegen.context.bool_type().const_int(1, false);
    codegen
        .builder
        .build_conditional_branch(cond, success_block, failure_block)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(value)
}

/// Creates a continuation for CPS transformation.
///
/// A continuation is a function that represents "the rest of the computation"
/// after an effect is performed.
pub fn create_continuation<'ctx>(
    _codegen: &mut CodeGen<'ctx>,
    _name: &str,
    _resume_block: BlockId,
    _captured_values: &[ValueId],
) -> CodegenResult<FunctionValue<'ctx>> {
    // This is a placeholder for CPS transformation
    // In a full implementation, this would:
    // 1. Create a new function representing the continuation
    // 2. Capture the necessary values
    // 3. Return the continuation function
    Err(CodegenError::unsupported_instruction(
        "CPS transformation not yet implemented",
    ))
}

/// Compiles an async function.
///
/// Async functions are transformed into state machines that can be suspended
/// and resumed.
pub fn compile_async_function<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    func: &Function,
) -> CodegenResult<FunctionValue<'ctx>> {
    // First, compile the function normally
    use crate::function::compile_function;
    let fn_val = compile_function(codegen, func)?;

    // Mark as async
    fn_val.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMCCallConv as u32);

    // Add async attribute
    fn_val.add_attribute(
        inkwell::attributes::AttributeLoc::Function,
        codegen.context.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id("async"),
            0,
        ),
    );

    Ok(fn_val)
}

/// Compiles an await operation.
///
/// Await suspends the current async function until the future completes.
pub fn compile_await<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    future: ValueId,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let future_val = codegen.get_value(future)?;

    // Get or create the runtime await function
    let await_fn = get_or_create_await_fn(codegen)?;

    let result = codegen
        .builder
        .build_call(await_fn, &[future_val.into()], "await_result")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    result
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::instruction_error("await returned void"))
}

/// Gets or creates the runtime await function.
fn get_or_create_await_fn<'ctx>(codegen: &CodeGen<'ctx>) -> CodegenResult<FunctionValue<'ctx>> {
    let name = "jet_rt_await";

    if let Some(func) = codegen.module.get_function(name) {
        return Ok(func);
    }

    // Create the function type: i8* future -> i8* result
    let fn_type = codegen
        .context
        .i8_type()
        .ptr_type(AddressSpace::default())
        .fn_type(
            &[codegen
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into()],
            false,
        );

    Ok(codegen.module.add_function(name, fn_type, None))
}

/// Compiles a spawn operation for structured concurrency.
pub fn compile_spawn<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    func: FunctionValue<'ctx>,
    args: &[BasicMetadataValueEnum<'ctx>],
) -> CodegenResult<BasicValueEnum<'ctx>> {
    // Get or create the runtime spawn function
    let spawn_fn = get_or_create_spawn_fn(codegen)?;

    // Create a closure for the function
    let closure = create_closure(codegen, func, args)?;

    let result = codegen
        .builder
        .build_call(spawn_fn, &[closure.into()], "spawn")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    result
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::instruction_error("spawn returned void"))
}

/// Gets or creates the runtime spawn function.
fn get_or_create_spawn_fn<'ctx>(codegen: &CodeGen<'ctx>) -> CodegenResult<FunctionValue<'ctx>> {
    let name = "jet_rt_spawn";

    if let Some(func) = codegen.module.get_function(name) {
        return Ok(func);
    }

    // Create the function type: i8* closure -> i64 task_id
    let fn_type = codegen.context.i64_type().fn_type(
        &[codegen
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .into()],
        false,
    );

    Ok(codegen.module.add_function(name, fn_type, None))
}

/// Creates a closure for a function call.
fn create_closure<'ctx>(
    codegen: &CodeGen<'ctx>,
    func: FunctionValue<'ctx>,
    _args: &[BasicMetadataValueEnum<'ctx>],
) -> CodegenResult<PointerValue<'ctx>> {
    // Allocate a struct to hold the function pointer and captured args
    let fn_ptr_type = func.get_type().ptr_type(AddressSpace::default());

    // For simplicity, just pack the function pointer
    // In a real implementation, we'd also capture the args
    let closure_type = codegen.context.struct_type(&[fn_ptr_type.into()], false);

    let closure_alloca = codegen
        .builder
        .build_alloca(closure_type, "closure")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Store the function pointer
    let fn_ptr_field = codegen
        .builder
        .build_struct_gep(closure_type, closure_alloca, 0, "fn_ptr_field")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    codegen
        .builder
        .build_store(fn_ptr_field, func.as_global_value().as_pointer_value())
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(closure_alloca)
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::context::Context;

    #[test]
    fn test_perform_fn_creation() {
        let context = Context::create();
        let codegen = CodeGen::new(&context, "test");

        let perform_fn = get_or_create_perform_fn(&codegen).unwrap();
        assert_eq!(perform_fn.get_name().to_str().unwrap(), "jet_rt_perform");
        // Function should have no basic blocks (just a declaration)
        assert!(perform_fn.get_basic_blocks().is_empty());
    }

    #[test]
    fn test_resume_fn_creation() {
        let context = Context::create();
        let codegen = CodeGen::new(&context, "test");

        let resume_fn = get_or_create_resume_fn(&codegen).unwrap();
        assert_eq!(resume_fn.get_name().to_str().unwrap(), "jet_rt_resume");
        assert!(resume_fn.get_basic_blocks().is_empty());
    }

    #[test]
    fn test_await_fn_creation() {
        let context = Context::create();
        let codegen = CodeGen::new(&context, "test");

        let await_fn = get_or_create_await_fn(&codegen).unwrap();
        assert_eq!(await_fn.get_name().to_str().unwrap(), "jet_rt_await");
        assert!(await_fn.get_basic_blocks().is_empty());
    }

    #[test]
    fn test_spawn_fn_creation() {
        let context = Context::create();
        let codegen = CodeGen::new(&context, "test");

        let spawn_fn = get_or_create_spawn_fn(&codegen).unwrap();
        assert_eq!(spawn_fn.get_name().to_str().unwrap(), "jet_rt_spawn");
        assert!(spawn_fn.get_basic_blocks().is_empty());
    }
}
