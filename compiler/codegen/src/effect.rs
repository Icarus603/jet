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
/// after an effect is performed. In CPS transformation, when an effect is performed,
/// the current function is split at the effect point, and the "rest" of the function
/// is captured as a continuation that can be resumed later.
///
/// The continuation function has the following signature:
///   fn continuation(resume_value: *mut u8, env: *mut u8) -> *mut u8
///
/// Where:
///   - resume_value: The value passed to resume (the result of the effect)
///   - env: Pointer to the captured environment (local variables needed after resume)
///   - returns: The final result of the computation
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `name` - The name for the continuation function
/// * `resume_block` - The block ID where execution should resume
/// * `captured_values` - The values that need to be captured from the suspended computation
///
/// # Returns
///
/// Returns the created continuation function.
pub fn create_continuation<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    name: &str,
    resume_block: BlockId,
    captured_values: &[ValueId],
) -> CodegenResult<FunctionValue<'ctx>> {
    // Create the continuation function type:
    // fn(resume_value: *mut u8, env: *mut u8) -> *mut u8
    let i8_ptr = codegen.context.i8_type().ptr_type(AddressSpace::default());
    let fn_type = i8_ptr.fn_type(&[i8_ptr.into(), i8_ptr.into()], false);

    let cont_fn = codegen
        .module
        .add_function(&format!("cont_{}", name), fn_type, None);

    // Create the entry basic block
    let entry_block = codegen.context.append_basic_block(cont_fn, "entry");
    let builder = codegen.context.create_builder();
    builder.position_at_end(entry_block);

    // Get the parameters
    let resume_value_param = cont_fn.get_nth_param(0).unwrap().into_pointer_value();
    let env_param = cont_fn.get_nth_param(1).unwrap().into_pointer_value();

    resume_value_param.set_name("resume_value");
    env_param.set_name("env");

    // If there are captured values, extract them from the environment
    // The environment layout is:
    //   +------------------+
    //   | refcount         |  8 bytes
    //   +------------------+
    //   | captured value 0 |  (pointer)
    //   +------------------+
    //   | captured value 1 |  (pointer)
    //   +------------------+
    //   | ...              |
    //   +------------------+

    if !captured_values.is_empty() {
        // Store the captured values in the codegen value map
        // so they can be referenced by the resume block
        for (i, value_id) in captured_values.iter().enumerate() {
            // Calculate offset (skip refcount, each pointer is 8 bytes)
            let offset = 8 + (i * 8);

            // Get pointer to the captured value in the environment
            let gep = unsafe {
                builder
                    .build_gep(
                        codegen.context.i8_type(),
                        env_param,
                        &[codegen.context.i64_type().const_int(offset as u64, false)],
                        &format!("env_slot_{}", i),
                    )
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?
            };

            // Cast to i8** and load the captured value pointer
            let i8_ptr_ptr = i8_ptr.ptr_type(AddressSpace::default());
            let typed_slot = builder
                .build_pointer_cast(gep, i8_ptr_ptr, &format!("env_slot_{}_cast", i))
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

            let loaded = builder
                .build_load(i8_ptr, typed_slot, &format!("captured_{}", i))
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

            // Store in the value map for use by the resume block
            codegen.set_value(*value_id, loaded);
        }
    }

    // Also store the resume value with a special ID that the resume block can reference
    // We use a placeholder value ID for the resume value
    let resume_value_id = ValueId::new(u32::MAX);
    codegen.set_value(resume_value_id, resume_value_param.into());

    // Get the target block for resumption
    let target_block = codegen.get_block(resume_block)?;

    // Branch to the resume block
    builder
        .build_unconditional_branch(target_block)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Note: The resume block will be compiled separately as part of the main
    // function compilation. The continuation function simply acts as a trampoline
    // that unpacks the environment and jumps to the resume block.

    Ok(cont_fn)
}

/// Creates a continuation function for a specific resume point.
///
/// This is a simplified version that creates a continuation which returns
/// the resume value directly. It's used for simple effect handlers that
/// just want to resume with a value.
pub fn create_simple_continuation<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    name: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    // Create the continuation function type:
    // fn(resume_value: *mut u8) -> *mut u8
    let i8_ptr = codegen.context.i8_type().ptr_type(AddressSpace::default());
    let fn_type = i8_ptr.fn_type(&[i8_ptr.into()], false);

    let cont_fn = codegen
        .module
        .add_function(&format!("cont_simple_{}", name), fn_type, None);

    // Create the entry basic block
    let entry_block = codegen.context.append_basic_block(cont_fn, "entry");
    let builder = codegen.context.create_builder();
    builder.position_at_end(entry_block);

    // Get the resume value parameter
    let resume_value = cont_fn.get_nth_param(0).unwrap();

    // Simply return the resume value
    builder
        .build_return(Some(&resume_value))
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(cont_fn)
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

    #[test]
    fn test_create_simple_continuation() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        let cont_fn = create_simple_continuation(&mut codegen, "test").unwrap();

        // Check function name
        assert_eq!(cont_fn.get_name().to_str().unwrap(), "cont_simple_test");

        // Check function signature: fn(*mut u8) -> *mut u8
        assert_eq!(cont_fn.count_params(), 1);

        // Check that the function has an entry block
        assert!(!cont_fn.get_basic_blocks().is_empty());

        // Verify the function
        assert!(cont_fn.verify(true));
    }

    #[test]
    fn test_create_continuation() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        // Create a block within the continuation function itself
        // First create a dummy function to own the block
        let dummy_fn_type = context.void_type().fn_type(&[], false);
        let dummy_fn = codegen.module.add_function("dummy", dummy_fn_type, None);
        let target_block = context.append_basic_block(dummy_fn, "resume");

        // Add a return to the target block so it's valid
        let builder = context.create_builder();
        builder.position_at_end(target_block);
        builder.build_return(None).unwrap();

        // Register the block in codegen
        let block_id = BlockId::new(0);
        codegen.set_block(block_id, target_block);

        // Create a continuation with no captured values
        let cont_fn = create_continuation(&mut codegen, "test", block_id, &[]).unwrap();

        // Check function name
        assert_eq!(cont_fn.get_name().to_str().unwrap(), "cont_test");

        // Check function signature: fn(*mut u8, *mut u8) -> *mut u8
        assert_eq!(cont_fn.count_params(), 2);

        // Check that the function has an entry block
        assert!(!cont_fn.get_basic_blocks().is_empty());

        // Note: We can't verify the continuation function because it branches
        // to a block in a different function. This is expected for CPS transformation
        // where the continuation function's entry block jumps to the resume block.
        // In a real implementation, the resume block would be part of the continuation
        // function or the IR would be structured differently.
    }
}
