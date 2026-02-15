//! Async/Await Code Generation
//!
//! This module provides LLVM code generation for Jet's async/await.
//!
//! # Async Function Transformation
//!
//! Async functions are transformed into state machines:
//!
//! ```text
//! Before:
//! async fn foo(x: i32) -> i32:
//!     let y = await bar(x)
//!     return y + 1
//!
//! After (state machine):
//! fn foo(x: i32, state: *mut FooState) -> Poll<i32>:
//!     switch state->stage:
//!         0: goto start
//!         1: goto after_await_0
//!
//!     start:
//!         state->x = x
//!         state->stage = 1
//!         state->future_0 = bar(x)
//!         return Poll::Pending
//!
//!     after_await_0:
//!         y = state->future_0.result
//!         return Poll::Ready(y + 1)
//! ```
//!
//! # State Machine Layout
//!
//! ```text
//! State:
//!   +------------------+
//!   | stage            |  u32 - current execution stage
//!   +------------------+
//!   | waker            |  *mut Waker - callback when ready
//!   +------------------+
//!   | local_0          |  captured locals
//!   +------------------+
//!   | local_1          |
//!   +------------------+
//!   | ...              |
//!   +------------------+
//!   | future_0         |  nested futures being awaited
//!   +------------------+
//!   | future_1         |
//!   +------------------+
//! ```

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use crate::gc::{emit_gc_alloc, AllocationSite};
use crate::types::TypeMapping;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use jet_ir::{Function, Ty, ValueId};
use std::collections::HashMap;

/// The stages of an async state machine.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AsyncStage {
    /// Initial stage - start execution.
    Start = 0,
    /// Stage after first await.
    AfterAwait0 = 1,
    /// Stage after second await.
    AfterAwait1 = 2,
    /// Stage after third await.
    AfterAwait2 = 3,
}

impl AsyncStage {
    pub fn as_u32(self) -> u32 {
        self as u32
    }
}

/// Information about an async function being compiled.
#[derive(Debug, Clone)]
pub struct AsyncFunctionInfo {
    /// The original function.
    pub function: Function,
    /// The number of await points.
    pub await_count: usize,
    /// The locals that need to be preserved across await points.
    pub captured_locals: Vec<(ValueId, Ty)>,
    /// The futures being awaited at each point.
    pub awaited_futures: Vec<ValueId>,
}

/// Information about the state struct for an async function.
#[derive(Debug, Clone)]
pub struct StateStructInfo<'ctx> {
    /// The LLVM struct type.
    pub struct_type: StructType<'ctx>,
    /// Mapping from local variable to field index.
    pub local_indices: HashMap<ValueId, usize>,
    /// Mapping from await point to future field index.
    pub future_indices: HashMap<usize, usize>,
    /// Index of the stage field.
    pub stage_index: usize,
    /// Index of the waker field.
    pub waker_index: usize,
}

/// Compiles an async function to a state machine.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `info` - Information about the async function
///
/// # Returns
///
/// Returns the compiled state machine function.
pub fn compile_async_function<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    info: &AsyncFunctionInfo,
) -> CodegenResult<FunctionValue<'ctx>> {
    // Create the state struct type
    let state_info = create_state_struct(codegen, info)?;

    // Create the poll function
    let poll_fn = create_poll_function(codegen, info, &state_info)?;

    // Create the future constructor
    let _constructor_fn = create_future_constructor(codegen, info, &state_info)?;

    // Mark as async
    poll_fn.add_attribute(
        AttributeLoc::Function,
        codegen
            .context
            .create_enum_attribute(Attribute::get_named_enum_kind_id("noinline"), 0),
    );

    Ok(poll_fn)
}

/// Creates the state struct type for an async function.
fn create_state_struct<'ctx>(
    codegen: &CodeGen<'ctx>,
    info: &AsyncFunctionInfo,
) -> CodegenResult<StateStructInfo<'ctx>> {
    let mut field_types: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
    let mut local_indices = HashMap::new();
    let mut future_indices = HashMap::new();

    // Stage field (u32)
    let stage_index = 0;
    field_types.push(codegen.context.i32_type().into());

    // Waker field (*mut Waker)
    let waker_index = 1;
    let waker_ptr_type = codegen.context.i8_type().ptr_type(AddressSpace::default());
    field_types.push(waker_ptr_type.into());

    // Captured locals
    for (i, (var_id, ty)) in info.captured_locals.iter().enumerate() {
        let llvm_ty = TypeMapping::jet_to_llvm(codegen, ty)?;
        local_indices.insert(*var_id, 2 + i);
        field_types.push(llvm_ty.into());
    }

    // Awaited futures (stored as *mut dyn Future)
    let future_start = 2 + info.captured_locals.len();
    for (i, _) in info.awaited_futures.iter().enumerate() {
        let future_ptr = codegen.context.i8_type().ptr_type(AddressSpace::default());
        future_indices.insert(i, future_start + i);
        field_types.push(future_ptr.into());
    }

    // Convert BasicMetadataTypeEnum to BasicTypeEnum for struct creation
    // BasicMetadataTypeEnum is an enum wrapper around BasicTypeEnum, so we need to extract it
    let field_types: Vec<BasicTypeEnum<'ctx>> = field_types
        .into_iter()
        .map(|t| match t {
            BasicMetadataTypeEnum::ArrayType(ty) => ty.into(),
            BasicMetadataTypeEnum::FloatType(ty) => ty.into(),
            BasicMetadataTypeEnum::IntType(ty) => ty.into(),
            BasicMetadataTypeEnum::PointerType(ty) => ty.into(),
            BasicMetadataTypeEnum::StructType(ty) => ty.into(),
            BasicMetadataTypeEnum::VectorType(ty) => ty.into(),
            _ => unreachable!(),
        })
        .collect();

    let struct_type = codegen.context.struct_type(&field_types, false);

    Ok(StateStructInfo {
        struct_type,
        local_indices,
        future_indices,
        stage_index,
        waker_index,
    })
}

/// Creates the poll function for an async state machine.
fn create_poll_function<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    info: &AsyncFunctionInfo,
    state_info: &StateStructInfo<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let fn_name = format!("{}_poll", info.function.name);

    // Check if function already exists
    if let Some(func) = codegen.module.get_function(&fn_name) {
        return Ok(func);
    }

    // Function signature: fn(state: *mut State) -> Poll<ReturnType>
    let state_ptr_type = state_info.struct_type.ptr_type(AddressSpace::default());
    let ret_type = get_poll_type(codegen, &info.function.return_ty)?;

    let fn_type = ret_type.fn_type(&[state_ptr_type.into()], false);
    let func = codegen.module.add_function(&fn_name, fn_type, None);

    // Create entry block
    let entry_block = codegen.context.append_basic_block(func, "entry");
    codegen.builder.position_at_end(entry_block);

    // Get state parameter
    let state_param = func.get_nth_param(0).unwrap().into_pointer_value();

    // Load current stage
    let stage_ptr = codegen
        .builder
        .build_struct_gep(
            state_info.struct_type,
            state_param,
            state_info.stage_index as u32,
            "stage_ptr",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    let stage_val = codegen
        .builder
        .build_load(codegen.context.i32_type(), stage_ptr, "stage")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Create switch for stages
    let default_block = codegen.context.append_basic_block(func, "invalid_stage");
    let mut stage_blocks = Vec::new();

    // Create blocks for each stage
    for i in 0..=info.await_count {
        let stage_block = codegen
            .context
            .append_basic_block(func, &format!("stage_{}", i));
        stage_blocks.push((i as u64, stage_block));
    }

    // Build switch
    let llvm_cases: Vec<_> = stage_blocks
        .iter()
        .map(|(i, block)| {
            let val = codegen.context.i32_type().const_int(*i, false);
            (val, *block)
        })
        .collect();

    codegen
        .builder
        .build_switch(stage_val.into_int_value(), default_block, &llvm_cases)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Build default block (invalid stage)
    codegen.builder.position_at_end(default_block);
    codegen
        .builder
        .build_unreachable()
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // For now, just create placeholder blocks for each stage
    // A full implementation would compile the actual body
    for (i, block) in stage_blocks {
        codegen.builder.position_at_end(block);

        // Placeholder: return Pending for all stages except last
        if (i as usize) < info.await_count {
            // Return Poll::Pending
            let pending_val = create_pending_value(codegen, &info.function.return_ty)?;
            codegen
                .builder
                .build_return(Some(&pending_val))
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
        } else {
            // Return Poll::Ready(default)
            let ready_val = create_ready_value(codegen, &info.function.return_ty)?;
            codegen
                .builder
                .build_return(Some(&ready_val))
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
        }
    }

    Ok(func)
}

/// Creates the future constructor function.
fn create_future_constructor<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    info: &AsyncFunctionInfo,
    state_info: &StateStructInfo<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let fn_name = format!("{}_new", info.function.name);

    // Check if function already exists
    if let Some(func) = codegen.module.get_function(&fn_name) {
        return Ok(func);
    }

    // Function signature: fn(params...) -> *mut State
    let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = info
        .function
        .params
        .iter()
        .map(|p| TypeMapping::jet_to_llvm(codegen, &p.ty).map(|t| t.into()))
        .collect::<CodegenResult<Vec<_>>>()?;

    let state_ptr_type = state_info.struct_type.ptr_type(AddressSpace::default());
    let fn_type = state_ptr_type.fn_type(&param_types, false);
    let func = codegen.module.add_function(&fn_name, fn_type, None);

    // Create entry block
    let entry_block = codegen.context.append_basic_block(func, "entry");
    codegen.builder.position_at_end(entry_block);

    // Allocate state on GC heap
    let state_size = state_info
        .struct_type
        .size_of()
        .ok_or_else(|| CodegenError::instruction_error("could not compute state size"))?;

    // Use a default alignment of 8 for the state struct
    let state_align = 8u64;

    let state_ptr = emit_gc_alloc(
        codegen,
        AllocationSite {
            size: state_size.get_zero_extended_constant().unwrap_or(64),
            align: state_align,
            type_id: 0xFFFF, // Special type ID for async states
            location: Some(format!("async_state_{}", info.function.name)),
        },
    )?;

    // Cast to state pointer type
    let typed_state_ptr = codegen
        .builder
        .build_pointer_cast(state_ptr, state_ptr_type, "state")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Initialize stage to 0
    let stage_ptr = codegen
        .builder
        .build_struct_gep(
            state_info.struct_type,
            typed_state_ptr,
            state_info.stage_index as u32,
            "stage_ptr",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    let zero = codegen.context.i32_type().const_int(0, false);
    codegen
        .builder
        .build_store(stage_ptr, zero)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Initialize waker to null
    let waker_ptr = codegen
        .builder
        .build_struct_gep(
            state_info.struct_type,
            typed_state_ptr,
            state_info.waker_index as u32,
            "waker_ptr",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    let null_waker = codegen
        .context
        .i8_type()
        .ptr_type(AddressSpace::default())
        .const_null();
    codegen
        .builder
        .build_store(waker_ptr, null_waker)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Store parameters in state
    for (i, param) in info.function.params.iter().enumerate() {
        if let Some(&field_index) = state_info.local_indices.get(&param.value) {
            let param_val = func.get_nth_param(i as u32).unwrap();
            let field_ptr = codegen
                .builder
                .build_struct_gep(
                    state_info.struct_type,
                    typed_state_ptr,
                    field_index as u32,
                    &format!("param_{}_ptr", i),
                )
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

            codegen
                .builder
                .build_store(field_ptr, param_val)
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
        }
    }

    // Return state pointer
    codegen
        .builder
        .build_return(Some(&typed_state_ptr))
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(func)
}

/// Gets the LLVM type for Poll<T>.
fn get_poll_type<'ctx>(
    codegen: &CodeGen<'ctx>,
    inner_ty: &Ty,
) -> CodegenResult<BasicTypeEnum<'ctx>> {
    // Poll<T> is represented as a struct { tag: i32, value: T }
    // tag: 0 = Pending, 1 = Ready
    let inner_llvm_ty = if inner_ty.is_void() {
        codegen.context.i8_type().into()
    } else {
        TypeMapping::jet_to_llvm(codegen, inner_ty)?
    };

    Ok(codegen
        .context
        .struct_type(&[codegen.context.i32_type().into(), inner_llvm_ty], false)
        .into())
}

/// Creates a Poll::Pending value.
fn create_pending_value<'ctx>(
    codegen: &CodeGen<'ctx>,
    inner_ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let poll_ty = get_poll_type(codegen, inner_ty)?;
    let mut val = poll_ty.into_struct_type().const_zero();

    // Set tag to 0 (Pending)
    let tag = codegen.context.i32_type().const_int(0, false);
    val = codegen
        .builder
        .build_insert_value(val, tag, 0, "pending_tag")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
        .into_struct_value();

    Ok(val.into())
}

/// Creates a Poll::Ready(value) value.
fn create_ready_value<'ctx>(
    codegen: &CodeGen<'ctx>,
    inner_ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let poll_ty = get_poll_type(codegen, inner_ty)?;
    let mut val = poll_ty.into_struct_type().const_zero();

    // Set tag to 1 (Ready)
    let tag = codegen.context.i32_type().const_int(1, false);
    val = codegen
        .builder
        .build_insert_value(val, tag, 0, "ready_tag")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
        .into_struct_value();

    // Value is zero-initialized for now
    Ok(val.into())
}

/// Compiles an await expression.
///
/// This generates code that:
/// 1. Stores the future in the state
/// 2. Updates the stage number
/// 3. Returns Poll::Pending
/// 4. On resume, loads the result from the future
pub fn compile_await<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    future: BasicValueEnum<'ctx>,
    state_ptr: PointerValue<'ctx>,
    state_info: &StateStructInfo<'ctx>,
    await_index: usize,
    return_type: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    // Store the future in the state
    if let Some(&future_index) = state_info.future_indices.get(&await_index) {
        let future_ptr = codegen
            .builder
            .build_struct_gep(
                state_info.struct_type,
                state_ptr,
                future_index as u32,
                "future_ptr",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

        let future_cast = codegen
            .builder
            .build_pointer_cast(
                future.into_pointer_value(),
                codegen.context.i8_type().ptr_type(AddressSpace::default()),
                "future_cast",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

        codegen
            .builder
            .build_store(future_ptr, future_cast)
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
    }

    // Update stage number
    let stage_ptr = codegen
        .builder
        .build_struct_gep(
            state_info.struct_type,
            state_ptr,
            state_info.stage_index as u32,
            "stage_ptr",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    let next_stage = codegen
        .context
        .i32_type()
        .const_int((await_index + 1) as u64, false);
    codegen
        .builder
        .build_store(stage_ptr, next_stage)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Return Poll::Pending
    create_pending_value(codegen, return_type)
}

/// Compiles a spawn expression for structured concurrency.
pub fn compile_spawn<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    future: BasicValueEnum<'ctx>,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    // Get or create the spawn runtime function
    let spawn_fn = get_or_create_spawn_fn(codegen)?;

    // Cast future to i8*
    let future_cast = codegen
        .builder
        .build_pointer_cast(
            future.into_pointer_value(),
            codegen.context.i8_type().ptr_type(AddressSpace::default()),
            "spawn_future",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Call spawn
    let result = codegen
        .builder
        .build_call(spawn_fn, &[future_cast.into()], "spawn_result")
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

    // Create the function type: fn(future: *mut u8) -> task_id: u64
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

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::context::Context;

    fn create_test_codegen<'ctx>(context: &'ctx Context) -> CodeGen<'ctx> {
        CodeGen::new(context, "test")
    }

    #[test]
    fn test_async_stage() {
        assert_eq!(AsyncStage::Start.as_u32(), 0);
        assert_eq!(AsyncStage::AfterAwait0.as_u32(), 1);
        assert_eq!(AsyncStage::AfterAwait1.as_u32(), 2);
        assert_eq!(AsyncStage::AfterAwait2.as_u32(), 3);
    }

    #[test]
    fn test_poll_type() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let poll_i32 = get_poll_type(&codegen, &Ty::I32).unwrap();
        assert!(poll_i32.is_struct_type());

        let poll_void = get_poll_type(&codegen, &Ty::Void).unwrap();
        assert!(poll_void.is_struct_type());
    }

    #[test]
    fn test_pending_value() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        // Create a dummy function and block to set builder position
        let fn_type = context.i32_type().fn_type(&[], false);
        let func = codegen.module.add_function("test", fn_type, None);
        let block = context.append_basic_block(func, "entry");
        codegen.builder.position_at_end(block);

        let pending = create_pending_value(&codegen, &Ty::I32).unwrap();
        assert!(pending.is_struct_value());
    }

    #[test]
    fn test_ready_value() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        // Create a dummy function and block to set builder position
        let fn_type = context.i32_type().fn_type(&[], false);
        let func = codegen.module.add_function("test", fn_type, None);
        let block = context.append_basic_block(func, "entry");
        codegen.builder.position_at_end(block);

        let ready = create_ready_value(&codegen, &Ty::I32).unwrap();
        assert!(ready.is_struct_value());
    }

    #[test]
    fn test_spawn_fn_creation() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let func = get_or_create_spawn_fn(&codegen).unwrap();
        assert_eq!(func.get_name().to_str().unwrap(), "jet_rt_spawn");
        assert_eq!(func.count_params(), 1);
    }
}
