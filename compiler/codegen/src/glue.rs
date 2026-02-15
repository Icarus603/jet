//! Runtime Glue Code
//!
//! This module provides the interface between generated LLVM code and the Jet runtime.
//! It handles:
//! - GC root registration for local variables
//! - Function prologue/epilogue with stack management
//! - Safepoint insertion for GC
//! - Runtime function calls (allocation, task spawning, etc.)
//! - Type marshaling for FFI

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::types::StructType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;

/// Information about a GC root.
#[derive(Debug, Clone)]
pub struct GcRoot {
    /// The stack slot where the root is stored
    pub slot: PointerValue<'static>,
    /// The type ID of the object
    pub type_id: u32,
    /// A name for debugging
    pub name: String,
}

/// Manages GC roots for a function.
#[derive(Debug, Default)]
pub struct GcRootSet {
    /// The roots in this function
    roots: Vec<GcRoot>,
}

impl GcRootSet {
    /// Creates a new empty root set.
    pub fn new() -> Self {
        Self { roots: Vec::new() }
    }

    /// Adds a GC root to the set.
    pub fn add_root(&mut self, slot: PointerValue<'static>, type_id: u32, name: impl Into<String>) {
        self.roots.push(GcRoot {
            slot,
            type_id,
            name: name.into(),
        });
    }

    /// Returns the number of roots.
    pub fn len(&self) -> usize {
        self.roots.len()
    }

    /// Returns true if there are no roots.
    pub fn is_empty(&self) -> bool {
        self.roots.is_empty()
    }
}

/// Emits function prologue with GC root registration.
///
/// This should be called at the beginning of each function that may allocate
/// or call other functions. It sets up the stack frame and registers GC roots.
pub fn emit_function_prologue<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    gc_roots: &GcRootSet,
) -> CodegenResult<()> {
    // If we have GC roots, register them
    if !gc_roots.is_empty() {
        // Create the root chain link
        emit_root_chain_prologue(codegen, gc_roots)?;
    }

    // Emit safepoint poll at function entry
    emit_safepoint_poll(codegen)?;

    Ok(())
}

/// Emits function epilogue with GC root cleanup.
///
/// This should be called at each function exit point.
pub fn emit_function_epilogue<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    gc_roots: &GcRootSet,
) -> CodegenResult<()> {
    // If we have GC roots, unregister them
    if !gc_roots.is_empty() {
        emit_root_chain_epilogue(codegen, gc_roots)?;
    }

    Ok(())
}

/// Emits the prologue for the GC root chain.
fn emit_root_chain_prologue<'ctx>(
    _codegen: &mut CodeGen<'ctx>,
    _gc_roots: &GcRootSet,
) -> CodegenResult<()> {
    // In a full implementation, this would:
    // 1. Save the previous root chain pointer
    // 2. Create a new root chain entry for this function
    // 3. Link it to the previous chain
    // 4. Store the new chain pointer in thread-local storage

    // For now, this is a simplified version
    // The actual implementation would need to coordinate with the runtime

    Ok(())
}

/// Emits the epilogue for the GC root chain.
fn emit_root_chain_epilogue<'ctx>(
    _codegen: &mut CodeGen<'ctx>,
    _gc_roots: &GcRootSet,
) -> CodegenResult<()> {
    // In a full implementation, this would:
    // 1. Restore the previous root chain pointer
    // 2. Unregister all roots from this function

    Ok(())
}

/// Creates a GC root on the stack for a local variable.
///
/// This creates a stack slot that will be scanned by the GC.
/// The root must remain live until the corresponding `unregister_gc_root` call.
pub fn register_gc_root<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    value: PointerValue<'ctx>,
    _type_id: u32,
    name: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    // Create a stack slot for the root
    let i8_ptr = codegen.context.i8_type().ptr_type(AddressSpace::default());
    let slot = codegen
        .builder
        .build_alloca(i8_ptr, &format!("gc_root_{}", name))
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Store the value
    codegen
        .builder
        .build_store(slot, value)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // In a full implementation, we would also emit metadata for the stack map

    Ok(slot)
}

/// Updates a GC root with a new value.
pub fn update_gc_root<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    root_slot: PointerValue<'ctx>,
    new_value: PointerValue<'ctx>,
) -> CodegenResult<()> {
    codegen
        .builder
        .build_store(root_slot, new_value)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(())
}

/// Emits a safepoint poll.
///
/// Safepoints are locations where the GC can safely pause mutator threads.
/// They are typically inserted at:
/// - Function entry points
/// - Loop back-edges
/// - Before/after function calls
pub fn emit_safepoint_poll<'ctx>(codegen: &mut CodeGen<'ctx>) -> CodegenResult<()> {
    let poll_fn = get_or_create_safepoint_fn(codegen)?;

    codegen
        .builder
        .build_call(poll_fn, &[], "safepoint")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(())
}

/// Emits a call to the GC allocation function.
///
/// This allocates memory in the GC heap. The returned pointer is managed
/// by the GC and will be freed when no longer reachable.
pub fn emit_gc_alloc_call<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    size: u64,
    align: u64,
    type_id: u32,
) -> CodegenResult<PointerValue<'ctx>> {
    let alloc_fn = get_or_create_alloc_fn(codegen)?;

    let size_val = codegen.context.i64_type().const_int(size, false);
    let align_val = codegen.context.i64_type().const_int(align, false);
    let type_id_val = codegen.context.i32_type().const_int(type_id as u64, false);

    let args: Vec<BasicMetadataValueEnum<'ctx>> =
        vec![size_val.into(), align_val.into(), type_id_val.into()];

    let result = codegen
        .builder
        .build_call(alloc_fn, &args, "gc_alloc")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(result
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::instruction_error("gc_alloc returned void"))?
        .into_pointer_value())
}

/// Emits a call to spawn a new task.
///
/// This creates a new task on the scheduler.
pub fn emit_task_spawn<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    func: FunctionValue<'ctx>,
    arg: Option<BasicValueEnum<'ctx>>,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let spawn_fn = get_or_create_spawn_fn(codegen)?;

    // Get function pointer as i8*
    let func_ptr = codegen
        .builder
        .build_pointer_cast(
            func.as_global_value().as_pointer_value(),
            codegen.context.i8_type().ptr_type(AddressSpace::default()),
            "task_func_ptr",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Get argument pointer (or null)
    let arg_ptr = if let Some(arg_val) = arg {
        // Allocate space for the argument
        let arg_type = arg_val.get_type();
        let arg_slot = codegen
            .builder
            .build_alloca(arg_type, "task_arg")
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
        codegen
            .builder
            .build_store(arg_slot, arg_val)
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

        codegen
            .builder
            .build_pointer_cast(
                arg_slot,
                codegen.context.i8_type().ptr_type(AddressSpace::default()),
                "task_arg_ptr",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?
    } else {
        codegen
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .const_null()
    };

    let args: Vec<BasicMetadataValueEnum<'ctx>> = vec![func_ptr.into(), arg_ptr.into()];

    let result = codegen
        .builder
        .build_call(spawn_fn, &args, "task_id")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(result
        .try_as_basic_value()
        .basic()
        .unwrap_or_else(|| codegen.context.i64_type().const_zero().into()))
}

/// Emits a write barrier for generational GC.
///
/// This should be called when storing a pointer to a young object
/// into an old object.
pub fn emit_write_barrier_call<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    object: PointerValue<'ctx>,
    field_offset: u64,
    value: BasicValueEnum<'ctx>,
) -> CodegenResult<()> {
    // Only emit write barrier for pointer stores
    if !value.is_pointer_value() {
        return Ok(());
    }

    let barrier_fn = get_or_create_write_barrier_fn(codegen)?;

    let offset_val = codegen.context.i64_type().const_int(field_offset, false);

    let args: Vec<BasicMetadataValueEnum<'ctx>> =
        vec![object.into(), offset_val.into(), value.into()];

    codegen
        .builder
        .build_call(barrier_fn, &args, "write_barrier")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(())
}

/// Marshals a Jet value to a C-compatible value for FFI calls.
pub fn marshal_for_ffi<'ctx>(
    _codegen: &mut CodeGen<'ctx>,
    value: BasicValueEnum<'ctx>,
    jet_type: &jet_ir::Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    match jet_type {
        // Primitive types pass directly
        jet_ir::Ty::Int(_) | jet_ir::Ty::Float(_) | jet_ir::Ty::Bool => Ok(value),

        // Pointers pass as-is
        jet_ir::Ty::Ptr(_) => Ok(value),

        // Named types (including String) need special handling
        jet_ir::Ty::Named(name) => {
            if name == "String" {
                // For now, strings are passed as i8* pointers
                // A full implementation would need to handle the string layout
                Ok(value)
            } else {
                // Other named types: get the payload pointer from the GC object
                Ok(value)
            }
        }

        // GC objects need to unbox the payload pointer
        jet_ir::Ty::Struct(_) => {
            // Get the payload pointer from the GC object
            // In a full implementation, this would skip the header
            Ok(value)
        }

        // Other types
        _ => Ok(value),
    }
}

/// Marshals a C-compatible value back to a Jet value after FFI calls.
pub fn marshal_from_ffi<'ctx>(
    _codegen: &mut CodeGen<'ctx>,
    value: BasicValueEnum<'ctx>,
    jet_type: &jet_ir::Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    match jet_type {
        // Primitive types pass directly
        jet_ir::Ty::Int(_) | jet_ir::Ty::Float(_) | jet_ir::Ty::Bool => Ok(value),

        // Pointers pass as-is
        jet_ir::Ty::Ptr(_) => Ok(value),

        // Named types (including String) need special handling
        jet_ir::Ty::Named(name) => {
            if name == "String" {
                // For now, return the pointer as-is
                // A full implementation would allocate a Jet string object
                Ok(value)
            } else {
                // Allocate a GC object and store the value
                // For now, return the pointer as-is
                Ok(value)
            }
        }

        // GC objects need to be boxed
        jet_ir::Ty::Struct(_) => {
            // Allocate a GC object and store the value
            // For now, return the pointer as-is
            Ok(value)
        }

        // Other types
        _ => Ok(value),
    }
}

/// Gets or creates the GC allocation function declaration.
pub fn get_or_create_alloc_fn<'ctx>(codegen: &CodeGen<'ctx>) -> CodegenResult<FunctionValue<'ctx>> {
    let name = "jet_rt_gc_alloc";

    if let Some(func) = codegen.module.get_function(name) {
        return Ok(func);
    }

    // Function signature: i64 size, i64 align, i32 type_id -> i8*
    let i8_ptr = codegen.context.i8_type().ptr_type(AddressSpace::default());
    let fn_type = i8_ptr.fn_type(
        &[
            codegen.context.i64_type().into(),
            codegen.context.i64_type().into(),
            codegen.context.i32_type().into(),
        ],
        false,
    );

    let func = codegen.module.add_function(name, fn_type, None);

    // Mark as noinline to ensure the call is visible to the GC
    func.add_attribute(
        AttributeLoc::Function,
        codegen
            .context
            .create_enum_attribute(Attribute::get_named_enum_kind_id("noinline"), 0),
    );

    Ok(func)
}

/// Gets or creates the task spawn function declaration.
pub fn get_or_create_spawn_fn<'ctx>(codegen: &CodeGen<'ctx>) -> CodegenResult<FunctionValue<'ctx>> {
    let name = "jet_rt_sched_spawn";

    if let Some(func) = codegen.module.get_function(name) {
        return Ok(func);
    }

    // Function signature: i8* func, i8* arg -> i64 (task_id)
    let i8_ptr = codegen.context.i8_type().ptr_type(AddressSpace::default());
    let fn_type = codegen
        .context
        .i64_type()
        .fn_type(&[i8_ptr.into(), i8_ptr.into()], false);

    Ok(codegen.module.add_function(name, fn_type, None))
}

/// Gets or creates the write barrier function declaration.
pub fn get_or_create_write_barrier_fn<'ctx>(
    codegen: &CodeGen<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let name = "jet_rt_gc_write_barrier";

    if let Some(func) = codegen.module.get_function(name) {
        return Ok(func);
    }

    // Function signature: i8* object, i64 offset, i8* value -> void
    let i8_ptr = codegen.context.i8_type().ptr_type(AddressSpace::default());
    let fn_type = codegen.context.void_type().fn_type(
        &[
            i8_ptr.into(),
            codegen.context.i64_type().into(),
            i8_ptr.into(),
        ],
        false,
    );

    Ok(codegen.module.add_function(name, fn_type, None))
}

/// Gets or creates the safepoint poll function declaration.
pub fn get_or_create_safepoint_fn<'ctx>(
    codegen: &CodeGen<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let name = "jet_rt_gc_safepoint";

    if let Some(func) = codegen.module.get_function(name) {
        return Ok(func);
    }

    let fn_type = codegen.context.void_type().fn_type(&[], false);

    let func = codegen.module.add_function(name, fn_type, None);

    // Mark as cold (rarely executed)
    func.add_attribute(
        AttributeLoc::Function,
        codegen
            .context
            .create_enum_attribute(Attribute::get_named_enum_kind_id("cold"), 0),
    );

    Ok(func)
}

/// Creates the GC object header type.
pub fn create_object_header_type<'ctx>(codegen: &CodeGen<'ctx>) -> StructType<'ctx> {
    // Header layout:
    // - u32: size (lower 24 bits) + flags (upper 8 bits)
    // - u32: type_id
    // Total: 8 bytes
    codegen.context.struct_type(
        &[
            codegen.context.i32_type().into(), // size_and_flags
            codegen.context.i32_type().into(), // type_id
        ],
        false,
    )
}

/// Emits initialization of a GC object header.
pub fn emit_init_object_header<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    object: PointerValue<'ctx>,
    size: u64,
    type_id: u32,
) -> CodegenResult<()> {
    let header_type = create_object_header_type(codegen);

    // Cast object to header pointer
    let header_ptr = codegen
        .builder
        .build_pointer_cast(
            object,
            header_type.ptr_type(AddressSpace::default()),
            "header_ptr",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Store size_and_flags (size in lower 24 bits, flags in upper 8)
    let size_and_flags = codegen.context.i32_type().const_int(size & 0xFFFFFF, false);
    let size_ptr = codegen
        .builder
        .build_struct_gep(header_type, header_ptr, 0, "size_ptr")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
    codegen
        .builder
        .build_store(size_ptr, size_and_flags)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Store type_id
    let type_id_val = codegen.context.i32_type().const_int(type_id as u64, false);
    let type_id_ptr = codegen
        .builder
        .build_struct_gep(header_type, header_ptr, 1, "type_id_ptr")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
    codegen
        .builder
        .build_store(type_id_ptr, type_id_val)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::context::Context;

    fn create_test_codegen<'ctx>(context: &'ctx Context) -> CodeGen<'ctx> {
        CodeGen::new(context, "test")
    }

    #[test]
    fn test_alloc_fn_creation() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let func = get_or_create_alloc_fn(&codegen).unwrap();
        assert_eq!(func.get_name().to_str().unwrap(), "jet_rt_gc_alloc");
        assert_eq!(func.count_params(), 3);
    }

    #[test]
    fn test_spawn_fn_creation() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let func = get_or_create_spawn_fn(&codegen).unwrap();
        assert_eq!(func.get_name().to_str().unwrap(), "jet_rt_sched_spawn");
        assert_eq!(func.count_params(), 2);
    }

    #[test]
    fn test_write_barrier_fn_creation() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let func = get_or_create_write_barrier_fn(&codegen).unwrap();
        assert_eq!(func.get_name().to_str().unwrap(), "jet_rt_gc_write_barrier");
        assert_eq!(func.count_params(), 3);
    }

    #[test]
    fn test_safepoint_fn_creation() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let func = get_or_create_safepoint_fn(&codegen).unwrap();
        assert_eq!(func.get_name().to_str().unwrap(), "jet_rt_gc_safepoint");
        assert_eq!(func.count_params(), 0);
    }

    #[test]
    fn test_object_header_type() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let header_type = create_object_header_type(&codegen);
        assert_eq!(header_type.count_fields(), 2);
    }

    #[test]
    fn test_gc_root_set() {
        let roots = GcRootSet::new();
        assert!(roots.is_empty());
        assert_eq!(roots.len(), 0);
    }
}
