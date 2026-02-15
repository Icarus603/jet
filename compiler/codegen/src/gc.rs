//! GC Integration
//!
//! This module provides LLVM code generation support for the Jet garbage collector.
//! It includes:
//! - GC allocation hooks that call into the runtime
//! - Stack map generation for GC root tracking
//! - Write barriers for generational GC
//!
//! The Jet GC is an Immix-style collector with:
//! - Nursery space for new objects (fast bump allocation)
//! - Mature space for long-lived objects
//! - Line-granularity allocation within blocks
//! - Conservative stack scanning with precise heap tracking

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::types::StructType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use jet_ir::Ty;

/// Information about a GC allocation site.
#[derive(Debug, Clone)]
pub struct AllocationSite {
    /// The size of the allocation in bytes.
    pub size: u64,
    /// The alignment requirement.
    pub align: u64,
    /// The type ID for the object.
    pub type_id: u32,
    /// The source location (for debugging).
    pub location: Option<String>,
}

/// Creates a GC allocation hook that calls into the runtime.
///
/// This generates a call to `jet_rt_gc_alloc` which allocates memory
/// in the nursery space using bump allocation.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `site` - Information about the allocation site
///
/// # Returns
///
/// Returns a pointer to the allocated memory.
pub fn emit_gc_alloc<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    site: AllocationSite,
) -> CodegenResult<PointerValue<'ctx>> {
    let alloc_fn = get_or_create_alloc_fn(codegen)?;

    // Build arguments: size, align, type_id
    let size_val = codegen.context.i64_type().const_int(site.size, false);
    let align_val = codegen.context.i64_type().const_int(site.align, false);
    let type_id_val = codegen
        .context
        .i32_type()
        .const_int(site.type_id as u64, false);

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

/// Creates a GC allocation for an object with a specific type.
///
/// This is a convenience wrapper that calculates size and alignment from the type.
pub fn emit_gc_alloc_typed<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    ty: &Ty,
    type_id: u32,
) -> CodegenResult<PointerValue<'ctx>> {
    let size = calculate_type_size(ty)?;
    let align = calculate_type_align(ty)?;

    emit_gc_alloc(
        codegen,
        AllocationSite {
            size,
            align,
            type_id,
            location: None,
        },
    )
}

/// Emits a write barrier for generational GC.
///
/// Write barriers are needed when storing a pointer to a young object
/// into an old object. This ensures the GC can track inter-generational
/// pointers during nursery collections.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `object` - The object being written to (the old object)
/// * `field_offset` - The offset of the field being written
/// * `value` - The value being stored (may be a young pointer)
pub fn emit_write_barrier<'ctx>(
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

/// Declares a GC root on the stack.
///
/// This creates a stack slot that will be scanned by the GC.
/// The GC needs to know about all pointers that are live on the stack.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `value` - The pointer value to root
/// * `name` - A name for the root (for debugging)
///
/// # Returns
///
/// Returns the stack slot pointer (can be used with `load`/`store`).
pub fn emit_gc_root<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    value: PointerValue<'ctx>,
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

    // Emit stack map entry for this root
    // In a full implementation, this would emit metadata for the GC
    emit_stack_map_entry(codegen, slot, name)?;

    Ok(slot)
}

/// Updates a GC root with a new value.
pub fn update_gc_root<'ctx>(
    codegen: &CodeGen<'ctx>,
    root_slot: PointerValue<'ctx>,
    new_value: PointerValue<'ctx>,
) -> CodegenResult<()> {
    codegen
        .builder
        .build_store(root_slot, new_value)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(())
}

/// Emits a stack map entry for GC root tracking.
///
/// Stack maps tell the GC where to find live pointers on the stack
/// at each potential GC point (allocation sites, function calls).
fn emit_stack_map_entry<'ctx>(
    _codegen: &CodeGen<'ctx>,
    _root_slot: PointerValue<'ctx>,
    _name: &str,
) -> CodegenResult<()> {
    // In a full implementation, this would emit LLVM stackmap intrinsic calls
    // or generate metadata that the runtime can use
    // For now, we rely on conservative stack scanning
    Ok(())
}

/// Emits a safepoint poll.
///
/// Safepoints are locations where the GC can safely pause mutator threads.
/// They are typically inserted at loop back-edges and function entry points.
pub fn emit_safepoint_poll<'ctx>(codegen: &CodeGen<'ctx>) -> CodegenResult<()> {
    // Get or create the safepoint poll function
    let poll_fn = get_or_create_safepoint_fn(codegen)?;

    codegen
        .builder
        .build_call(poll_fn, &[], "safepoint")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(())
}

/// Gets or creates the GC allocation function.
fn get_or_create_alloc_fn<'ctx>(codegen: &CodeGen<'ctx>) -> CodegenResult<FunctionValue<'ctx>> {
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

/// Gets or creates the write barrier function.
fn get_or_create_write_barrier_fn<'ctx>(
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

/// Gets or creates the safepoint poll function.
fn get_or_create_safepoint_fn<'ctx>(codegen: &CodeGen<'ctx>) -> CodegenResult<FunctionValue<'ctx>> {
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

/// Calculates the size of a type in bytes.
pub fn calculate_type_size(ty: &Ty) -> CodegenResult<u64> {
    match ty {
        Ty::Void => Ok(0),
        Ty::Int(bits) => Ok((*bits as u64).div_ceil(8)),
        Ty::Float(32) => Ok(4),
        Ty::Float(64) => Ok(8),
        Ty::Float(bits) => Ok((*bits as u64).div_ceil(8)),
        Ty::Bool => Ok(1),
        Ty::Ptr(_) => Ok(8), // 64-bit pointer
        Ty::Struct(fields) => {
            let mut size = 0u64;
            for field in fields {
                let field_size = calculate_type_size(field)?;
                let field_align = calculate_type_align(field)?;
                // Align current size
                size = size.next_multiple_of(field_align);
                size += field_size;
            }
            Ok(size)
        }
        Ty::Array(elem, count) => {
            let elem_size = calculate_type_size(elem)?;
            Ok(elem_size * (*count as u64))
        }
        Ty::Function(_, _) => Ok(8), // Function pointer
        Ty::Generic(_, _) => Ok(8),
        Ty::Named(_) => Err(CodegenError::unsupported_type(
            "cannot calculate size of unresolved named type",
        )),
    }
}

/// Calculates the alignment of a type in bytes.
pub fn calculate_type_align(ty: &Ty) -> CodegenResult<u64> {
    match ty {
        Ty::Void => Ok(1),
        Ty::Int(bits) => {
            let size = (*bits as u64).div_ceil(8);
            Ok(size.next_power_of_two().min(8))
        }
        Ty::Float(32) => Ok(4),
        Ty::Float(64) => Ok(8),
        Ty::Float(bits) => {
            let size = (*bits as u64).div_ceil(8);
            Ok(size.next_power_of_two().min(8))
        }
        Ty::Bool => Ok(1),
        Ty::Ptr(_) => Ok(8),
        Ty::Struct(fields) => {
            // Alignment is max of field alignments
            let mut max_align = 1u64;
            for field in fields {
                let field_align = calculate_type_align(field)?;
                max_align = max_align.max(field_align);
            }
            Ok(max_align)
        }
        Ty::Array(elem, _) => calculate_type_align(elem),
        Ty::Function(_, _) => Ok(8),
        Ty::Generic(_, _) => Ok(8),
        Ty::Named(_) => Err(CodegenError::unsupported_type(
            "cannot calculate alignment of unresolved named type",
        )),
    }
}

/// Creates a struct type for GC object headers.
///
/// The object header contains metadata used by the GC:
/// - Object size
/// - Type ID (for precise scanning)
/// - GC flags (mark bits, generation, etc.)
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

/// Emits initialization of an object header.
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
    fn test_gc_alloc_fn_creation() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let func = get_or_create_alloc_fn(&codegen).unwrap();
        assert_eq!(func.get_name().to_str().unwrap(), "jet_rt_gc_alloc");
        assert_eq!(func.count_params(), 3);
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
    fn test_calculate_type_size() {
        assert_eq!(calculate_type_size(&Ty::I32).unwrap(), 4);
        assert_eq!(calculate_type_size(&Ty::I64).unwrap(), 8);
        assert_eq!(calculate_type_size(&Ty::Bool).unwrap(), 1);
        assert_eq!(calculate_type_size(&Ty::Ptr(Box::new(Ty::I32))).unwrap(), 8);

        // Struct { i32, i64 } = 4 + 4 (padding) + 8 = 16
        let struct_ty = Ty::Struct(vec![Ty::I32, Ty::I64]);
        assert_eq!(calculate_type_size(&struct_ty).unwrap(), 16);
    }

    #[test]
    fn test_calculate_type_align() {
        assert_eq!(calculate_type_align(&Ty::I32).unwrap(), 4);
        assert_eq!(calculate_type_align(&Ty::I64).unwrap(), 8);
        assert_eq!(calculate_type_align(&Ty::Bool).unwrap(), 1);

        // Struct alignment is max of field alignments
        let struct_ty = Ty::Struct(vec![Ty::I32, Ty::I64]);
        assert_eq!(calculate_type_align(&struct_ty).unwrap(), 8);
    }

    #[test]
    fn test_object_header_type() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let header_type = create_object_header_type(&codegen);
        assert_eq!(header_type.count_fields(), 2);
    }
}
