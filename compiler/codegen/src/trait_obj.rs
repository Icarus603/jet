//! Trait Object Compilation
//!
//! This module provides LLVM code generation for Jet trait objects.
//!
//! # Trait Object Representation
//!
//! A trait object in Jet is represented as a "fat pointer":
//! - **Data pointer**: Points to the actual object
//! - **VTable pointer**: Points to the virtual method table
//!
//! # VTable Layout
//!
//! The vtable contains function pointers for each method in the trait,
//! plus metadata for the type:
//!
//! ```text
//! VTable:
//!   +------------------+
//!   | type_id          |  u32 - unique type identifier
//!   +------------------+
//!   | type_size        |  u32 - size of the concrete type
//!   +------------------+
//!   | type_align       |  u32 - alignment of the concrete type
//!   +------------------+
//!   | drop_fn          |  fn(*mut u8) - destructor
//!   +------------------+
//!   | method_0         |  first trait method
//!   +------------------+
//!   | method_1         |
//!   +------------------+
//!   | ...              |
//!   +------------------+
//! ```
//!
//! # Trait Object Layout
//!
//! ```text
//! TraitObject:
//!   +------------------+
//!   | data_ptr         |  *mut u8 - points to object
//!   +------------------+
//!   | vtable_ptr       |  *const VTable
//!   +------------------+
//! ```

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, GlobalValue, PointerValue,
};
use inkwell::AddressSpace;
use jet_ir::Ty;

/// Information about a trait for vtable generation.
#[derive(Debug, Clone)]
pub struct TraitInfo {
    /// The name of the trait.
    pub name: String,
    /// The methods in the trait (name, param types, return type).
    pub methods: Vec<(String, Vec<Ty>, Ty)>,
}

/// Information about a concrete type implementing a trait.
#[derive(Debug, Clone)]
pub struct ImplInfo {
    /// The name of the implementing type.
    pub type_name: String,
    /// The type ID for this concrete type.
    pub type_id: u32,
    /// The size of the concrete type.
    pub type_size: u32,
    /// The alignment of the concrete type.
    pub type_align: u32,
    /// The implementing functions for each method.
    pub method_impls: Vec<FunctionValue<'static>>,
    /// The drop function for this type.
    pub drop_fn: Option<FunctionValue<'static>>,
}

/// Creates the LLVM struct type for a trait object (fat pointer).
pub fn trait_object_type<'ctx>(context: &'ctx inkwell::context::Context) -> StructType<'ctx> {
    let data_ptr = context.i8_type().ptr_type(AddressSpace::default());
    let vtable_ptr = context.i8_type().ptr_type(AddressSpace::default());

    context.struct_type(&[data_ptr.into(), vtable_ptr.into()], false)
}

/// Generates a vtable for a trait implementation.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `trait_info` - Information about the trait
/// * `impl_info` - Information about the implementation
///
/// # Returns
///
/// Returns the global vtable value.
pub fn generate_vtable<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    trait_info: &TraitInfo,
    impl_info: &ImplInfo,
) -> CodegenResult<GlobalValue<'ctx>> {
    let vtable_name = format!("vtable.{}.for.{}", trait_info.name, impl_info.type_name);

    // Check if vtable already exists
    if let Some(global) = codegen.module.get_global(&vtable_name) {
        return Ok(global);
    }

    // Create vtable type
    let vtable_ty = vtable_type(codegen, trait_info)?;

    // Create method pointers
    let mut vtable_values: Vec<BasicValueEnum<'ctx>> = vec![
        // type_id
        codegen
            .context
            .i32_type()
            .const_int(impl_info.type_id as u64, false)
            .into(),
        // type_size
        codegen
            .context
            .i32_type()
            .const_int(impl_info.type_size as u64, false)
            .into(),
        // type_align
        codegen
            .context
            .i32_type()
            .const_int(impl_info.type_align as u64, false)
            .into(),
    ];

    // Add drop function pointer (or null)
    let drop_ptr = if let Some(drop_fn) = impl_info.drop_fn {
        drop_fn.as_global_value().as_pointer_value().into()
    } else {
        codegen
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .const_null()
            .into()
    };
    vtable_values.push(drop_ptr);

    // Add method pointers
    for (i, method_impl) in impl_info.method_impls.iter().enumerate() {
        if i >= trait_info.methods.len() {
            return Err(CodegenError::invalid_operand(
                "vtable generation",
                "too many method implementations",
            ));
        }
        let fn_ptr = method_impl.as_global_value().as_pointer_value();
        vtable_values.push(fn_ptr.into());
    }

    // Create global vtable
    let vtable_const = codegen.context.const_struct(&vtable_values, false);
    let global = codegen.module.add_global(vtable_ty, None, &vtable_name);
    global.set_initializer(&vtable_const);
    global.set_linkage(Linkage::Internal);
    global.set_constant(true);

    Ok(global)
}

/// Creates the LLVM struct type for a vtable.
fn vtable_type<'ctx>(
    codegen: &CodeGen<'ctx>,
    trait_info: &TraitInfo,
) -> CodegenResult<StructType<'ctx>> {
    let fn_ptr_type = codegen
        .context
        .i8_type()
        .fn_type(
            &[codegen
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into()],
            false,
        )
        .ptr_type(AddressSpace::default());

    // Fields: type_id, type_size, type_align, drop_fn, method_0, ..., method_n
    let _num_fields = 4 + trait_info.methods.len();
    let mut field_types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![
        codegen.context.i32_type().into(), // type_id
        codegen.context.i32_type().into(), // type_size
        codegen.context.i32_type().into(), // type_align
        fn_ptr_type.into(),                // drop_fn
    ];

    // Add method pointer types
    for _ in &trait_info.methods {
        field_types.push(fn_ptr_type.into());
    }

    // Convert BasicMetadataTypeEnum to BasicTypeEnum for struct creation
    let field_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> = field_types
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

    Ok(codegen.context.struct_type(&field_types, false))
}

/// Creates a trait object from a concrete value.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `data_ptr` - Pointer to the concrete object
/// * `vtable_global` - The global vtable for this type/trait combination
///
/// # Returns
///
/// Returns the trait object (fat pointer).
pub fn create_trait_object<'ctx>(
    codegen: &CodeGen<'ctx>,
    data_ptr: PointerValue<'ctx>,
    vtable_global: GlobalValue<'ctx>,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let trait_obj_ty = trait_object_type(codegen.context);
    let mut trait_obj = trait_obj_ty.const_zero();

    // Store data pointer
    let data_ptr_cast = codegen
        .builder
        .build_pointer_cast(
            data_ptr,
            codegen.context.i8_type().ptr_type(AddressSpace::default()),
            "trait_data_ptr",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    trait_obj = codegen
        .builder
        .build_insert_value(trait_obj, data_ptr_cast, 0, "trait_obj_data")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
        .into_struct_value();

    // Store vtable pointer
    let vtable_ptr = vtable_global.as_pointer_value();
    let vtable_ptr_cast = codegen
        .builder
        .build_pointer_cast(
            vtable_ptr,
            codegen.context.i8_type().ptr_type(AddressSpace::default()),
            "trait_vtable_ptr",
        )
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    trait_obj = codegen
        .builder
        .build_insert_value(trait_obj, vtable_ptr_cast, 1, "trait_obj_vtable")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?
        .into_struct_value();

    Ok(trait_obj.into())
}

/// Extracts the data pointer from a trait object.
pub fn get_trait_data_ptr<'ctx>(
    codegen: &CodeGen<'ctx>,
    trait_obj: BasicValueEnum<'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    let ptr = codegen
        .builder
        .build_extract_value(trait_obj.into_struct_value(), 0, "trait_data")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(ptr.into_pointer_value())
}

/// Extracts the vtable pointer from a trait object.
pub fn get_trait_vtable_ptr<'ctx>(
    codegen: &CodeGen<'ctx>,
    trait_obj: BasicValueEnum<'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    let ptr = codegen
        .builder
        .build_extract_value(trait_obj.into_struct_value(), 1, "trait_vtable")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(ptr.into_pointer_value())
}

/// Calls a method on a trait object (dynamic dispatch).
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `trait_obj` - The trait object
/// * `method_index` - The index of the method in the vtable
/// * `args` - Additional arguments to pass
/// * `return_type` - The expected return type
///
/// # Returns
///
/// Returns the result of the method call.
pub fn call_trait_method<'ctx>(
    codegen: &CodeGen<'ctx>,
    trait_obj: BasicValueEnum<'ctx>,
    method_index: usize,
    args: &[BasicValueEnum<'ctx>],
    return_type: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    // Extract vtable pointer
    let vtable_ptr = get_trait_vtable_ptr(codegen, trait_obj)?;

    // Extract data pointer
    let data_ptr = get_trait_data_ptr(codegen, trait_obj)?;

    // Calculate method pointer offset in vtable
    // Skip: type_id (4 bytes), type_size (4 bytes), type_align (4 bytes), drop_fn (8 bytes)
    let method_offset = 4 + 4 + 4 + 8 + (method_index as u64) * 8;

    // Load method pointer from vtable
    let method_ptr_ptr = unsafe {
        codegen
            .builder
            .build_gep(
                codegen.context.i8_type(),
                vtable_ptr,
                &[codegen.context.i64_type().const_int(method_offset, false)],
                "method_ptr_addr",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?
    };

    let fn_ptr_type = codegen
        .context
        .i8_type()
        .fn_type(
            &[codegen
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into()],
            false,
        )
        .ptr_type(AddressSpace::default());

    let method_ptr = codegen
        .builder
        .build_pointer_cast(method_ptr_ptr, fn_ptr_type, "method_ptr")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Build argument list: self (data ptr) first, then other args
    let mut call_args: Vec<BasicMetadataValueEnum<'ctx>> = vec![data_ptr.into()];
    for arg in args {
        call_args.push((*arg).into());
    }

    // Create function type for call
    let fn_type = if return_type.is_void() {
        codegen.context.void_type().fn_type(
            &[codegen
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into()],
            false,
        )
    } else {
        codegen
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
            )
    };

    // Call the method
    let result = codegen
        .builder
        .build_indirect_call(fn_type, method_ptr, &call_args, "trait_call")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    if return_type.is_void() {
        Ok(codegen.context.i32_type().const_zero().into())
    } else {
        result
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::instruction_error("trait method returned void"))
    }
}

/// Generates a drop call for a trait object.
///
/// This calls the destructor through the vtable.
pub fn drop_trait_object<'ctx>(
    codegen: &CodeGen<'ctx>,
    trait_obj: BasicValueEnum<'ctx>,
) -> CodegenResult<()> {
    // Extract vtable pointer
    let vtable_ptr = get_trait_vtable_ptr(codegen, trait_obj)?;

    // Extract data pointer
    let data_ptr = get_trait_data_ptr(codegen, trait_obj)?;

    // Load drop function pointer from vtable (offset 12, after type_id/size/align)
    let drop_fn_ptr_ptr = unsafe {
        codegen
            .builder
            .build_gep(
                codegen.context.i8_type(),
                vtable_ptr,
                &[codegen.context.i64_type().const_int(12, false)],
                "drop_fn_ptr_addr",
            )
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?
    };

    let fn_ptr_type = codegen
        .context
        .i8_type()
        .fn_type(
            &[codegen
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into()],
            false,
        )
        .ptr_type(AddressSpace::default());

    let drop_fn_ptr = codegen
        .builder
        .build_pointer_cast(drop_fn_ptr_ptr, fn_ptr_type, "drop_fn_ptr")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Call drop function
    let fn_type = codegen.context.void_type().fn_type(
        &[codegen
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .into()],
        false,
    );

    codegen
        .builder
        .build_indirect_call(fn_type, drop_fn_ptr, &[data_ptr.into()], "drop_call")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(())
}

/// Casts a trait object to a different trait (trait upcasting).
///
/// This requires that the target trait is a supertrait of the source trait.
pub fn upcast_trait_object<'ctx>(
    _codegen: &CodeGen<'ctx>,
    _trait_obj: BasicValueEnum<'ctx>,
    _target_trait: &TraitInfo,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    // In a full implementation, this would:
    // 1. Look up the vtable for the target trait
    // 2. Create a new trait object with the same data pointer but different vtable
    Err(CodegenError::unsupported_instruction(
        "trait upcasting not yet implemented",
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::context::Context;

    #[test]
    fn test_trait_object_type() {
        let context = Context::create();
        let ty = trait_object_type(&context);

        assert_eq!(ty.count_fields(), 2);
    }

    #[test]
    fn test_trait_info() {
        let info = TraitInfo {
            name: "ToString".to_string(),
            methods: vec![(
                "to_string".to_string(),
                vec![],
                Ty::Named("String".to_string()),
            )],
        };

        assert_eq!(info.methods.len(), 1);
    }

    #[test]
    fn test_vtable_type() {
        let context = Context::create();
        let codegen = CodeGen::new(&context, "test");

        let trait_info = TraitInfo {
            name: "TestTrait".to_string(),
            methods: vec![
                ("method1".to_string(), vec![], Ty::Void),
                ("method2".to_string(), vec![Ty::I32], Ty::I32),
            ],
        };

        let vtable_ty = vtable_type(&codegen, &trait_info).unwrap();
        // Fields: type_id, type_size, type_align, drop_fn, method1, method2
        assert_eq!(vtable_ty.count_fields(), 6);
    }
}
