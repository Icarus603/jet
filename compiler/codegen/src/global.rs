//! Global Variable and Type Definition Compilation
//!
//! This module handles the compilation of global variables and type definitions
//! from Jet IR to LLVM IR.

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use crate::types::TypeMapping;
use inkwell::module::Linkage;
use inkwell::values::GlobalValue;
use jet_ir::{Constant, Global, TypeDef, TypeKind};

/// Compiles all global variables in the module.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `globals` - The global variables to compile
///
/// # Returns
///
/// Returns Ok(()) on success, or a CodegenError on failure.
pub fn compile_globals<'ctx>(codegen: &mut CodeGen<'ctx>, globals: &[Global]) -> CodegenResult<()> {
    for global in globals {
        compile_global(codegen, global)?;
    }
    Ok(())
}

/// Compiles a single global variable to LLVM.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `global` - The global variable to compile
///
/// # Returns
///
/// Returns the compiled global value on success.
fn compile_global<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    global: &Global,
) -> CodegenResult<GlobalValue<'ctx>> {
    // Convert the Jet type to LLVM type
    let llvm_ty = codegen.jet_to_llvm(&global.ty)?;

    // Create the global variable
    let global_val = codegen.module.add_global(llvm_ty, None, &global.name);

    // Set linkage based on visibility
    if global.is_external || global.is_exported {
        global_val.set_linkage(Linkage::External);
    } else {
        global_val.set_linkage(Linkage::Internal);
    }

    // Set initial value if provided
    if let Some(ref constant) = global.initial_value {
        let init_val = compile_constant_value(codegen, constant)?;
        global_val.set_initializer(&init_val);
    } else if !global.is_external {
        // For non-external globals without initializers, use zero initializer
        global_val.set_initializer(&llvm_ty.const_zero());
    }

    Ok(global_val)
}

/// Compiles a constant value for global initializers.
fn compile_constant_value<'ctx>(
    codegen: &CodeGen<'ctx>,
    constant: &Constant,
) -> CodegenResult<inkwell::values::BasicValueEnum<'ctx>> {
    match constant {
        Constant::Int(val, ty) => {
            let int_ty = codegen.jet_to_llvm(ty)?.into_int_type();
            Ok(int_ty.const_int(*val as u64, true).into())
        }
        Constant::Float(val, ty) => {
            let float_ty = if *ty == jet_ir::Ty::F32 {
                codegen.context.f32_type()
            } else {
                codegen.context.f64_type()
            };
            Ok(float_ty.const_float(*val).into())
        }
        Constant::Bool(val) => Ok(codegen
            .context
            .bool_type()
            .const_int(*val as u64, false)
            .into()),
        Constant::Null(ty) => {
            let ptr_ty = codegen.jet_to_llvm(ty)?.into_pointer_type();
            Ok(ptr_ty.const_null().into())
        }
        Constant::Undef(ty) | Constant::Zero(ty) => {
            let llvm_ty = codegen.jet_to_llvm(ty)?;
            Ok(llvm_ty.const_zero())
        }
        Constant::String(s) => {
            // Create a global string constant
            let string_global = codegen
                .builder
                .build_global_string_ptr(s, "str.const")
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
            Ok(string_global.as_pointer_value().into())
        }
    }
}

/// Compiles all type definitions in the module.
///
/// This creates LLVM struct types for struct and enum definitions.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `type_defs` - The type definitions to compile
///
/// # Returns
///
/// Returns Ok(()) on success, or a CodegenError on failure.
pub fn compile_type_definitions<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    type_defs: &[TypeDef],
) -> CodegenResult<()> {
    // First pass: Create opaque struct types for all named types
    for type_def in type_defs {
        // Create the struct type (opaque for now)
        let struct_ty = codegen.context.opaque_struct_type(&type_def.name);

        // Store in type cache for later resolution
        // We use a special marker type to indicate this is a named struct
        let named_ty = jet_ir::Ty::Named(type_def.name.clone());
        codegen.set_cached_type(named_ty, struct_ty.into());
    }

    // Second pass: Set body for struct types
    for type_def in type_defs {
        match &type_def.kind {
            TypeKind::Struct(fields) => {
                compile_struct_type(codegen, &type_def.name, fields)?;
            }
            TypeKind::Enum(variants) => {
                compile_enum_type(codegen, &type_def.name, variants)?;
            }
            TypeKind::Alias(ty) => {
                // For type aliases, just map the underlying type
                let llvm_ty = codegen.jet_to_llvm(ty)?;
                let named_ty = jet_ir::Ty::Named(type_def.name.clone());
                codegen.set_cached_type(named_ty, llvm_ty);
            }
            TypeKind::Opaque => {
                // Opaque types remain opaque (already created in first pass)
            }
        }
    }

    Ok(())
}

/// Compiles a struct type definition.
fn compile_struct_type<'ctx>(
    codegen: &CodeGen<'ctx>,
    name: &str,
    fields: &[jet_ir::Ty],
) -> CodegenResult<()> {
    // Get the opaque struct type
    let struct_ty = codegen
        .module
        .get_struct_type(name)
        .ok_or_else(|| CodegenError::unsupported_type(format!("struct {} not found", name)))?;

    // Convert field types
    let field_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> = fields
        .iter()
        .map(|f| codegen.jet_to_llvm(f))
        .collect::<CodegenResult<Vec<_>>>()?;

    // Set the struct body
    struct_ty.set_body(&field_types, false);

    Ok(())
}

/// Compiles an enum type definition.
///
/// Enums are compiled as a struct with a discriminant and a union of payloads.
/// For simplicity, we use a struct with the discriminant as the first field
/// and a pointer-sized payload as the second field.
fn compile_enum_type<'ctx>(
    codegen: &CodeGen<'ctx>,
    name: &str,
    variants: &[(String, Vec<jet_ir::Ty>)],
) -> CodegenResult<()> {
    // Get the opaque struct type
    let struct_ty = codegen
        .module
        .get_struct_type(name)
        .ok_or_else(|| CodegenError::unsupported_type(format!("enum {} not found", name)))?;

    // Calculate the maximum payload size
    let max_payload_size = variants
        .iter()
        .map(|(_, fields)| {
            fields
                .iter()
                .map(|f| codegen.get_type_size(f).unwrap_or(8))
                .sum::<u64>()
        })
        .max()
        .unwrap_or(0);

    // Build enum struct: { discriminant: i32, payload: [max_payload_size x i8] }
    // The payload is represented as a byte array large enough for any variant
    let discriminant_ty = codegen.context.i32_type();

    // Create payload type - use a byte array for flexibility
    let payload_size = max_payload_size.max(8); // At least pointer-sized
    let payload_ty = codegen
        .context
        .i8_type()
        .array_type(payload_size as u32)
        .into();

    struct_ty.set_body(&[discriminant_ty.into(), payload_ty], false);

    Ok(())
}

/// Gets the LLVM type for a named type.
pub fn get_named_type<'ctx>(
    codegen: &CodeGen<'ctx>,
    name: &str,
) -> Option<inkwell::types::BasicTypeEnum<'ctx>> {
    codegen.module.get_struct_type(name).map(|t| t.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::context::Context;

    #[test]
    fn test_compile_global() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        // Create a simple global
        let global = Global::new("counter", jet_ir::Ty::I32).with_initial_value(Constant::i32(42));

        let result = compile_global(&mut codegen, &global);
        assert!(result.is_ok());

        // Check the global was created
        let llvm_global = codegen.module.get_global("counter");
        assert!(llvm_global.is_some());
    }

    #[test]
    fn test_compile_external_global() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        // Create an external global
        let global = Global::external("extern_var", jet_ir::Ty::I64);

        let result = compile_global(&mut codegen, &global);
        assert!(result.is_ok());

        // Check the global has external linkage
        let llvm_global = codegen.module.get_global("extern_var").unwrap();
        assert!(llvm_global.get_linkage() == Linkage::External);
    }

    #[test]
    fn test_compile_struct_type_def() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        // First pass: create opaque type
        let struct_ty = codegen.context.opaque_struct_type("Point");
        codegen.set_cached_type(jet_ir::Ty::Named("Point".to_string()), struct_ty.into());

        // Second pass: set body
        let result = compile_struct_type(&codegen, "Point", &[jet_ir::Ty::F64, jet_ir::Ty::F64]);
        assert!(result.is_ok());

        // Check the struct type
        let point_ty = codegen.module.get_struct_type("Point").unwrap();
        assert_eq!(point_ty.count_fields(), 2);
    }

    #[test]
    fn test_compile_enum_type_def() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        // First pass: create opaque type
        let struct_ty = codegen.context.opaque_struct_type("Option");
        codegen.set_cached_type(jet_ir::Ty::Named("Option".to_string()), struct_ty.into());

        // Second pass: compile enum
        let variants = vec![
            ("None".to_string(), vec![]),
            ("Some".to_string(), vec![jet_ir::Ty::I64]),
        ];
        let result = compile_enum_type(&codegen, "Option", &variants);
        assert!(result.is_ok());

        // Check the enum type
        let option_ty = codegen.module.get_struct_type("Option").unwrap();
        assert_eq!(option_ty.count_fields(), 2); // discriminant + payload
    }
}
