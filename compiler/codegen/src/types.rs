//! Type Mapping
//!
//! This module provides utilities for mapping Jet IR types to LLVM types.
//! It handles all the type conversions including primitives, pointers,
//! structs, arrays, and functions.

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use inkwell::types::{
    ArrayType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, FunctionType, IntType,
    PointerType, StructType,
};
use inkwell::AddressSpace;
use jet_ir::Ty;

/// Extension trait for type conversion operations.
pub trait TypeMapping<'ctx> {
    /// Converts a Jet IR type to an LLVM BasicTypeEnum.
    fn jet_to_llvm(&self, ty: &Ty) -> CodegenResult<BasicTypeEnum<'ctx>>;

    /// Converts a Jet IR type to an LLVM BasicMetadataTypeEnum (for function parameters).
    fn jet_to_llvm_metadata(&self, ty: &Ty) -> CodegenResult<BasicMetadataTypeEnum<'ctx>>;

    /// Gets the LLVM integer type for a given bit width.
    fn get_int_type(&self, bits: u32) -> CodegenResult<IntType<'ctx>>;

    /// Gets the LLVM float type for a given bit width.
    fn get_float_type(&self, bits: u32) -> CodegenResult<FloatType<'ctx>>;

    /// Creates a pointer type to the given element type.
    fn get_pointer_type(&self, elem_ty: BasicTypeEnum<'ctx>) -> PointerType<'ctx>;

    /// Creates a struct type with the given field types.
    fn get_struct_type(&self, fields: &[BasicTypeEnum<'ctx>], packed: bool) -> StructType<'ctx>;

    /// Creates an array type with the given element type and count.
    fn get_array_type(&self, elem_ty: BasicTypeEnum<'ctx>, count: usize) -> ArrayType<'ctx>;

    /// Gets the size of a type in bytes.
    fn get_type_size(&self, ty: &Ty) -> CodegenResult<u64>;

    /// Gets the alignment of a type in bytes.
    fn get_type_align(&self, ty: &Ty) -> CodegenResult<u64>;
}

impl<'ctx> TypeMapping<'ctx> for CodeGen<'ctx> {
    fn jet_to_llvm(&self, ty: &Ty) -> CodegenResult<BasicTypeEnum<'ctx>> {
        // Use the context's convert_type method
        // We need to clone self to call the mutable method
        // In practice, types should be cached
        match ty {
            // Void/unit type is represented as an empty struct (zero-sized type)
            // This allows it to be used as a BasicType for values
            Ty::Void => Ok(self.context.struct_type(&[], false).into()),
            Ty::Int(1) => Ok(self.context.bool_type().into()),
            Ty::Int(8) => Ok(self.context.i8_type().into()),
            Ty::Int(16) => Ok(self.context.i16_type().into()),
            Ty::Int(32) => Ok(self.context.i32_type().into()),
            Ty::Int(64) => Ok(self.context.i64_type().into()),
            Ty::Int(128) => Ok(self.context.i128_type().into()),
            Ty::Int(bits) => Ok(self.context.custom_width_int_type(*bits).into()),
            Ty::Float(32) => Ok(self.context.f32_type().into()),
            Ty::Float(64) => Ok(self.context.f64_type().into()),
            Ty::Float(bits) => Err(CodegenError::unsupported_type(format!(
                "float{} (only f32 and f64 supported)",
                bits
            ))),
            Ty::Bool => Ok(self.context.bool_type().into()),
            Ty::Ptr(inner) => {
                let inner_llvm = self.jet_to_llvm(inner)?;
                Ok(inner_llvm.ptr_type(AddressSpace::default()).into())
            }
            Ty::Struct(fields) => {
                let field_types: Vec<BasicTypeEnum<'ctx>> = fields
                    .iter()
                    .map(|f| self.jet_to_llvm(f))
                    .collect::<CodegenResult<Vec<_>>>()?;
                let struct_ty = self.context.struct_type(&field_types, false);
                Ok(struct_ty.into())
            }
            Ty::Array(elem, count) => {
                let elem_ty = self.jet_to_llvm(elem)?;
                Ok(elem_ty.array_type(*count as u32).into())
            }
            Ty::Function(params, ret) => {
                let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = params
                    .iter()
                    .map(|p| self.jet_to_llvm_metadata(p))
                    .collect::<CodegenResult<Vec<_>>>()?;
                let fn_ty = if ret.is_void() {
                    self.context.void_type().fn_type(&param_types, false)
                } else {
                    let ret_ty = self.jet_to_llvm(ret)?;
                    ret_ty.fn_type(&param_types, false)
                };
                Ok(fn_ty.ptr_type(AddressSpace::default()).into())
            }
            // Named and generic types are represented as opaque pointers.
            // All Jet user-defined types are GC-allocated heap objects.
            Ty::Named(_name) => Ok(self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into()),
            Ty::Generic(_name, _args) => Ok(self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into()),
        }
    }

    fn jet_to_llvm_metadata(&self, ty: &Ty) -> CodegenResult<BasicMetadataTypeEnum<'ctx>> {
        self.jet_to_llvm(ty).map(|t| t.into())
    }

    fn get_int_type(&self, bits: u32) -> CodegenResult<IntType<'ctx>> {
        match bits {
            1 => Ok(self.context.bool_type()),
            8 => Ok(self.context.i8_type()),
            16 => Ok(self.context.i16_type()),
            32 => Ok(self.context.i32_type()),
            64 => Ok(self.context.i64_type()),
            128 => Ok(self.context.i128_type()),
            _ => Ok(self.context.custom_width_int_type(bits)),
        }
    }

    fn get_float_type(&self, bits: u32) -> CodegenResult<FloatType<'ctx>> {
        match bits {
            32 => Ok(self.context.f32_type()),
            64 => Ok(self.context.f64_type()),
            _ => Err(CodegenError::unsupported_type(format!(
                "float{} (only f32 and f64 supported)",
                bits
            ))),
        }
    }

    fn get_pointer_type(&self, elem_ty: BasicTypeEnum<'ctx>) -> PointerType<'ctx> {
        elem_ty.ptr_type(AddressSpace::default())
    }

    fn get_struct_type(&self, fields: &[BasicTypeEnum<'ctx>], packed: bool) -> StructType<'ctx> {
        self.context.struct_type(fields, packed)
    }

    fn get_array_type(&self, elem_ty: BasicTypeEnum<'ctx>, count: usize) -> ArrayType<'ctx> {
        elem_ty.array_type(count as u32)
    }

    fn get_type_size(&self, ty: &Ty) -> CodegenResult<u64> {
        // Simplified size calculation - assumes 64-bit target
        let size = match ty {
            Ty::Void => 0,
            Ty::Int(bits) => (*bits as u64).div_ceil(8),
            Ty::Float(32) => 4,
            Ty::Float(64) => 8,
            Ty::Float(bits) => (*bits as u64).div_ceil(8),
            Ty::Bool => 1,
            Ty::Ptr(_) => 8,
            Ty::Struct(fields) => fields
                .iter()
                .try_fold(0u64, |acc, f| self.get_type_size(f).map(|s| acc + s))?,
            Ty::Array(elem, count) => self.get_type_size(elem)? * (*count as u64),
            Ty::Function(_, _) => 8,
            // Named and generic types are pointer-sized (GC heap objects)
            Ty::Named(_) | Ty::Generic(_, _) => 8,
        };
        Ok(size)
    }

    fn get_type_align(&self, ty: &Ty) -> CodegenResult<u64> {
        // Simplified alignment calculation - assumes 64-bit target
        let align = match ty {
            Ty::Void => 1,
            Ty::Int(bits) => {
                let size = (*bits as u64).div_ceil(8);
                size.next_power_of_two().min(8)
            }
            Ty::Float(32) => 4,
            Ty::Float(64) => 8,
            Ty::Float(bits) => {
                let size = (*bits as u64).div_ceil(8);
                size.next_power_of_two().min(8)
            }
            Ty::Bool => 1,
            Ty::Ptr(_) => 8,
            Ty::Struct(fields) => {
                // Alignment of struct is max alignment of fields
                fields
                    .iter()
                    .try_fold(1u64, |acc, f| self.get_type_align(f).map(|a| acc.max(a)))?
            }
            Ty::Array(elem, _) => self.get_type_align(elem)?,
            Ty::Function(_, _) => 8,
            // Named and generic types are pointer-aligned (GC heap objects)
            Ty::Named(_) | Ty::Generic(_, _) => 8,
        };
        Ok(align)
    }
}

/// Creates a function type from parameter types and return type.
pub fn create_function_type<'ctx>(
    context: &'ctx inkwell::context::Context,
    params: &[BasicMetadataTypeEnum<'ctx>],
    ret: Option<BasicTypeEnum<'ctx>>,
    is_var_args: bool,
) -> FunctionType<'ctx> {
    match ret {
        Some(ret_ty) => ret_ty.fn_type(params, is_var_args),
        None => context.void_type().fn_type(params, is_var_args),
    }
}

/// Gets the element type of a pointer type.
pub fn get_pointee_type(ty: BasicTypeEnum<'_>) -> Option<BasicTypeEnum<'_>> {
    match ty {
        BasicTypeEnum::PointerType(_ptr_ty) => {
            // Try to get the element type
            // Note: In inkwell, we can't always get the element type from a pointer
            // This is a limitation of the LLVM C API
            None
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::context::Context;

    fn create_test_codegen<'ctx>(context: &'ctx Context) -> CodeGen<'ctx> {
        CodeGen::new(context, "test")
    }

    #[test]
    fn test_integer_types() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        assert!(codegen.get_int_type(8).is_ok());
        assert!(codegen.get_int_type(32).is_ok());
        assert!(codegen.get_int_type(64).is_ok());
        assert!(codegen.get_int_type(128).is_ok());
        assert!(codegen.get_int_type(256).is_ok()); // Custom width
    }

    #[test]
    fn test_float_types() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        assert!(codegen.get_float_type(32).is_ok());
        assert!(codegen.get_float_type(64).is_ok());
        assert!(codegen.get_float_type(16).is_err());
    }

    #[test]
    fn test_pointer_type() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let i32_ty = context.i32_type().into();
        let ptr_ty = codegen.get_pointer_type(i32_ty);
        assert_eq!(ptr_ty.get_address_space(), AddressSpace::default());
    }

    #[test]
    fn test_struct_type() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let fields = vec![context.i32_type().into(), context.i64_type().into()];
        let struct_ty = codegen.get_struct_type(&fields, false);
        assert_eq!(struct_ty.count_fields(), 2);
    }

    #[test]
    fn test_array_type() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        let elem_ty = context.i32_type().into();
        let arr_ty = codegen.get_array_type(elem_ty, 10);
        assert_eq!(arr_ty.len(), 10);
    }

    #[test]
    fn test_type_size() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        assert_eq!(codegen.get_type_size(&Ty::I8).unwrap(), 1);
        assert_eq!(codegen.get_type_size(&Ty::I32).unwrap(), 4);
        assert_eq!(codegen.get_type_size(&Ty::I64).unwrap(), 8);
        assert_eq!(codegen.get_type_size(&Ty::Bool).unwrap(), 1);
        assert_eq!(
            codegen.get_type_size(&Ty::Ptr(Box::new(Ty::I32))).unwrap(),
            8
        );
    }

    #[test]
    fn test_type_alignment() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        assert_eq!(codegen.get_type_align(&Ty::I8).unwrap(), 1);
        assert_eq!(codegen.get_type_align(&Ty::I32).unwrap(), 4);
        assert_eq!(codegen.get_type_align(&Ty::I64).unwrap(), 8);
        assert_eq!(codegen.get_type_align(&Ty::Bool).unwrap(), 1);
    }

    #[test]
    fn test_jet_to_llvm_conversion() {
        let context = Context::create();
        let codegen = create_test_codegen(&context);

        // Test all basic types
        assert!(codegen.jet_to_llvm(&Ty::I32).is_ok());
        assert!(codegen.jet_to_llvm(&Ty::I64).is_ok());
        assert!(codegen.jet_to_llvm(&Ty::F32).is_ok());
        assert!(codegen.jet_to_llvm(&Ty::F64).is_ok());
        assert!(codegen.jet_to_llvm(&Ty::Bool).is_ok());

        // Test pointer
        let ptr_ty = Ty::Ptr(Box::new(Ty::I32));
        assert!(codegen.jet_to_llvm(&ptr_ty).is_ok());

        // Test struct
        let struct_ty = Ty::Struct(vec![Ty::I32, Ty::I64]);
        assert!(codegen.jet_to_llvm(&struct_ty).is_ok());

        // Test array
        let arr_ty = Ty::Array(Box::new(Ty::I32), 10);
        assert!(codegen.jet_to_llvm(&arr_ty).is_ok());

        // Test function pointer
        let fn_ty = Ty::Function(vec![Ty::I32, Ty::I32], Box::new(Ty::I32));
        assert!(codegen.jet_to_llvm(&fn_ty).is_ok());
    }
}
