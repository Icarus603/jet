//! Type conversion from AST types to IR types.
//!
//! This module provides functions for converting Jet AST types to Jet IR types,
//! as well as converting type checker types (TypeId) to IR types.

use jet_ir::Ty;
use jet_parser::ast;
use jet_typeck::{DefId, FloatSize, IntSize, TypeContext, TypeId, TypeKind, TypeVarKind};
use std::collections::HashMap;

/// Converts an AST type to an IR type.
pub fn lower_type(ty: &ast::Type) -> Ty {
    lower_type_with_self(ty, None)
}

/// Converts an AST type to an IR type, with optional Self type substitution.
///
/// When `self_type` is provided, any `Self` references in the AST type will be
/// replaced with the provided type. This is used when lowering impl blocks
/// where `Self` refers to the concrete type being implemented.
pub fn lower_type_with_self(ty: &ast::Type, self_type: Option<&Ty>) -> Ty {
    match ty {
        ast::Type::Path(path) => lower_path_type(path),
        ast::Type::Generic(base, args) => lower_generic_type_with_self(base, args, self_type),
        ast::Type::Tuple(types) => Ty::Struct(
            types
                .iter()
                .map(|t| lower_type_with_self(t, self_type))
                .collect(),
        ),
        ast::Type::Array(elem, size) => {
            lower_array_type_with_self(elem, size.as_deref(), self_type)
        }
        ast::Type::Function {
            params,
            return_type,
            ..
        } => lower_function_type_with_self(params, return_type.as_deref(), self_type),
        ast::Type::Reference { inner, .. } => {
            Ty::Ptr(Box::new(lower_type_with_self(inner, self_type)))
        }
        ast::Type::Channel(elem) => Ty::Ptr(Box::new(lower_type_with_self(elem, self_type))),
        ast::Type::Async(inner) => lower_type_with_self(inner, self_type),
        ast::Type::Infer => Ty::Named("_".to_string()),
        ast::Type::SelfType => self_type
            .cloned()
            .unwrap_or_else(|| Ty::Named("Self".to_string())),
    }
}

/// Converts a path type to an IR type.
fn lower_path_type(path: &ast::Path) -> Ty {
    if path.segments.is_empty() {
        return Ty::Void;
    }

    let name = &path.segments[0].name;

    match name.as_str() {
        "int" => Ty::I64,
        "i8" => Ty::I8,
        "i16" => Ty::I16,
        "i32" => Ty::I32,
        "i64" => Ty::I64,
        "i128" => Ty::I128,
        "uint" => Ty::Int(64),
        "u8" => Ty::Int(8),
        "u16" => Ty::Int(16),
        "u32" => Ty::Int(32),
        "u64" => Ty::Int(64),
        "float" => Ty::F64,
        "f32" => Ty::F32,
        "f64" => Ty::F64,
        "bool" => Ty::Bool,
        "char" => Ty::I32, // Unicode scalar value
        "string" => Ty::Ptr(Box::new(Ty::I8)),
        "unit" => Ty::Void,
        "never" => Ty::Void, // Never type represented as void
        _ => Ty::Named(name.clone()),
    }
}

/// Converts a generic type to an IR type.
#[allow(dead_code)]
fn lower_generic_type(base: &ast::Type, args: &[ast::Type]) -> Ty {
    let base_ty = lower_type(base);
    let arg_tys: Vec<Ty> = args.iter().map(lower_type).collect();

    match base_ty {
        Ty::Named(name) => Ty::Generic(name, arg_tys),
        _ => base_ty,
    }
}

/// Converts a generic type to an IR type, with optional Self type substitution.
fn lower_generic_type_with_self(
    base: &ast::Type,
    args: &[ast::Type],
    self_type: Option<&Ty>,
) -> Ty {
    let base_ty = lower_type_with_self(base, self_type);
    let arg_tys: Vec<Ty> = args
        .iter()
        .map(|a| lower_type_with_self(a, self_type))
        .collect();

    match base_ty {
        Ty::Named(name) => Ty::Generic(name, arg_tys),
        _ => base_ty,
    }
}

/// Converts an array type to an IR type.
#[allow(dead_code)]
fn lower_array_type(elem: &ast::Type, _size: Option<&ast::Expr>) -> Ty {
    // For now, treat all arrays as pointers
    // Size information would be extracted from the expression
    Ty::Ptr(Box::new(lower_type(elem)))
}

/// Converts an array type to an IR type, with optional Self type substitution.
fn lower_array_type_with_self(
    elem: &ast::Type,
    _size: Option<&ast::Expr>,
    self_type: Option<&Ty>,
) -> Ty {
    // For now, treat all arrays as pointers
    Ty::Ptr(Box::new(lower_type_with_self(elem, self_type)))
}

/// Converts a function type to an IR type.
#[allow(dead_code)]
fn lower_function_type(params: &[ast::Type], ret: Option<&ast::Type>) -> Ty {
    let param_tys: Vec<Ty> = params.iter().map(lower_type).collect();
    let ret_ty = ret.map(lower_type).unwrap_or(Ty::Void);

    Ty::Function(param_tys, Box::new(ret_ty))
}

/// Converts a function type to an IR type, with optional Self type substitution.
fn lower_function_type_with_self(
    params: &[ast::Type],
    ret: Option<&ast::Type>,
    self_type: Option<&Ty>,
) -> Ty {
    let param_tys: Vec<Ty> = params
        .iter()
        .map(|p| lower_type_with_self(p, self_type))
        .collect();
    let ret_ty = ret
        .map(|r| lower_type_with_self(r, self_type))
        .unwrap_or(Ty::Void);

    Ty::Function(param_tys, Box::new(ret_ty))
}

/// Converts a type checker type (TypeId) to an IR type.
///
/// This is the main entry point for converting types from the type checker
/// to IR types during the lowering phase.
pub fn lower_typeck_type(type_id: TypeId, tcx: &TypeContext) -> Ty {
    let mut cache = HashMap::new();
    lower_typeck_type_cached(type_id, tcx, &mut cache)
}

/// Converts a type checker type to an IR type with caching to handle recursive types.
fn lower_typeck_type_cached(
    type_id: TypeId,
    tcx: &TypeContext,
    cache: &mut HashMap<TypeId, Ty>,
) -> Ty {
    // Check cache first to handle recursive types
    if let Some(ty) = cache.get(&type_id) {
        return ty.clone();
    }

    // Get the type kind, following links for type variables
    let kind = resolve_type_var(type_id, tcx);

    match kind {
        TypeKind::Int(size) => int_size_to_ty(size),
        TypeKind::Uint(size) => uint_size_to_ty(size),
        TypeKind::Float(size) => float_size_to_ty(size),
        TypeKind::Bool => Ty::Bool,
        TypeKind::Char => Ty::I32,
        TypeKind::String => Ty::Ptr(Box::new(Ty::I8)),
        TypeKind::Unit => Ty::Void,
        TypeKind::Never => Ty::Void,
        TypeKind::Tuple(elements) => {
            let field_tys: Vec<Ty> = elements
                .iter()
                .map(|&elem_id| lower_typeck_type_cached(elem_id, tcx, cache))
                .collect();
            Ty::Struct(field_tys)
        }
        TypeKind::Array(elem, size) => {
            let elem_ty = lower_typeck_type_cached(elem, tcx, cache);
            Ty::Array(Box::new(elem_ty), size)
        }
        TypeKind::Slice(elem) => {
            // Slices are represented as pointer + length, but for now just use pointer
            let elem_ty = lower_typeck_type_cached(elem, tcx, cache);
            Ty::Ptr(Box::new(elem_ty))
        }
        TypeKind::Struct(def_id) => {
            // Struct types are named - the actual definition is stored elsewhere
            Ty::Named(format!("struct_{}", def_id.0))
        }
        TypeKind::Enum(def_id) => {
            // Enum types are named - the actual definition is stored elsewhere
            Ty::Named(format!("enum_{}", def_id.0))
        }
        TypeKind::Function { params, ret, .. } => {
            let param_tys: Vec<Ty> = params
                .iter()
                .map(|&param_id| lower_typeck_type_cached(param_id, tcx, cache))
                .collect();
            let ret_ty = lower_typeck_type_cached(ret, tcx, cache);
            Ty::Function(param_tys, Box::new(ret_ty))
        }
        TypeKind::Ref(inner, _mutability) => {
            let inner_ty = lower_typeck_type_cached(inner, tcx, cache);
            Ty::Ptr(Box::new(inner_ty))
        }
        TypeKind::RawPtr(inner, _mutability) => {
            let inner_ty = lower_typeck_type_cached(inner, tcx, cache);
            Ty::Ptr(Box::new(inner_ty))
        }
        TypeKind::Var(var_id) => {
            // This should have been resolved by resolve_type_var
            // If we get here, the variable is unbound - use a placeholder
            Ty::Named(format!("'t{}", var_id))
        }
        TypeKind::Param(def_id) => {
            // Generic parameter - use a named type with the parameter name
            Ty::Named(format!("'p{}", def_id.0))
        }
        TypeKind::Channel(elem) => {
            // Channels are represented as pointers
            let elem_ty = lower_typeck_type_cached(elem, tcx, cache);
            Ty::Ptr(Box::new(elem_ty))
        }
        TypeKind::Async(inner) => {
            // Async types wrap the inner type
            lower_typeck_type_cached(inner, tcx, cache)
        }
    }
}

/// Resolves a type variable to its underlying type kind.
///
/// If the type is not a variable, returns the kind directly.
/// If the type is a variable, follows the link chain until finding
/// a concrete type or an unbound variable.
fn resolve_type_var(type_id: TypeId, tcx: &TypeContext) -> TypeKind {
    match tcx.type_kind(type_id) {
        TypeKind::Var(var_id) => {
            // Look up the type variable
            if let Some(var) = tcx.get_var(*var_id) {
                match &var.kind {
                    TypeVarKind::Link(linked_id) => {
                        // Follow the link recursively
                        resolve_type_var(*linked_id, tcx)
                    }
                    TypeVarKind::Unbound { .. } => {
                        // Unbound variable - return as-is
                        TypeKind::Var(*var_id)
                    }
                    TypeVarKind::Generic(def_id) => {
                        // Generic parameter
                        TypeKind::Param(*def_id)
                    }
                }
            } else {
                // Variable not found - return as-is
                TypeKind::Var(*var_id)
            }
        }
        _ => tcx.type_kind(type_id).clone(),
    }
}

/// Converts an integer size to an IR type.
fn int_size_to_ty(size: IntSize) -> Ty {
    match size {
        IntSize::I8 => Ty::I8,
        IntSize::I16 => Ty::I16,
        IntSize::I32 => Ty::I32,
        IntSize::I64 => Ty::I64,
        IntSize::Isize => Ty::I64, // Assuming 64-bit platform
    }
}

/// Converts an unsigned integer size to an IR type.
fn uint_size_to_ty(size: IntSize) -> Ty {
    // IR uses signed integers for both signed and unsigned
    // The difference is in the operations, not the representation
    match size {
        IntSize::I8 => Ty::Int(8),
        IntSize::I16 => Ty::Int(16),
        IntSize::I32 => Ty::Int(32),
        IntSize::I64 => Ty::Int(64),
        IntSize::Isize => Ty::Int(64), // Assuming 64-bit platform
    }
}

/// Converts a float size to an IR type.
fn float_size_to_ty(size: FloatSize) -> Ty {
    match size {
        FloatSize::F32 => Ty::F32,
        FloatSize::F64 => Ty::F64,
    }
}

/// Converts a type checker type to an IR type with generic parameter substitution.
///
/// This is used when lowering generic functions with known type arguments.
/// The `param_substitutions` map provides the concrete types for generic parameters.
pub fn lower_typeck_type_with_substitutions(
    type_id: TypeId,
    tcx: &TypeContext,
    param_substitutions: &HashMap<DefId, TypeId>,
) -> Ty {
    let mut cache = HashMap::new();
    lower_typeck_type_with_subs_cached(type_id, tcx, param_substitutions, &mut cache)
}

/// Internal implementation with caching.
fn lower_typeck_type_with_subs_cached(
    type_id: TypeId,
    tcx: &TypeContext,
    param_substitutions: &HashMap<DefId, TypeId>,
    cache: &mut HashMap<TypeId, Ty>,
) -> Ty {
    // Check cache first
    if let Some(ty) = cache.get(&type_id) {
        return ty.clone();
    }

    // Get the type kind, following links for type variables
    let kind = resolve_type_var(type_id, tcx);

    match kind {
        TypeKind::Param(def_id) => {
            // Look up substitution for this parameter
            if let Some(&substituted_id) = param_substitutions.get(&def_id) {
                lower_typeck_type_cached(substituted_id, tcx, cache)
            } else {
                // No substitution - keep as parameter
                Ty::Named(format!("'p{}", def_id.0))
            }
        }
        _ => {
            // For other types, use the standard conversion
            // but we need to handle the substitution for nested types
            lower_typeck_type_with_subs_recursive(kind, tcx, param_substitutions, cache)
        }
    }
}

/// Recursively converts a type kind with substitution.
fn lower_typeck_type_with_subs_recursive(
    kind: TypeKind,
    tcx: &TypeContext,
    param_substitutions: &HashMap<DefId, TypeId>,
    cache: &mut HashMap<TypeId, Ty>,
) -> Ty {
    match kind {
        TypeKind::Int(size) => int_size_to_ty(size),
        TypeKind::Uint(size) => uint_size_to_ty(size),
        TypeKind::Float(size) => float_size_to_ty(size),
        TypeKind::Bool => Ty::Bool,
        TypeKind::Char => Ty::I32,
        TypeKind::String => Ty::Ptr(Box::new(Ty::I8)),
        TypeKind::Unit => Ty::Void,
        TypeKind::Never => Ty::Void,
        TypeKind::Tuple(elements) => {
            let field_tys: Vec<Ty> = elements
                .iter()
                .map(|&elem_id| {
                    lower_typeck_type_with_subs_cached(elem_id, tcx, param_substitutions, cache)
                })
                .collect();
            Ty::Struct(field_tys)
        }
        TypeKind::Array(elem, size) => {
            let elem_ty = lower_typeck_type_with_subs_cached(elem, tcx, param_substitutions, cache);
            Ty::Array(Box::new(elem_ty), size)
        }
        TypeKind::Slice(elem) => {
            let elem_ty = lower_typeck_type_with_subs_cached(elem, tcx, param_substitutions, cache);
            Ty::Ptr(Box::new(elem_ty))
        }
        TypeKind::Struct(def_id) => Ty::Named(format!("struct_{}", def_id.0)),
        TypeKind::Enum(def_id) => Ty::Named(format!("enum_{}", def_id.0)),
        TypeKind::Function { params, ret, .. } => {
            let param_tys: Vec<Ty> = params
                .iter()
                .map(|&param_id| {
                    lower_typeck_type_with_subs_cached(param_id, tcx, param_substitutions, cache)
                })
                .collect();
            let ret_ty = lower_typeck_type_with_subs_cached(ret, tcx, param_substitutions, cache);
            Ty::Function(param_tys, Box::new(ret_ty))
        }
        TypeKind::Ref(inner, _) => {
            let inner_ty =
                lower_typeck_type_with_subs_cached(inner, tcx, param_substitutions, cache);
            Ty::Ptr(Box::new(inner_ty))
        }
        TypeKind::RawPtr(inner, _) => {
            let inner_ty =
                lower_typeck_type_with_subs_cached(inner, tcx, param_substitutions, cache);
            Ty::Ptr(Box::new(inner_ty))
        }
        TypeKind::Var(var_id) => Ty::Named(format!("'t{}", var_id)),
        TypeKind::Param(def_id) => {
            // This should have been handled by the caller
            if let Some(&substituted_id) = param_substitutions.get(&def_id) {
                lower_typeck_type_cached(substituted_id, tcx, cache)
            } else {
                Ty::Named(format!("'p{}", def_id.0))
            }
        }
        TypeKind::Channel(elem) => {
            let elem_ty = lower_typeck_type_with_subs_cached(elem, tcx, param_substitutions, cache);
            Ty::Ptr(Box::new(elem_ty))
        }
        TypeKind::Async(inner) => {
            lower_typeck_type_with_subs_cached(inner, tcx, param_substitutions, cache)
        }
    }
}

/// Gets the default type for an integer literal.
pub fn int_literal_type() -> Ty {
    Ty::I64
}

/// Gets the default type for a float literal.
pub fn float_literal_type() -> Ty {
    Ty::F64
}

/// Gets the type for a boolean literal.
pub fn bool_literal_type() -> Ty {
    Ty::Bool
}

/// Gets the type for a string literal.
pub fn string_literal_type() -> Ty {
    Ty::Ptr(Box::new(Ty::I8))
}

/// Gets the type for a character literal.
pub fn char_literal_type() -> Ty {
    Ty::I32
}

/// Gets the unit type.
pub fn unit_type() -> Ty {
    Ty::Void
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span;
    use jet_parser::ast::Ident;
    use jet_typeck::Mutability;

    fn make_ident(name: &str) -> Ident {
        Ident::new(name, Span::new(0, 0))
    }

    fn make_path(name: &str) -> ast::Path {
        ast::Path::new(vec![make_ident(name)], Span::new(0, 0))
    }

    #[test]
    fn test_primitive_types() {
        assert_eq!(lower_type(&ast::Type::Path(make_path("int"))), Ty::I64);
        assert_eq!(lower_type(&ast::Type::Path(make_path("i32"))), Ty::I32);
        assert_eq!(lower_type(&ast::Type::Path(make_path("i64"))), Ty::I64);
        assert_eq!(lower_type(&ast::Type::Path(make_path("bool"))), Ty::Bool);
        assert_eq!(lower_type(&ast::Type::Path(make_path("f32"))), Ty::F32);
        assert_eq!(lower_type(&ast::Type::Path(make_path("f64"))), Ty::F64);
    }

    #[test]
    fn test_tuple_type() {
        let tuple = ast::Type::Tuple(vec![
            ast::Type::Path(make_path("i32")),
            ast::Type::Path(make_path("bool")),
        ]);

        let ty = lower_type(&tuple);
        assert_eq!(ty, Ty::Struct(vec![Ty::I32, Ty::Bool]));
    }

    #[test]
    fn test_reference_type() {
        let reference = ast::Type::Reference {
            mutable: false,
            inner: Box::new(ast::Type::Path(make_path("i32"))),
        };

        let ty = lower_type(&reference);
        assert_eq!(ty, Ty::Ptr(Box::new(Ty::I32)));
    }

    #[test]
    fn test_function_type() {
        let func = ast::Type::Function {
            params: vec![ast::Type::Path(make_path("i32"))],
            return_type: Some(Box::new(ast::Type::Path(make_path("bool")))),
            effects: vec![],
        };

        let ty = lower_type(&func);
        assert_eq!(ty, Ty::Function(vec![Ty::I32], Box::new(Ty::Bool)));
    }

    #[test]
    fn test_literal_types() {
        assert_eq!(int_literal_type(), Ty::I64);
        assert_eq!(float_literal_type(), Ty::F64);
        assert_eq!(bool_literal_type(), Ty::Bool);
        assert_eq!(string_literal_type(), Ty::Ptr(Box::new(Ty::I8)));
        assert_eq!(char_literal_type(), Ty::I32);
        assert_eq!(unit_type(), Ty::Void);
    }

    #[test]
    fn test_typeck_primitive_types() {
        let tcx = TypeContext::new();

        // Test primitive type conversions
        assert_eq!(lower_typeck_type(TypeId::INT, &tcx), Ty::I64);
        assert_eq!(lower_typeck_type(TypeId::BOOL, &tcx), Ty::Bool);
        assert_eq!(lower_typeck_type(TypeId::FLOAT, &tcx), Ty::F64);
        assert_eq!(
            lower_typeck_type(TypeId::STRING, &tcx),
            Ty::Ptr(Box::new(Ty::I8))
        );
        assert_eq!(lower_typeck_type(TypeId::UNIT, &tcx), Ty::Void);
        assert_eq!(lower_typeck_type(TypeId::NEVER, &tcx), Ty::Void);
    }

    #[test]
    fn test_typeck_tuple_type() {
        let mut tcx = TypeContext::new();
        let tuple = tcx.mk_tuple(vec![TypeId::INT, TypeId::BOOL]);

        let ty = lower_typeck_type(tuple, &tcx);
        assert_eq!(ty, Ty::Struct(vec![Ty::I64, Ty::Bool]));
    }

    #[test]
    fn test_typeck_function_type() {
        let mut tcx = TypeContext::new();
        let func = tcx.mk_function(
            vec![TypeId::INT, TypeId::INT],
            TypeId::BOOL,
            jet_typeck::EffectSet::empty(),
        );

        let ty = lower_typeck_type(func, &tcx);
        assert_eq!(ty, Ty::Function(vec![Ty::I64, Ty::I64], Box::new(Ty::Bool)));
    }

    #[test]
    fn test_typeck_reference_type() {
        let mut tcx = TypeContext::new();
        let ref_int = tcx.mk_ref(TypeId::INT, Mutability::Immutable);

        let ty = lower_typeck_type(ref_int, &tcx);
        assert_eq!(ty, Ty::Ptr(Box::new(Ty::I64)));
    }

    #[test]
    fn test_typeck_array_type() {
        let mut tcx = TypeContext::new();
        let arr = tcx.mk_array(TypeId::INT, 10);

        let ty = lower_typeck_type(arr, &tcx);
        assert_eq!(ty, Ty::Array(Box::new(Ty::I64), 10));
    }

    #[test]
    fn test_typeck_int_sizes() {
        let mut tcx = TypeContext::new();

        let i8_ty = tcx.mk_int(IntSize::I8);
        let i16_ty = tcx.mk_int(IntSize::I16);
        let i32_ty = tcx.mk_int(IntSize::I32);
        let i64_ty = tcx.mk_int(IntSize::I64);

        assert_eq!(lower_typeck_type(i8_ty, &tcx), Ty::I8);
        assert_eq!(lower_typeck_type(i16_ty, &tcx), Ty::I16);
        assert_eq!(lower_typeck_type(i32_ty, &tcx), Ty::I32);
        assert_eq!(lower_typeck_type(i64_ty, &tcx), Ty::I64);
    }

    #[test]
    fn test_typeck_float_sizes() {
        let mut tcx = TypeContext::new();

        let f32_ty = tcx.mk_float(FloatSize::F32);
        let f64_ty = tcx.mk_float(FloatSize::F64);

        assert_eq!(lower_typeck_type(f32_ty, &tcx), Ty::F32);
        assert_eq!(lower_typeck_type(f64_ty, &tcx), Ty::F64);
    }
}
