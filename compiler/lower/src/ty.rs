//! Type conversion from AST types to IR types.
//!
//! This module provides functions for converting Jet AST types to Jet IR types.

use jet_ir::Ty;
use jet_parser::ast;

/// Converts an AST type to an IR type.
pub fn lower_type(ty: &ast::Type) -> Ty {
    match ty {
        ast::Type::Path(path) => lower_path_type(path),
        ast::Type::Generic(base, args) => lower_generic_type(base, args),
        ast::Type::Tuple(types) => Ty::Struct(types.iter().map(lower_type).collect()),
        ast::Type::Array(elem, size) => lower_array_type(elem, size.as_deref()),
        ast::Type::Function {
            params,
            return_type,
            ..
        } => lower_function_type(params, return_type.as_deref()),
        ast::Type::Reference { inner, .. } => Ty::Ptr(Box::new(lower_type(inner))),
        ast::Type::Channel(elem) => Ty::Ptr(Box::new(lower_type(elem))),
        ast::Type::Async(inner) => lower_type(inner),
        ast::Type::Infer => Ty::Named("_".to_string()),
        ast::Type::SelfType => Ty::Named("Self".to_string()),
    }
}

/// Converts a path type to an IR type.
fn lower_path_type(path: &ast::Path) -> Ty {
    if path.segments.is_empty() {
        return Ty::Void;
    }

    let name = &path.segments[0].name;

    match name.as_str() {
        "int" => Ty::I32,
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
fn lower_generic_type(base: &ast::Type, args: &[ast::Type]) -> Ty {
    let base_ty = lower_type(base);
    let arg_tys: Vec<Ty> = args.iter().map(lower_type).collect();

    match base_ty {
        Ty::Named(name) => Ty::Generic(name, arg_tys),
        _ => base_ty,
    }
}

/// Converts an array type to an IR type.
fn lower_array_type(elem: &ast::Type, _size: Option<&ast::Expr>) -> Ty {
    // For now, treat all arrays as pointers
    // Size information would be extracted from the expression
    Ty::Ptr(Box::new(lower_type(elem)))
}

/// Converts a function type to an IR type.
fn lower_function_type(params: &[ast::Type], ret: Option<&ast::Type>) -> Ty {
    let param_tys: Vec<Ty> = params.iter().map(lower_type).collect();
    let ret_ty = ret.map(lower_type).unwrap_or(Ty::Void);

    Ty::Function(param_tys, Box::new(ret_ty))
}

/// Gets the default type for an integer literal.
pub fn int_literal_type() -> Ty {
    Ty::I32
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

    fn make_ident(name: &str) -> Ident {
        Ident::new(name, Span::new(0, 0))
    }

    fn make_path(name: &str) -> ast::Path {
        ast::Path::new(vec![make_ident(name)], Span::new(0, 0))
    }

    #[test]
    fn test_primitive_types() {
        assert_eq!(lower_type(&ast::Type::Path(make_path("int"))), Ty::I32);
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
        assert_eq!(int_literal_type(), Ty::I32);
        assert_eq!(float_literal_type(), Ty::F64);
        assert_eq!(bool_literal_type(), Ty::Bool);
        assert_eq!(string_literal_type(), Ty::Ptr(Box::new(Ty::I8)));
        assert_eq!(char_literal_type(), Ty::I32);
        assert_eq!(unit_type(), Ty::Void);
    }
}
