//! Function lowering from AST to IR.
//!
//! This module provides functions for lowering AST function definitions to Jet IR functions.

use crate::context::LoweringContext;
use crate::expr::lower_expr;
use crate::ty::lower_type;
use jet_ir::{Effect, Function, Param, Terminator, Ty, ValueId};
use jet_parser::ast;

/// Lowers a function definition from AST to IR.
pub fn lower_function(ctx: &mut LoweringContext, func: &ast::Function) -> usize {
    // Lower parameter types
    let mut params = Vec::new();
    let mut param_values = Vec::new();

    for (i, param) in func.params.iter().enumerate() {
        let param_ty = lower_type(&param.ty);
        let param_value = ValueId::new(i as u32);

        let param_name = get_pattern_name(&param.pattern);
        params.push(Param::new(param_name, param_ty.clone(), param_value));
        param_values.push((
            param_name.to_string(),
            param_value,
            param_ty,
            is_pattern_mutable(&param.pattern),
        ));
    }

    // Lower return type
    let return_ty = func
        .return_type
        .as_ref()
        .map(lower_type)
        .unwrap_or(Ty::Void);

    // Create the IR function
    let mut ir_func = Function::new(&func.name.name, params, return_ty.clone());

    // Add effects
    for effect in &func.effects {
        if let Some(ir_effect) = lower_effect(effect) {
            ir_func.effects.push(ir_effect);
        }
    }

    // Mark as exported if public
    if func.public {
        ir_func.is_exported = true;
    }

    // Add function to module
    let func_index = ctx.module.functions.len();
    ctx.module.add_function(ir_func);

    // Start lowering the function body
    ctx.start_function(func_index);

    // Create entry block
    let entry_block = ctx.create_block("entry");
    ctx.set_current_block(entry_block);

    // Bind parameters in the entry block
    for (name, value, ty, is_mut) in param_values {
        // Allocate space for the parameter
        let alloc = ctx.new_value();
        ctx.emit(jet_ir::Instruction::Alloc {
            result: alloc,
            ty: ty.clone(),
        });

        // Store the parameter value
        ctx.emit(jet_ir::Instruction::Store { ptr: alloc, value });

        // Bind the parameter name to its allocation
        ctx.bind_variable(name, alloc, ty, is_mut);
    }

    // Lower the function body
    let body_value = lower_expr(ctx, &func.body);

    // Add return terminator if not already terminated
    let current_block = ctx.get_current_block();
    if current_block.map(|b| !b.is_terminated()).unwrap_or(false) {
        // If the function returns a value, return the body value
        // Otherwise return void
        let ret_value = if return_ty.is_void() {
            None
        } else {
            Some(body_value)
        };

        ctx.terminate(Terminator::Return(ret_value));
    }

    // End function lowering
    ctx.end_function();

    func_index
}

/// Lowers an external function declaration.
pub fn lower_external_function(
    ctx: &mut LoweringContext,
    name: &str,
    params: &[ast::Param],
    return_type: Option<&ast::Type>,
    effects: &[ast::Type],
) -> usize {
    // Lower parameter types
    let ir_params: Vec<Param> = params
        .iter()
        .enumerate()
        .map(|(i, param)| {
            let ty = lower_type(&param.ty);
            let name = get_pattern_name(&param.pattern);
            Param::new(name, ty, ValueId::new(i as u32))
        })
        .collect();

    // Lower return type
    let return_ty = return_type.map(lower_type).unwrap_or(Ty::Void);

    // Create external function
    let mut func = Function::external(name, ir_params, return_ty);

    // Add effects
    for effect in effects {
        if let Some(ir_effect) = lower_effect(effect) {
            func.effects.push(ir_effect);
        }
    }

    let func_index = ctx.module.functions.len();
    ctx.module.add_function(func);

    func_index
}

/// Lowers an effect type to an IR effect.
fn lower_effect(effect: &ast::Type) -> Option<Effect> {
    match effect {
        ast::Type::Path(path) if !path.segments.is_empty() => {
            let name = &path.segments[0].name;
            match name.as_str() {
                "async" => Some(Effect::Async),
                "io" => Some(Effect::IO),
                "alloc" => Some(Effect::Alloc),
                "diverges" => Some(Effect::Diverges),
                _ => Some(Effect::Raise(Ty::Named(name.clone()))),
            }
        }
        _ => None,
    }
}

/// Gets the name from a pattern.
fn get_pattern_name(pattern: &ast::Pattern) -> &str {
    match pattern {
        ast::Pattern::Ident { name, .. } => &name.name,
        ast::Pattern::Wildcard(_) => "_",
        ast::Pattern::Mut(inner) => get_pattern_name(inner),
        ast::Pattern::Ref { pattern, .. } => get_pattern_name(pattern),
        _ => "_",
    }
}

/// Checks if a pattern is mutable.
fn is_pattern_mutable(pattern: &ast::Pattern) -> bool {
    match pattern {
        ast::Pattern::Ident { mutable, .. } => *mutable,
        ast::Pattern::Mut(_) => true,
        _ => false,
    }
}

/// Lowers a struct definition to a type definition.
pub fn lower_struct_def(ctx: &mut LoweringContext, struct_def: &ast::StructDef) {
    let field_types: Vec<Ty> = struct_def
        .fields
        .iter()
        .map(|f| lower_type(&f.ty))
        .collect();

    let field_names: Vec<String> = struct_def
        .fields
        .iter()
        .map(|f| f.name.name.clone())
        .collect();

    // Register struct info for field access lowering
    let struct_info = crate::context::StructInfo {
        name: struct_def.name.name.clone(),
        field_names,
        field_types: field_types.clone(),
    };
    ctx.register_struct(struct_info);

    let type_def = jet_ir::TypeDef {
        name: struct_def.name.name.clone(),
        kind: jet_ir::TypeKind::Struct(field_types),
    };

    ctx.module.add_type_def(type_def);
}

/// Lowers an enum definition to type definitions.
pub fn lower_enum_def(ctx: &mut LoweringContext, enum_def: &ast::EnumDef) {
    let variants: Vec<(String, Vec<Ty>)> = enum_def
        .variants
        .iter()
        .map(|v| {
            let types = match &v.body {
                ast::VariantBody::Unit => vec![],
                ast::VariantBody::Tuple(types) => types.iter().map(lower_type).collect(),
                ast::VariantBody::Struct(fields) => {
                    fields.iter().map(|f| lower_type(&f.ty)).collect()
                }
                ast::VariantBody::Discriminant(_) => vec![Ty::I64], // Discriminant as i64
            };
            (v.name.name.clone(), types)
        })
        .collect();

    let type_def = jet_ir::TypeDef {
        name: enum_def.name.name.clone(),
        kind: jet_ir::TypeKind::Enum(variants),
    };

    ctx.module.add_type_def(type_def);
}

/// Lowers a constant definition to a global.
pub fn lower_const_def(ctx: &mut LoweringContext, const_def: &ast::ConstDef) {
    let ty = lower_type(&const_def.ty);

    // Lower the constant value
    // For now, create a placeholder global
    let global = jet_ir::Global::new(&const_def.name.name, ty.clone());

    ctx.module.add_global(global);
}

/// Lowers a trait definition (creates vtable structure).
pub fn lower_trait_def(_ctx: &mut LoweringContext, _trait_def: &ast::TraitDef) {
    // Trait definitions don't generate direct IR
    // They are used for type checking and vtable generation
    // In a full implementation, this would create vtable type definitions
}

/// Lowers an impl block (generates method implementations).
pub fn lower_impl_block(ctx: &mut LoweringContext, impl_def: &ast::ImplDef) {
    // Lower each method in the impl block
    for item in &impl_def.items {
        if let ast::ImplItem::Method(func) = item {
            lower_function(ctx, func);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span;
    use jet_parser::ast::{Ident, Pattern, Type};

    fn make_ident(name: &str) -> Ident {
        Ident::new(name, Span::new(0, 0))
    }

    #[test]
    fn test_lower_simple_function() {
        let mut ctx = LoweringContext::new("test");

        // fn add(x: int, y: int) -> int: x + y
        let func = ast::Function {
            public: false,
            name: make_ident("add"),
            generics: vec![],
            params: vec![
                ast::Param {
                    pattern: Pattern::Ident {
                        mutable: false,
                        name: make_ident("x"),
                    },
                    ty: Type::Path(ast::Path::new(vec![make_ident("int")], Span::new(0, 0))),
                },
                ast::Param {
                    pattern: Pattern::Ident {
                        mutable: false,
                        name: make_ident("y"),
                    },
                    ty: Type::Path(ast::Path::new(vec![make_ident("int")], Span::new(0, 0))),
                },
            ],
            return_type: Some(Type::Path(ast::Path::new(
                vec![make_ident("int")],
                Span::new(0, 0),
            ))),
            effects: vec![],
            where_clause: vec![],
            body: ast::Expr::Binary {
                op: ast::BinaryOp::Add,
                left: Box::new(ast::Expr::Variable(make_ident("x"))),
                right: Box::new(ast::Expr::Variable(make_ident("y"))),
            },
            span: Span::new(0, 0),
        };

        let func_idx = lower_function(&mut ctx, &func);

        assert_eq!(func_idx, 0);
        assert_eq!(ctx.module.functions.len(), 1);

        let ir_func = &ctx.module.functions[0];
        assert_eq!(ir_func.name, "add");
        assert_eq!(ir_func.params.len(), 2);
        assert!(!ir_func.is_external);
    }

    #[test]
    fn test_lower_external_function() {
        let mut ctx = LoweringContext::new("test");

        let params = vec![ast::Param {
            pattern: Pattern::Ident {
                mutable: false,
                name: make_ident("x"),
            },
            ty: Type::Path(ast::Path::new(vec![make_ident("int")], Span::new(0, 0))),
        }];

        let func_idx = lower_external_function(
            &mut ctx,
            "external_func",
            &params,
            Some(&Type::Path(ast::Path::new(
                vec![make_ident("int")],
                Span::new(0, 0),
            ))),
            &[],
        );

        assert_eq!(func_idx, 0);
        assert_eq!(ctx.module.functions.len(), 1);

        let ir_func = &ctx.module.functions[0];
        assert_eq!(ir_func.name, "external_func");
        assert!(ir_func.is_external);
    }

    #[test]
    fn test_lower_struct_def() {
        let mut ctx = LoweringContext::new("test");

        let struct_def = ast::StructDef {
            public: true,
            name: make_ident("Point"),
            generics: vec![],
            fields: vec![
                ast::FieldDef {
                    public: false,
                    name: make_ident("x"),
                    ty: Type::Path(ast::Path::new(vec![make_ident("f64")], Span::new(0, 0))),
                },
                ast::FieldDef {
                    public: false,
                    name: make_ident("y"),
                    ty: Type::Path(ast::Path::new(vec![make_ident("f64")], Span::new(0, 0))),
                },
            ],
            span: Span::new(0, 0),
        };

        lower_struct_def(&mut ctx, &struct_def);

        assert_eq!(ctx.module.type_defs.len(), 1);
        assert_eq!(ctx.module.type_defs[0].name, "Point");
    }
}
