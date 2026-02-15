//! Jet IR Lowering
//!
//! This crate provides the lowering phase of the Jet compiler, converting
//! typed AST (TAST) to Jet IR (SSA-based intermediate representation).
//!
//! # Architecture
//!
//! The lowering process consists of several phases:
//!
//! 1. **Module Lowering**: Lower module-level items (functions, types, globals)
//! 2. **Function Lowering**: Create functions with parameters, locals, and blocks
//! 3. **Expression Lowering**: Convert expressions to SSA instructions
//! 4. **Statement Lowering**: Convert statements to control flow
//! 5. **Effect Lowering**: Handle perform/resume operations
//!
//! # Example
//!
//! ```rust
//! use jet_lower::{lower_module, LoweringContext};
//! use jet_parser::ast::Module;
//!
//! fn compile(module: &Module) -> jet_ir::Module {
//!     lower_module(module, "main")
//! }
//! ```

pub mod closure;
pub mod context;
pub mod effect;
pub mod expr;
pub mod function;
pub mod pattern;
pub mod stmt;
pub mod ty;

pub use context::LoweringContext;

use jet_ir::Module as IrModule;
use jet_parser::ast;

/// Lowers an AST module to IR.
///
/// This is the main entry point for the lowering phase. It processes all
/// module-level items and generates the corresponding IR.
pub fn lower_module(module: &ast::Module, name: impl Into<String>) -> IrModule {
    let mut ctx = LoweringContext::new(name);

    // Inject external declarations for builtin functions
    inject_builtins(&mut ctx);

    for item in &module.items {
        lower_module_item(&mut ctx, item);
    }

    ctx.module
}

/// Lowers a single module item.
fn lower_module_item(ctx: &mut LoweringContext, item: &ast::ModuleItem) {
    match item {
        ast::ModuleItem::Function(func) => {
            function::lower_function(ctx, func);
        }
        ast::ModuleItem::Struct(struct_def) => {
            function::lower_struct_def(ctx, struct_def);
        }
        ast::ModuleItem::Enum(enum_def) => {
            function::lower_enum_def(ctx, enum_def);
        }
        ast::ModuleItem::Const(const_def) => {
            function::lower_const_def(ctx, const_def);
        }
        ast::ModuleItem::Trait(trait_def) => {
            function::lower_trait_def(ctx, trait_def);
        }
        ast::ModuleItem::Impl(impl_def) => {
            function::lower_impl_block(ctx, impl_def);
        }
        ast::ModuleItem::Import(_) => {
            // Imports are handled during name resolution
        }
        ast::ModuleItem::TypeAlias(_) => {
            // Type aliases are resolved during type checking
        }
        ast::ModuleItem::Effect(effect_def) => {
            // Effect definitions don't generate IR directly
            // They are used for type checking and effect tracking
            lower_effect_def(ctx, effect_def);
        }
    }
}

/// Lowers an effect definition.
fn lower_effect_def(_ctx: &mut LoweringContext, _effect_def: &ast::EffectDef) {
    // Effect definitions are used for type checking
    // They don't generate direct IR, but may be used for effect metadata
}

/// Injects external declarations for builtin functions.
///
/// This declares the runtime functions (like print, println, etc.) as external
/// so they can be called from compiled Jet code and linked with the runtime library.
///
/// Note: The function names here match what Jet code calls (e.g., "println").
/// The codegen will map these to runtime function names (e.g., "jet_println").
fn inject_builtins(ctx: &mut LoweringContext) {
    use jet_ir::{Function, Param, Ty, ValueId};

    // String type is represented as *i8 (C-style string pointer)
    let str_ty = Ty::Ptr(Box::new(Ty::Int(8)));

    // print(s: string) -> void
    let print_params = vec![Param::new("s", str_ty.clone(), ValueId::new(0))];
    let print_func = Function::external("print", print_params, Ty::Void);
    ctx.module.add_function(print_func);

    // println(s: string) -> void
    let println_params = vec![Param::new("s", str_ty.clone(), ValueId::new(0))];
    let println_func = Function::external("println", println_params, Ty::Void);
    ctx.module.add_function(println_func);

    // eprint(s: string) -> void
    let eprint_params = vec![Param::new("s", str_ty.clone(), ValueId::new(0))];
    let eprint_func = Function::external("eprint", eprint_params, Ty::Void);
    ctx.module.add_function(eprint_func);

    // eprintln(s: string) -> void
    let eprintln_params = vec![Param::new("s", str_ty.clone(), ValueId::new(0))];
    let eprintln_func = Function::external("eprintln", eprintln_params, Ty::Void);
    ctx.module.add_function(eprintln_func);

    // panic(s: string) -> never (represented as Void since it never returns)
    let panic_params = vec![Param::new("s", str_ty, ValueId::new(0))];
    let panic_func = Function::external("panic", panic_params, Ty::Void);
    ctx.module.add_function(panic_func);

    // Helper functions for printing primitives
    // print_int(n: i64) -> void
    let print_int_params = vec![Param::new("n", Ty::Int(64), ValueId::new(0))];
    let print_int_func = Function::external("print_int", print_int_params, Ty::Void);
    ctx.module.add_function(print_int_func);

    // print_float(f: f64) -> void
    let print_float_params = vec![Param::new("f", Ty::Float(64), ValueId::new(0))];
    let print_float_func = Function::external("print_float", print_float_params, Ty::Void);
    ctx.module.add_function(print_float_func);

    // print_bool(b: bool) -> void
    let print_bool_params = vec![Param::new("b", Ty::Bool, ValueId::new(0))];
    let print_bool_func = Function::external("print_bool", print_bool_params, Ty::Void);
    ctx.module.add_function(print_bool_func);
}

/// Lowers an expression from AST to IR.
///
/// This is a convenience function for lowering individual expressions
/// outside of a full module context.
pub fn lower_expression(ctx: &mut LoweringContext, expr: &ast::Expr) -> jet_ir::ValueId {
    expr::lower_expr(ctx, expr)
}

/// Lowers a statement from AST to IR.
///
/// This is a convenience function for lowering individual statements
/// outside of a full module context.
pub fn lower_statement(ctx: &mut LoweringContext, stmt: &ast::Stmt) -> Option<jet_ir::ValueId> {
    stmt::lower_stmt(ctx, stmt)
}

/// Validates that the lowered IR is well-formed.
///
/// Returns Ok(()) if the IR is valid, or an error message otherwise.
pub fn validate_ir(module: &IrModule) -> Result<(), String> {
    module.validate()
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_ir::{Instruction, Ty, ValueId};
    use jet_lexer::Span;
    use jet_parser::ast::{Ident, Literal, Path, Pattern, Type};

    fn make_ident(name: &str) -> Ident {
        Ident::new(name, Span::new(0, 0))
    }

    fn make_path(name: &str) -> Path {
        Path::new(vec![make_ident(name)], Span::new(0, 0))
    }

    #[test]
    fn test_lower_empty_module() {
        let module = ast::Module::new(Span::new(0, 0));
        let ir = lower_module(&module, "test");

        assert_eq!(ir.name, "test");
        // Empty module still has 8 injected builtin functions
        assert_eq!(ir.functions.len(), 8);
        assert!(ir.type_defs.is_empty());
        assert!(ir.globals.is_empty());
    }

    #[test]
    fn test_lower_simple_function() {
        let mut module = ast::Module::new(Span::new(0, 0));

        let func = ast::Function {
            public: true,
            name: make_ident("main"),
            generics: vec![],
            params: vec![],
            return_type: Some(Type::Path(make_path("int"))),
            effects: vec![],
            where_clause: vec![],
            body: ast::Expr::Literal(Literal::Integer(0)),
            span: Span::new(0, 0),
        };

        module.items.push(ast::ModuleItem::Function(func));

        let ir = lower_module(&module, "test");

        // 8 builtin functions + 1 user function = 9 total
        assert_eq!(ir.functions.len(), 9);
        // User function is at the end after builtins
        assert_eq!(ir.functions[8].name, "main");
        assert!(ir.functions[8].is_exported);
    }

    #[test]
    fn test_lower_function_with_params() {
        let mut module = ast::Module::new(Span::new(0, 0));

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
                    ty: Type::Path(make_path("int")),
                },
                ast::Param {
                    pattern: Pattern::Ident {
                        mutable: false,
                        name: make_ident("y"),
                    },
                    ty: Type::Path(make_path("int")),
                },
            ],
            return_type: Some(Type::Path(make_path("int"))),
            effects: vec![],
            where_clause: vec![],
            body: ast::Expr::Binary {
                op: ast::BinaryOp::Add,
                left: Box::new(ast::Expr::Variable(make_ident("x"))),
                right: Box::new(ast::Expr::Variable(make_ident("y"))),
            },
            span: Span::new(0, 0),
        };

        module.items.push(ast::ModuleItem::Function(func));

        let ir = lower_module(&module, "test");

        // 8 builtin functions + 1 user function = 9 total
        assert_eq!(ir.functions.len(), 9);
        // User function is at the end after builtins
        assert_eq!(ir.functions[8].params.len(), 2);
    }

    #[test]
    fn test_lower_struct() {
        let mut module = ast::Module::new(Span::new(0, 0));

        let struct_def = ast::StructDef {
            public: true,
            name: make_ident("Point"),
            generics: vec![],
            fields: vec![
                ast::FieldDef {
                    public: false,
                    name: make_ident("x"),
                    ty: Type::Path(make_path("f64")),
                },
                ast::FieldDef {
                    public: false,
                    name: make_ident("y"),
                    ty: Type::Path(make_path("f64")),
                },
            ],
            span: Span::new(0, 0),
        };

        module.items.push(ast::ModuleItem::Struct(struct_def));

        let ir = lower_module(&module, "test");

        assert_eq!(ir.type_defs.len(), 1);
        assert_eq!(ir.type_defs[0].name, "Point");
    }

    #[test]
    fn test_validate_ir() {
        let module = ast::Module::new(Span::new(0, 0));
        let ir = lower_module(&module, "test");

        assert!(validate_ir(&ir).is_ok());
    }

    #[test]
    fn test_lower_expression_convenience() {
        let mut ctx = LoweringContext::new("test");
        let func = jet_ir::Function::new("test", vec![], Ty::I32);
        ctx.module.add_function(func);
        ctx.start_function(0);

        let entry = ctx.create_block("entry");
        ctx.set_current_block(entry);

        let expr = ast::Expr::Literal(Literal::Integer(42));
        let value = lower_expression(&mut ctx, &expr);

        assert_eq!(value, ValueId::new(0));
    }

    #[test]
    fn test_lower_if_expression() {
        let mut ctx = LoweringContext::new("test");
        let func = jet_ir::Function::new("test", vec![], Ty::I32);
        ctx.module.add_function(func);
        ctx.start_function(0);

        let entry = ctx.create_block("entry");
        ctx.set_current_block(entry);

        let expr = ast::Expr::If {
            cond: Box::new(ast::Expr::Literal(Literal::Bool(true))),
            then_branch: Box::new(ast::Expr::Literal(Literal::Integer(1))),
            else_branch: Some(Box::new(ast::Expr::Literal(Literal::Integer(0)))),
        };

        let _value = lower_expression(&mut ctx, &expr);

        // Check that we have the expected blocks
        let func = &ctx.module.functions[0];
        assert!(func.blocks.len() >= 4); // entry, then, else, merge
    }

    #[test]
    fn test_lower_while_loop() {
        let mut ctx = LoweringContext::new("test");
        let func = jet_ir::Function::new("test", vec![], Ty::Void);
        ctx.module.add_function(func);
        ctx.start_function(0);

        let entry = ctx.create_block("entry");
        ctx.set_current_block(entry);

        let expr = ast::Expr::While {
            label: None,
            cond: Box::new(ast::Expr::Literal(Literal::Bool(true))),
            body: Box::new(ast::Expr::Block(ast::Block {
                stmts: vec![],
                expr: None,
                span: Span::new(0, 0),
            })),
        };

        let _value = lower_expression(&mut ctx, &expr);

        // Check that we have the expected blocks
        let func = &ctx.module.functions[0];
        assert!(func.blocks.len() >= 3); // entry/cond, body, exit
    }

    #[test]
    fn test_ssa_form_generation() {
        let mut ctx = LoweringContext::new("test");
        let func = jet_ir::Function::new("test", vec![], Ty::I32);
        ctx.module.add_function(func);
        ctx.start_function(0);

        let entry = ctx.create_block("entry");
        ctx.set_current_block(entry);

        // Create a variable and modify it
        let x = ctx.new_value();
        ctx.emit(Instruction::Const {
            result: x,
            value: jet_ir::ConstantValue::Int(0, Ty::I32),
        });

        let y = ctx.new_value();
        ctx.emit(Instruction::Const {
            result: y,
            value: jet_ir::ConstantValue::Int(1, Ty::I32),
        });

        // Each value should have a unique ID
        assert_ne!(x, y);
        assert_eq!(x, ValueId::new(0));
        assert_eq!(y, ValueId::new(1));
    }
}
