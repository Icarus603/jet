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
use jet_typeck::{TypeContext, TypedModule};

/// Lowers a typed AST module to IR.
///
/// This is the main entry point for the lowering phase. It processes all
/// module-level items and generates the corresponding IR.
///
/// # Arguments
///
/// * `module` - The typed module (TAST) containing full type information
/// * `type_context` - The type context containing type definitions and mappings
/// * `name` - The name for the generated IR module
pub fn lower_module(
    module: &TypedModule,
    type_context: &TypeContext,
    name: impl Into<String>,
) -> IrModule {
    let mut ctx = LoweringContext::new(name, type_context);

    // Inject external declarations for builtin functions
    inject_builtins(&mut ctx);

    for item in &module.items {
        lower_module_item(&mut ctx, item);
    }

    ctx.module
}

/// Lowers a single typed module item.
fn lower_module_item(ctx: &mut LoweringContext, item: &TypedModuleItem) {
    match item {
        TypedModuleItem::Function(func) => {
            function::lower_typed_function(ctx, func);
        }
    }
}

use jet_typeck::TypedModuleItem;

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

/// Lowers a typed expression to IR.
///
/// This is a convenience function for lowering individual expressions
/// outside of a full module context.
pub fn lower_typed_expression(
    ctx: &mut LoweringContext,
    expr: &jet_typeck::TypedExpr,
) -> jet_ir::ValueId {
    expr::lower_typed_expr(ctx, expr)
}

/// Lowers a typed statement to IR.
///
/// This is a convenience function for lowering individual statements
/// outside of a full module context.
pub fn lower_typed_statement(
    ctx: &mut LoweringContext,
    stmt: &jet_typeck::TypedStmt,
) -> Option<jet_ir::ValueId> {
    stmt::lower_typed_stmt(ctx, stmt)
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
    use jet_diagnostics::Span;

    #[test]
    fn test_lower_empty_module() {
        let tcx = TypeContext::new();
        let module = TypedModule {
            items: vec![],
            span: Span::new(0, 0),
        };
        let ir = lower_module(&module, &tcx, "test");

        assert_eq!(ir.name, "test");
        // Empty module still has 8 injected builtin functions
        assert_eq!(ir.functions.len(), 8);
        assert!(ir.type_defs.is_empty());
        assert!(ir.globals.is_empty());
    }

    #[test]
    fn test_validate_ir() {
        let tcx = TypeContext::new();
        let module = TypedModule {
            items: vec![],
            span: Span::new(0, 0),
        };
        let ir = lower_module(&module, &tcx, "test");

        assert!(validate_ir(&ir).is_ok());
    }

    #[test]
    fn test_ssa_form_generation() {
        let tcx = TypeContext::new();
        let mut ctx = LoweringContext::new("test", &tcx);
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
