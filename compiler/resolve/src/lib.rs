//! Jet Name Resolver (jet-resolve)
//!
//! This crate provides name resolution for the Jet programming language.
//! It takes an AST from the parser and resolves all identifiers to their
//! definitions, producing an AST with resolved names and a symbol table.
//!
//! # Usage
//!
//! ```
//! use jet_resolve::{Resolver, ModuleId};
//! use jet_parser::ast::Module;
//!
//! let mut resolver = Resolver::new(ModuleId::root());
//! let mut module = Module::new(Default::default());
//! let result = resolver.resolve_module(&mut module);
//! ```

pub mod def_id;
pub mod error;
pub mod import;
pub mod resolver;
pub mod symbol;

// Re-export main types
pub use def_id::{DefId, DefIdGenerator, DefIdTable, ModuleId, ModuleIdGenerator};
pub use error::{ErrorHelpers, ResolutionError, ResolutionErrors};
pub use import::ImportResolver;
pub use resolver::{ResolutionResult, Resolver};
pub use symbol::{Binding, BindingKind, Import, ModuleScope, Scope, ScopeKind, SymbolTable};

use jet_diagnostics::Diagnostic;
use jet_parser::ast::Module;

/// Resolves names in a module.
///
/// This is a convenience function that creates a resolver and runs name resolution.
///
/// # Arguments
///
/// * `module` - The AST module to resolve
///
/// # Returns
///
/// Returns `Ok(())` if resolution succeeded, or `Err(Vec<Diagnostic>)` if there were errors.
///
/// # Example
///
/// ```
/// use jet_resolve::resolve;
/// use jet_parser::ast::Module;
///
/// let mut module = Module::new(Default::default());
/// match resolve(&mut module) {
///     Ok(()) => println!("Resolution successful!"),
///     Err(diagnostics) => eprintln!("Resolution failed: {:?}", diagnostics),
/// }
/// ```
pub fn resolve(module: &mut Module) -> Result<(), Vec<Diagnostic>> {
    let mut resolver = Resolver::new(ModuleId::root());
    let result = resolver.resolve_module(module);
    result.into_result_with_imports(resolver.import_diagnostics)
}

/// Resolves names in a module with a specific root module ID.
///
/// This is useful when resolving multiple modules in a crate.
pub fn resolve_with_module_id(
    module: &mut Module,
    module_id: ModuleId,
) -> Result<(), Vec<Diagnostic>> {
    let mut resolver = Resolver::new(module_id);
    let result = resolver.resolve_module(module);
    result.into_result_with_imports(resolver.import_diagnostics)
}

#[cfg(test)]
mod integration_tests {
    use super::*;
    use jet_lexer::Span;
    use jet_parser::ast::{
        Expr, Function, Ident, Literal, Module, ModuleItem, Pattern, Stmt, Type,
    };

    fn make_span() -> Span {
        Span::new(0, 0)
    }

    fn make_ident(name: &str) -> Ident {
        Ident::new(name, make_span())
    }

    fn make_simple_function(name: &str, body: Expr) -> Function {
        Function {
            public: true,
            name: make_ident(name),
            generics: vec![],
            params: vec![],
            return_type: None,
            effects: vec![],
            where_clause: vec![],
            body,
            span: make_span(),
        }
    }

    #[test]
    fn test_resolve_simple_function() {
        let mut module = Module::new(make_span());
        module.items.push(ModuleItem::Function(make_simple_function(
            "main",
            Expr::Literal(Literal::Unit),
        )));

        let result = resolve(&mut module);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_variable_reference() {
        let mut module = Module::new(make_span());

        // Create a function that references an undefined variable
        let body = Expr::Variable(make_ident("undefined_var"));
        module
            .items
            .push(ModuleItem::Function(make_simple_function("test", body)));

        let result = resolve(&mut module);
        assert!(result.is_err());
    }

    #[test]
    fn test_resolve_let_binding() {
        let mut module = Module::new(make_span());

        // Create a function with a let binding
        let body = Expr::Block(jet_parser::ast::Block {
            stmts: vec![Stmt::Let {
                pattern: Pattern::Ident {
                    mutable: false,
                    name: make_ident("x"),
                },
                ty: None,
                value: Box::new(Expr::Literal(Literal::Integer(42))),
            }],
            expr: Some(Box::new(Expr::Variable(make_ident("x")))),
            span: make_span(),
        });

        module
            .items
            .push(ModuleItem::Function(make_simple_function("test", body)));

        let result = resolve(&mut module);
        assert!(result.is_ok());
    }

    #[test]
    fn test_duplicate_function_definition() {
        let mut module = Module::new(make_span());

        // Add two functions with the same name
        module.items.push(ModuleItem::Function(make_simple_function(
            "foo",
            Expr::Literal(Literal::Unit),
        )));
        module.items.push(ModuleItem::Function(make_simple_function(
            "foo",
            Expr::Literal(Literal::Unit),
        )));

        let result = resolve(&mut module);
        assert!(result.is_err());
    }

    #[test]
    fn test_resolve_function_call() {
        let mut module = Module::new(make_span());

        // Add a function
        module.items.push(ModuleItem::Function(make_simple_function(
            "helper",
            Expr::Literal(Literal::Integer(1)),
        )));

        // Add another function that calls it
        let body = Expr::Call {
            func: Box::new(Expr::Variable(make_ident("helper"))),
            args: vec![],
        };
        module
            .items
            .push(ModuleItem::Function(make_simple_function("main", body)));

        let result = resolve(&mut module);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_type_reference() {
        let mut module = Module::new(make_span());

        // Add a struct
        module
            .items
            .push(ModuleItem::Struct(jet_parser::ast::StructDef {
                public: true,
                name: make_ident("MyStruct"),
                generics: vec![],
                fields: vec![],
                span: make_span(),
            }));

        // Add a function that uses the struct type
        let func = Function {
            public: true,
            name: make_ident("make_struct"),
            generics: vec![],
            params: vec![],
            return_type: Some(Type::Path(jet_parser::ast::Path::single(make_ident(
                "MyStruct",
            )))),
            effects: vec![],
            where_clause: vec![],
            body: Expr::Literal(Literal::Unit),
            span: make_span(),
        };
        module.items.push(ModuleItem::Function(func));

        let result = resolve(&mut module);
        assert!(result.is_ok());
    }
}
