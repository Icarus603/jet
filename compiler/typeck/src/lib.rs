//! Jet Type Checker (jet-typeck)
//!
//! Hindley-Milner type inference for the Jet programming language.
//!
//! # Overview
//!
//! This crate implements the type checking phase of the Jet compiler:
//!
//! 1. **Type Representation** (`types`): Core type system definitions including
//!    primitive types, composite types, and type variables for inference.
//!
//! 2. **Unification** (`unify`): The unification algorithm that solves type
//!    constraints by finding the most general unifier.
//!
//! 3. **Type Checker** (`checker`): Hindley-Milner type inference with
//!    let-generalization for polymorphism and effect tracking.
//!
//! # Example
//!
//! ```rust,ignore
//! use jet_typeck::{TypeContext, TypeChecker};
//! use jet_parser::parse_module;
//!
//! fn type_check(source: &str) -> Result<TypedModule, Vec<Diagnostic>> {
//!     let ast = parse_module(source)?;
//!     let mut tcx = TypeContext::new();
//!     let mut checker = TypeChecker::new(&mut tcx);
//!     checker.infer_module(&ast)
//! }
//! ```

pub mod associated_types;
pub mod checker;
pub mod constraints;
pub mod patterns;
pub mod traits;
pub mod types;
pub mod unify;

// Re-export main types
pub use associated_types::{
    AssociatedType, AssociatedTypeContext, AssociatedTypeResolver, Projection, ProjectionError,
};
pub use checker::{
    Scheme, TypeChecker, TypedBlock, TypedExpr, TypedExprKind, TypedFunction, TypedMatchArm,
    TypedModule, TypedModuleItem, TypedParam, TypedStmt,
};
pub use constraints::{
    Constraint, ConstraintSet, ConstraintSolver, GenericContext, GenericParamInfo,
    WhereClauseChecker,
};
pub use patterns::{Binding, PatternConstructor, PatternInference, PatternInferrer, PatternMatrix};
pub use traits::{
    Impl, ImplItem, MethodCandidate, MethodSource, Resolution, ResolutionError, Trait, TraitBound,
    TraitContext, TraitItemDef, TraitResolver,
};
pub use types::{
    DefId, EffectInstance, EffectSet, FloatSize, IntSize, Level, Mutability, TypeContext, TypeId,
    TypeKind, TypeVar, TypeVarKind, VarId,
};
pub use unify::{Unifier, UnifyError};

use jet_diagnostics::Diagnostic;
use jet_parser::ast::Module;

/// Type check a module and return the typed AST or diagnostics.
///
/// This is a convenience function for the common case of type checking
/// a single module.
pub fn type_check_module(module: &Module) -> Result<TypedModule, Vec<Diagnostic>> {
    let mut tcx = TypeContext::new();
    let mut checker = TypeChecker::new(&mut tcx);

    match checker.infer_module(module) {
        Ok(typed) => {
            let diagnostics = checker.take_diagnostics();
            if diagnostics.is_empty() {
                Ok(typed)
            } else {
                Err(diagnostics)
            }
        }
        Err(diag) => {
            let mut diagnostics = checker.take_diagnostics();
            diagnostics.push(diag);
            Err(diagnostics)
        }
    }
}

/// Type context with a type checker attached.
///
/// This is useful when you need to perform multiple type checking
/// operations while preserving the type context.
pub struct TypeCheckSession {
    tcx: TypeContext,
    diagnostics: Vec<Diagnostic>,
}

impl TypeCheckSession {
    /// Create a new type check session.
    pub fn new() -> Self {
        Self {
            tcx: TypeContext::new(),
            diagnostics: Vec::new(),
        }
    }

    /// Type check a module.
    #[allow(clippy::result_unit_err)]
    pub fn check_module(&mut self, module: &Module) -> Result<TypedModule, ()> {
        let mut checker = TypeChecker::new(&mut self.tcx);

        match checker.infer_module(module) {
            Ok(typed) => {
                self.diagnostics.extend(checker.take_diagnostics());
                Ok(typed)
            }
            Err(diag) => {
                self.diagnostics.push(diag);
                self.diagnostics.extend(checker.take_diagnostics());
                Err(())
            }
        }
    }

    /// Get the collected diagnostics.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Take the diagnostics (clears the internal buffer).
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }

    /// Get a reference to the type context.
    pub fn type_context(&self) -> &TypeContext {
        &self.tcx
    }

    /// Format a type ID as a string.
    pub fn type_to_string(&self, id: TypeId) -> String {
        self.tcx.type_to_string(id)
    }
}

impl Default for TypeCheckSession {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;
    use jet_lexer::Span;
    use jet_parser::ast::Module;

    fn create_test_module() -> Module {
        Module {
            items: vec![],
            span: Span::default(),
        }
    }

    #[test]
    fn test_empty_module() {
        let module = create_test_module();
        let result = type_check_module(&module);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_check_session() {
        let mut session = TypeCheckSession::new();
        let module = create_test_module();

        let result = session.check_module(&module);
        assert!(result.is_ok());
        assert!(session.diagnostics().is_empty());
    }
}
