//! Pattern type inference for Jet
//!
//! This module handles type inference and checking for all pattern types:
//! - Identifier patterns (let x = ...)
//! - Tuple patterns (let (a, b) = ...)
//! - Struct patterns (let Point { x, y } = ...)
//! - Enum patterns (match x: Some(v) => ...)
//! - Array patterns
//! - Wildcard, rest, and bind patterns

use crate::types::{Mutability, TypeContext, TypeId, TypeKind};
use crate::unify::Unifier;
use jet_diagnostics::{Diagnostic, ErrorCode, Span};
use jet_parser::ast::{Literal, Pattern};

/// Information about a pattern binding
#[derive(Debug, Clone)]
pub struct Binding {
    pub name: String,
    pub ty: TypeId,
    pub mutable: bool,
    pub span: Span,
}

/// Result of pattern type inference
#[derive(Debug, Clone)]
pub struct PatternInference {
    /// The type of the value being matched
    pub matched_ty: TypeId,
    /// Bindings introduced by the pattern
    pub bindings: Vec<Binding>,
    /// Any diagnostics generated
    pub diagnostics: Vec<Diagnostic>,
}

impl PatternInference {
    /// Create a new pattern inference result
    pub fn new(matched_ty: TypeId) -> Self {
        Self {
            matched_ty,
            bindings: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    /// Add a binding
    pub fn add_binding(&mut self, name: String, ty: TypeId, mutable: bool, span: Span) {
        self.bindings.push(Binding {
            name,
            ty,
            mutable,
            span,
        });
    }

    /// Add a diagnostic
    pub fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag);
    }

    /// Check if there were any errors
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.is_error())
    }
}

/// Pattern type inferrer
pub struct PatternInferrer<'tcx> {
    tcx: &'tcx mut TypeContext,
}

impl<'tcx> PatternInferrer<'tcx> {
    /// Create a new pattern inferrer
    pub fn new(tcx: &'tcx mut TypeContext) -> Self {
        Self { tcx }
    }

    /// Infer the type of a pattern and collect bindings
    pub fn infer_pattern(
        &mut self,
        pattern: &Pattern,
        expected_ty: TypeId,
        span: Span,
    ) -> PatternInference {
        let mut result = PatternInference::new(expected_ty);
        self.infer_pattern_internal(pattern, expected_ty, &mut result, span);
        result
    }

    fn infer_pattern_internal(
        &mut self,
        pattern: &Pattern,
        expected_ty: TypeId,
        result: &mut PatternInference,
        span: Span,
    ) {
        match pattern {
            Pattern::Wildcard(_) => {
                // Wildcard matches any type, no bindings
            }

            Pattern::Ident { mutable, name } => {
                // Identifier pattern binds the value to a name
                result.add_binding(name.name.clone(), expected_ty, *mutable, span);
            }

            Pattern::Literal(lit) => {
                // Literal pattern matches a specific value
                let lit_ty = self.type_of_literal(lit);
                let mut unifier = Unifier::new(self.tcx);
                if let Err(e) = unifier.unify(expected_ty, lit_ty, span) {
                    result.add_diagnostic(e.to_diagnostic(self.tcx));
                }
            }

            Pattern::Tuple(patterns) => {
                // Tuple pattern: (p1, p2, ..., pn) matches (T1, T2, ..., Tn)
                let elem_tys: Vec<_> = patterns.iter().map(|_| self.tcx.fresh_var(0)).collect();
                let tuple_ty = self.tcx.mk_tuple(elem_tys.clone());

                let mut unifier = Unifier::new(self.tcx);
                if let Err(e) = unifier.unify(expected_ty, tuple_ty, span) {
                    result.add_diagnostic(e.to_diagnostic(self.tcx));
                }

                // Infer each sub-pattern
                for (pat, elem_ty) in patterns.iter().zip(elem_tys.iter()) {
                    self.infer_pattern_internal(pat, *elem_ty, result, span);
                }
            }

            Pattern::Struct {
                path: _,
                fields,
                rest,
            } => {
                // Struct pattern: Struct { field1: pat1, field2: pat2, .. }
                // For now, we create a fresh type variable for the struct
                // In a full implementation, we'd look up the struct definition
                let struct_ty = self.tcx.fresh_var(0);

                let mut unifier = Unifier::new(self.tcx);
                if let Err(e) = unifier.unify(expected_ty, struct_ty, span) {
                    result.add_diagnostic(e.to_diagnostic(self.tcx));
                }

                // Process field patterns
                for field in fields {
                    let field_ty = self.tcx.fresh_var(0);
                    if let Some(ref pat) = field.pattern {
                        self.infer_pattern_internal(pat, field_ty, result, span);
                    } else {
                        // Shorthand pattern `field` means `field: field`
                        result.add_binding(field.name.name.clone(), field_ty, false, span);
                    }
                }

                // Check that all fields are covered if no rest pattern
                if !rest {
                    // In a full implementation, verify all fields are present
                }
            }

            Pattern::Enum {
                path: _,
                variant: _,
                inner,
            } => {
                // Enum pattern: Enum::Variant(inner)
                // For now, create a fresh type variable
                let enum_ty = self.tcx.fresh_var(0);

                let mut unifier = Unifier::new(self.tcx);
                if let Err(e) = unifier.unify(expected_ty, enum_ty, span) {
                    result.add_diagnostic(e.to_diagnostic(self.tcx));
                }

                // Process inner pattern if present
                if let Some(ref inner_pat) = inner {
                    let inner_ty = self.tcx.fresh_var(0);
                    self.infer_pattern_internal(inner_pat, inner_ty, result, span);
                }
            }

            Pattern::Array(patterns) => {
                // Array pattern: [p1, p2, ..., pn] matches [T; n]
                let elem_ty = self.tcx.fresh_var(0);
                let array_ty = self.tcx.mk_array(elem_ty, patterns.len());

                let mut unifier = Unifier::new(self.tcx);
                if let Err(e) = unifier.unify(expected_ty, array_ty, span) {
                    result.add_diagnostic(e.to_diagnostic(self.tcx));
                }

                // Each element pattern must match the element type
                for pat in patterns {
                    self.infer_pattern_internal(pat, elem_ty, result, span);
                }
            }

            Pattern::Rest(name) => {
                // Rest pattern `..` or `..rest` in struct/array patterns
                // Binds remaining elements to a name if provided
                if let Some(ref ident) = name {
                    // The type depends on context (slice for arrays, etc.)
                    let rest_ty = self.tcx.fresh_var(0);
                    result.add_binding(ident.name.clone(), rest_ty, false, span);
                }
            }

            Pattern::Or(left, right) => {
                // Or pattern: p1 | p2
                // Both patterns must have the same type
                self.infer_pattern_internal(left, expected_ty, result, span);
                self.infer_pattern_internal(right, expected_ty, result, span);
            }

            Pattern::Bind { name, pattern } => {
                // Bind pattern: name @ pattern
                // First process the inner pattern
                self.infer_pattern_internal(pattern, expected_ty, result, span);
                // Then bind the name to the matched value
                result.add_binding(name.name.clone(), expected_ty, false, span);
            }

            Pattern::Mut(inner) => {
                // Mut pattern wrapper: mut pattern
                // The inner pattern's bindings become mutable
                self.infer_pattern_internal(inner, expected_ty, result, span);
                // Mark the last binding as mutable
                if let Some(last) = result.bindings.last_mut() {
                    last.mutable = true;
                }
            }

            Pattern::Ref { mutable, pattern } => {
                // Ref pattern: ref pattern or ref mut pattern
                // Expected type should be a reference
                let inner_ty = self.tcx.fresh_var(0);
                let mutability = if *mutable {
                    Mutability::Mutable
                } else {
                    Mutability::Immutable
                };
                let ref_ty = self.tcx.mk_ref(inner_ty, mutability);

                let mut unifier = Unifier::new(self.tcx);
                if let Err(e) = unifier.unify(expected_ty, ref_ty, span) {
                    result.add_diagnostic(e.to_diagnostic(self.tcx));
                }

                self.infer_pattern_internal(pattern, inner_ty, result, span);
            }
        }
    }

    /// Get the type of a literal
    fn type_of_literal(&self, lit: &Literal) -> TypeId {
        use jet_parser::ast::Literal;
        match lit {
            Literal::Integer(_) => TypeId::INT,
            Literal::Float(_) => TypeId::FLOAT,
            Literal::String(_) => TypeId::STRING,
            Literal::Char(_) => TypeId::CHAR,
            Literal::Bool(_) => TypeId::BOOL,
            Literal::Unit => TypeId::UNIT,
        }
    }

    /// Check pattern exhaustiveness
    pub fn check_exhaustiveness(
        &self,
        patterns: &[Pattern],
        matched_ty: TypeId,
        span: Span,
    ) -> Result<(), Box<Diagnostic>> {
        // For now, a simplified check
        // Full exhaustiveness checking requires:
        // - Constructor analysis
        // - Reachability checking
        // - Missing pattern detection

        if patterns.is_empty() {
            return Err(Box::new(
                Diagnostic::error("non-exhaustive patterns: no patterns defined", span)
                    .with_error_code(ErrorCode::NonExhaustivePatterns),
            ));
        }

        // Check for wildcard pattern (always exhaustive)
        let has_wildcard = patterns.iter().any(|p| matches!(p, Pattern::Wildcard(_)));

        if !has_wildcard {
            // For enums, check if all variants are covered
            if let TypeKind::Enum(_def_id) = self.tcx.type_kind(matched_ty) {
                // In a full implementation, check if all variants are covered
                // For now, just warn about potential non-exhaustiveness
            }
        }

        Ok(())
    }
}

/// Pattern matrix for exhaustiveness and redundancy checking
#[derive(Debug, Default)]
pub struct PatternMatrix {
    rows: Vec<Vec<PatternConstructor>>,
}

impl PatternMatrix {
    /// Create a new empty pattern matrix
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a row to the matrix
    pub fn add_row(&mut self, patterns: Vec<PatternConstructor>) {
        self.rows.push(patterns);
    }

    /// Check if the matrix is exhaustive
    pub fn is_exhaustive(&self) -> bool {
        // Simplified check - full algorithm is complex
        // See "GADTs meet their match" by Karachalias et al.
        self.rows.iter().any(|row| {
            row.iter()
                .all(|p| matches!(p, PatternConstructor::Wildcard))
        })
    }
}

/// A pattern constructor for exhaustiveness checking
#[derive(Debug, Clone)]
pub enum PatternConstructor {
    /// Wildcard pattern
    Wildcard,
    /// Literal constructor
    Literal(Literal),
    /// Constructor with arguments (e.g., Some(_), Cons(_, _))
    Constructor {
        name: String,
        args: Vec<PatternConstructor>,
    },
    /// Or pattern
    Or(Box<PatternConstructor>, Box<PatternConstructor>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span as LexerSpan;
    use jet_parser::ast::Ident;

    fn test_span() -> Span {
        Span::default()
    }

    #[test]
    fn test_wildcard_pattern() {
        let mut tcx = TypeContext::new();
        let mut inferrer = PatternInferrer::new(&mut tcx);

        let pattern = Pattern::Wildcard(LexerSpan::default());
        let expected_ty = TypeId::INT;

        let result = inferrer.infer_pattern(&pattern, expected_ty, test_span());

        assert!(result.bindings.is_empty());
        assert!(!result.has_errors());
    }

    #[test]
    fn test_identifier_pattern() {
        let mut tcx = TypeContext::new();
        let mut inferrer = PatternInferrer::new(&mut tcx);

        let pattern = Pattern::Ident {
            mutable: false,
            name: Ident::new("x", LexerSpan::default()),
        };
        let expected_ty = TypeId::INT;

        let result = inferrer.infer_pattern(&pattern, expected_ty, test_span());

        assert_eq!(result.bindings.len(), 1);
        assert_eq!(result.bindings[0].name, "x");
        assert_eq!(result.bindings[0].ty, TypeId::INT);
        assert!(!result.bindings[0].mutable);
    }

    #[test]
    fn test_tuple_pattern() {
        let mut tcx = TypeContext::new();
        let expected_ty = tcx.mk_tuple(vec![TypeId::INT, TypeId::BOOL]);

        let mut inferrer = PatternInferrer::new(&mut tcx);

        let pattern = Pattern::Tuple(vec![
            Pattern::Ident {
                mutable: false,
                name: Ident::new("a", LexerSpan::default()),
            },
            Pattern::Ident {
                mutable: false,
                name: Ident::new("b", LexerSpan::default()),
            },
        ]);

        let result = inferrer.infer_pattern(&pattern, expected_ty, test_span());

        assert_eq!(result.bindings.len(), 2);
        assert_eq!(result.bindings[0].name, "a");
        assert_eq!(result.bindings[1].name, "b");
    }

    #[test]
    fn test_literal_pattern() {
        let mut tcx = TypeContext::new();
        let mut inferrer = PatternInferrer::new(&mut tcx);

        let pattern = Pattern::Literal(Literal::Integer(42));
        let expected_ty = TypeId::INT;

        let result = inferrer.infer_pattern(&pattern, expected_ty, test_span());

        assert!(result.bindings.is_empty());
        assert!(!result.has_errors());
    }

    #[test]
    fn test_array_pattern() {
        let mut tcx = TypeContext::new();
        let expected_ty = tcx.mk_array(TypeId::INT, 2);

        let mut inferrer = PatternInferrer::new(&mut tcx);

        let pattern = Pattern::Array(vec![
            Pattern::Ident {
                mutable: false,
                name: Ident::new("x", LexerSpan::default()),
            },
            Pattern::Ident {
                mutable: false,
                name: Ident::new("y", LexerSpan::default()),
            },
        ]);

        let result = inferrer.infer_pattern(&pattern, expected_ty, test_span());

        assert_eq!(result.bindings.len(), 2);
        assert_eq!(result.bindings[0].name, "x");
        assert_eq!(result.bindings[1].name, "y");
    }

    #[test]
    fn test_bind_pattern() {
        let mut tcx = TypeContext::new();
        let expected_ty = tcx.mk_tuple(vec![TypeId::INT, TypeId::INT]);

        let mut inferrer = PatternInferrer::new(&mut tcx);

        let pattern = Pattern::Bind {
            name: Ident::new("whole", LexerSpan::default()),
            pattern: Box::new(Pattern::Tuple(vec![
                Pattern::Ident {
                    mutable: false,
                    name: Ident::new("a", LexerSpan::default()),
                },
                Pattern::Ident {
                    mutable: false,
                    name: Ident::new("b", LexerSpan::default()),
                },
            ])),
        };

        let result = inferrer.infer_pattern(&pattern, expected_ty, test_span());

        assert_eq!(result.bindings.len(), 3);
        assert_eq!(result.bindings[0].name, "a");
        assert_eq!(result.bindings[1].name, "b");
        assert_eq!(result.bindings[2].name, "whole");
    }
}
