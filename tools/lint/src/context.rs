//! Lint context
//!
//! Provides access to source code, AST, and accumulates lint violations.

use jet_lexer::Span;

use crate::{LintLevel, LintViolation};

/// Context passed to lint passes during checking
#[derive(Debug)]
pub struct LintContext {
    /// Source code being linted
    source: String,
    /// Filename (if available)
    filename: Option<String>,
    /// Accumulated violations
    pub violations: Vec<LintViolation>,
    /// Current scope information
    scope_stack: Vec<Scope>,
}

/// Information about a scope
#[derive(Debug, Clone)]
struct Scope {
    /// Kind of scope
    #[allow(dead_code)]
    kind: ScopeKind,
    /// Variables defined in this scope
    variables: Vec<VariableInfo>,
    /// Whether this scope can return/break
    is_terminal: bool,
}

/// Kind of scope
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Module,
    Function,
    Block,
    Loop,
    Match,
}

/// Information about a variable
#[derive(Debug, Clone)]
struct VariableInfo {
    name: String,
    mutable: bool,
    used: bool,
    span: Span,
}

impl LintContext {
    /// Create a new lint context
    pub fn new(source: impl Into<String>, filename: Option<String>) -> Self {
        Self {
            source: source.into(),
            filename,
            violations: Vec::new(),
            scope_stack: vec![Scope {
                kind: ScopeKind::Module,
                variables: Vec::new(),
                is_terminal: false,
            }],
        }
    }

    /// Get the source code
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Get the filename
    pub fn filename(&self) -> Option<&str> {
        self.filename.as_deref()
    }

    /// Report a lint violation
    pub fn report(&mut self, violation: LintViolation) {
        self.violations.push(violation);
    }

    /// Report a violation with the given level
    pub fn report_with_level(
        &mut self,
        lint_name: impl Into<String>,
        level: LintLevel,
        message: impl Into<String>,
        span: Span,
    ) {
        self.report(LintViolation::new(lint_name, level, message, span));
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self, kind: ScopeKind) {
        self.scope_stack.push(Scope {
            kind,
            variables: Vec::new(),
            is_terminal: false,
        });
    }

    /// Leave the current scope
    pub fn leave_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }

    /// Check if we're currently in a terminal scope (after return/break)
    pub fn is_terminal(&self) -> bool {
        self.scope_stack
            .last()
            .map(|s| s.is_terminal)
            .unwrap_or(false)
    }

    /// Mark the current scope as terminal
    pub fn mark_terminal(&mut self) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.is_terminal = true;
        }
    }

    /// Add a variable to the current scope
    pub fn add_variable(&mut self, name: impl Into<String>, mutable: bool, span: Span) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.variables.push(VariableInfo {
                name: name.into(),
                mutable,
                used: false,
                span,
            });
        }
    }

    /// Mark a variable as used
    pub fn mark_variable_used(&mut self, name: &str) {
        for scope in self.scope_stack.iter_mut().rev() {
            if let Some(var) = scope.variables.iter_mut().find(|v| v.name == name) {
                var.used = true;
                return;
            }
        }
    }

    /// Check if a variable exists in scope
    pub fn variable_exists(&self, name: &str) -> bool {
        self.scope_stack
            .iter()
            .rev()
            .any(|s| s.variables.iter().any(|v| v.name == name))
    }

    /// Check if a variable is mutable
    pub fn is_variable_mutable(&self, name: &str) -> bool {
        for scope in self.scope_stack.iter().rev() {
            if let Some(var) = scope.variables.iter().find(|v| v.name == name) {
                return var.mutable;
            }
        }
        false
    }

    /// Get unused variables in the current scope
    pub fn unused_variables(&self) -> Vec<(String, Span)> {
        let mut result = Vec::new();
        for scope in &self.scope_stack {
            for var in &scope.variables {
                if !var.used && !var.name.starts_with('_') {
                    result.push((var.name.clone(), var.span));
                }
            }
        }
        result
    }

    /// Get the source text for a span
    pub fn source_for_span(&self, span: Span) -> &str {
        let start = span.start.min(self.source.len());
        let end = span.end.min(self.source.len());
        &self.source[start..end]
    }

    /// Get the line number for a position
    pub fn line_number(&self, pos: usize) -> usize {
        self.source[..pos.min(self.source.len())].lines().count()
    }

    /// Get the column number for a position
    pub fn column_number(&self, pos: usize) -> usize {
        let pos = pos.min(self.source.len());
        self.source[..pos]
            .lines()
            .last()
            .map(|line| line.len() + 1)
            .unwrap_or(1)
    }
}

impl ScopeKind {
    /// Check if this is a function scope
    pub fn is_function(&self) -> bool {
        matches!(self, ScopeKind::Function)
    }

    /// Check if this is a loop scope
    pub fn is_loop(&self) -> bool {
        matches!(self, ScopeKind::Loop)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_creation() {
        let cx = LintContext::new("fn main() {}", Some("test.jet".to_string()));
        assert_eq!(cx.source(), "fn main() {}");
        assert_eq!(cx.filename(), Some("test.jet"));
    }

    #[test]
    fn test_variable_tracking() {
        let mut cx = LintContext::new("", None);

        cx.enter_scope(ScopeKind::Function);
        cx.add_variable("x", true, Span::new(0, 1));
        cx.add_variable("y", false, Span::new(2, 3));

        assert!(cx.variable_exists("x"));
        assert!(cx.variable_exists("y"));
        assert!(!cx.variable_exists("z"));

        assert!(cx.is_variable_mutable("x"));
        assert!(!cx.is_variable_mutable("y"));

        cx.mark_variable_used("x");

        let unused = cx.unused_variables();
        assert_eq!(unused.len(), 1);
        assert_eq!(unused[0].0, "y");
    }

    #[test]
    fn test_scope_nesting() {
        let mut cx = LintContext::new("", None);

        cx.enter_scope(ScopeKind::Function);
        cx.add_variable("outer", false, Span::new(0, 5));

        cx.enter_scope(ScopeKind::Block);
        cx.add_variable("inner", false, Span::new(6, 11));

        // Both should be visible
        assert!(cx.variable_exists("outer"));
        assert!(cx.variable_exists("inner"));

        cx.leave_scope();

        // Only outer should be visible now
        assert!(cx.variable_exists("outer"));
        assert!(!cx.variable_exists("inner"));
    }

    #[test]
    fn test_terminal_scope() {
        let mut cx = LintContext::new("", None);

        cx.enter_scope(ScopeKind::Function);
        assert!(!cx.is_terminal());

        cx.mark_terminal();
        assert!(cx.is_terminal());
    }
}
