//! Correctness lints
//!
//! These lints detect code that is clearly wrong or will fail to compile.

use jet_lexer::Span;
use jet_parser::ast::{Block, Expr, MatchArm, Module, ModuleItem, Pattern, Stmt};

use crate::context::{LintContext, ScopeKind};
use crate::{Lint, LintCategory, LintLevel, LintPass, LintViolation};

/// Lint for unreachable code after return/break
pub struct UnreachableCodeLint;

impl Lint for UnreachableCodeLint {
    fn name(&self) -> &'static str {
        "unreachable_code"
    }

    fn description(&self) -> &'static str {
        "Detects code that will never be executed because it follows a return, break, or continue statement"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Error
    }

    fn category(&self) -> LintCategory {
        LintCategory::Correctness
    }
}

impl LintPass for UnreachableCodeLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                cx.enter_scope(ScopeKind::Function);
                self.check_expr(cx, &func.body);
                cx.leave_scope();
            }
        }
    }
}

impl UnreachableCodeLint {
    fn check_expr(&mut self, cx: &mut LintContext, expr: &Expr) {
        match expr {
            Expr::Block(block) => {
                self.check_block(cx, block);
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.check_expr(cx, cond);
                self.check_expr(cx, then_branch);
                if let Some(else_expr) = else_branch {
                    self.check_expr(cx, else_expr);
                }
            }
            Expr::Match { expr, arms } => {
                self.check_expr(cx, expr);
                for arm in arms {
                    self.check_expr(cx, &arm.body);
                }
            }
            Expr::While { cond, body, .. } => {
                self.check_expr(cx, cond);
                cx.enter_scope(ScopeKind::Loop);
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            Expr::For { iterable, body, .. } => {
                self.check_expr(cx, iterable);
                cx.enter_scope(ScopeKind::Loop);
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            Expr::Loop { body, .. } => {
                cx.enter_scope(ScopeKind::Loop);
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            Expr::Lambda { body, .. } => {
                cx.enter_scope(ScopeKind::Function);
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            _ => {}
        }
    }

    fn check_block(&mut self, cx: &mut LintContext, block: &Block) {
        let mut terminal_found = false;

        for stmt in &block.stmts {
            if terminal_found {
                // Report unreachable statement
                let span = self.stmt_span(stmt);
                cx.report(LintViolation::new(
                    self.name(),
                    self.default_level(),
                    "unreachable code",
                    span,
                ));
                break;
            }

            if self.is_terminal_stmt(stmt) {
                terminal_found = true;
            }

            self.check_stmt(cx, stmt);
        }

        // Check the trailing expression
        if let Some(expr) = &block.expr {
            if terminal_found {
                cx.report(LintViolation::new(
                    self.name(),
                    self.default_level(),
                    "unreachable code",
                    self.expr_span(expr),
                ));
            }
            self.check_expr(cx, expr);
        }
    }

    fn check_stmt(&mut self, cx: &mut LintContext, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.check_expr(cx, expr);
            }
            Stmt::Let { value, .. } => {
                self.check_expr(cx, value);
            }
            Stmt::Assign { value, .. } => {
                self.check_expr(cx, value);
            }
            _ => {}
        }
    }

    fn is_terminal_stmt(&self, stmt: &Stmt) -> bool {
        matches!(
            stmt,
            Stmt::Return(_) | Stmt::Break { .. } | Stmt::Continue { .. }
        )
    }

    fn stmt_span(&self, _stmt: &Stmt) -> Span {
        // This is a simplified span extraction
        // In a real implementation, AST nodes would have spans
        Span::new(0, 0)
    }

    fn expr_span(&self, _expr: &Expr) -> Span {
        Span::new(0, 0)
    }
}

/// Lint for detecting ignored Result/Option returns
pub struct UnusedMustUseLint;

impl Lint for UnusedMustUseLint {
    fn name(&self) -> &'static str {
        "unused_must_use"
    }

    fn description(&self) -> &'static str {
        "Detects when Result, Option, or #[must_use] types are ignored"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Error
    }

    fn category(&self) -> LintCategory {
        LintCategory::Correctness
    }
}

impl LintPass for UnusedMustUseLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                self.check_expr(cx, &func.body);
            }
        }
    }
}

impl UnusedMustUseLint {
    fn check_expr(&mut self, cx: &mut LintContext, expr: &Expr) {
        match expr {
            Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.check_stmt(cx, stmt);
                }
                if let Some(expr) = &block.expr {
                    self.check_expr(cx, expr);
                }
            }
            Expr::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.check_expr(cx, then_branch);
                if let Some(else_expr) = else_branch {
                    self.check_expr(cx, else_expr);
                }
            }
            Expr::Match { arms, .. } => {
                for arm in arms {
                    self.check_expr(cx, &arm.body);
                }
            }
            Expr::While { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::For { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::Loop { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::Lambda { body, .. } => {
                self.check_expr(cx, body);
            }
            _ => {}
        }
    }

    fn check_stmt(&mut self, cx: &mut LintContext, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                if self.is_must_use_expr(expr) {
                    cx.report(LintViolation::new(
                        self.name(),
                        self.default_level(),
                        "unused return value of function that must be used",
                        Span::new(0, 0),
                    )
                    .with_suggestion("consider using the return value or explicitly ignoring it with `let _ = ...`"));
                }
                self.check_expr(cx, expr);
            }
            Stmt::Let { value, .. } => {
                self.check_expr(cx, value);
            }
            Stmt::Assign { value, .. } => {
                self.check_expr(cx, value);
            }
            _ => {}
        }
    }

    fn is_must_use_expr(&self, expr: &Expr) -> bool {
        // Check for method calls that return Result/Option
        if let Expr::MethodCall {
            receiver: _,
            method,
            ..
        } = expr
        {
            let method_name = method.name.as_str();
            // Common methods that return Result/Option
            let must_use_methods = [
                "parse",
                "try_into",
                "try_from",
                "checked_add",
                "checked_sub",
                "checked_mul",
                "checked_div",
                "get",
                "insert",
                "remove",
            ];
            if must_use_methods.contains(&method_name) {
                return true;
            }
        }

        // Check for function calls that return Result/Option
        if let Expr::Call { func, .. } = expr {
            if let Expr::Path(path) = func.as_ref() {
                let path_str = path.segments.last().map(|s| s.name.as_str()).unwrap_or("");
                let must_use_functions = ["Result", "Option", "Ok", "Err", "Some", "None"];
                // This is a heuristic - in a real implementation we'd check types
                if must_use_functions.iter().any(|f| path_str.contains(f)) {
                    return true;
                }
            }
        }

        false
    }
}

/// Lint for invalid doc attributes
pub struct InvalidDocAttributesLint;

impl Lint for InvalidDocAttributesLint {
    fn name(&self) -> &'static str {
        "invalid_doc_attributes"
    }

    fn description(&self) -> &'static str {
        "Detects malformed doc comments and documentation attributes"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Error
    }

    fn category(&self) -> LintCategory {
        LintCategory::Correctness
    }
}

impl LintPass for InvalidDocAttributesLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        // Check module-level documentation
        // In a real implementation, we'd check for proper doc comment syntax
        // and validate markdown content

        for item in &module.items {
            self.check_item(cx, item);
        }
    }
}

impl InvalidDocAttributesLint {
    fn check_item(&mut self, cx: &mut LintContext, item: &ModuleItem) {
        // Check doc comments on items
        match item {
            ModuleItem::Function(func) => self.check_item_doc_syntax(
                cx,
                "function",
                &func.name.name,
                func.name.span,
                func.span,
            ),
            ModuleItem::Struct(struct_def) => self.check_item_doc_syntax(
                cx,
                "struct",
                &struct_def.name.name,
                struct_def.name.span,
                struct_def.span,
            ),
            ModuleItem::Enum(enum_def) => self.check_item_doc_syntax(
                cx,
                "enum",
                &enum_def.name.name,
                enum_def.name.span,
                enum_def.span,
            ),
            _ => {}
        }
    }

    fn check_item_doc_syntax(
        &mut self,
        cx: &mut LintContext,
        kind: &str,
        name: &str,
        name_span: jet_lexer::Span,
        item_span: jet_lexer::Span,
    ) {
        if has_malformed_preceding_doc_comment(cx.source(), item_span.start) {
            cx.report(LintViolation::new(
                "invalid_doc_attributes",
                LintLevel::Error,
                format!(
                    "{} `{}` has malformed doc comments; use `###` doc lines",
                    kind, name
                ),
                name_span,
            ));
        }
    }
}

fn has_malformed_preceding_doc_comment(source: &str, item_start: usize) -> bool {
    let prefix = &source[..item_start.min(source.len())];
    let mut saw_comment = false;

    for line in prefix.lines().rev() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            if saw_comment {
                break;
            }
            continue;
        }
        if trimmed.starts_with('#') {
            saw_comment = true;
            if !trimmed.starts_with("###") {
                return true;
            }
            continue;
        }
        break;
    }

    false
}

/// Lint for duplicate match arms
pub struct DuplicateMatchArmsLint;

impl Lint for DuplicateMatchArmsLint {
    fn name(&self) -> &'static str {
        "duplicate_match_arms"
    }

    fn description(&self) -> &'static str {
        "Detects unreachable match arms that are shadowed by previous arms"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Error
    }

    fn category(&self) -> LintCategory {
        LintCategory::Correctness
    }
}

impl LintPass for DuplicateMatchArmsLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                self.check_expr(cx, &func.body);
            }
        }
    }
}

impl DuplicateMatchArmsLint {
    fn check_expr(&mut self, cx: &mut LintContext, expr: &Expr) {
        match expr {
            Expr::Match { arms, .. } => {
                self.check_match_arms(cx, arms);
            }
            Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.check_stmt(cx, stmt);
                }
                if let Some(expr) = &block.expr {
                    self.check_expr(cx, expr);
                }
            }
            Expr::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.check_expr(cx, then_branch);
                if let Some(else_expr) = else_branch {
                    self.check_expr(cx, else_expr);
                }
            }
            Expr::While { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::For { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::Loop { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::Lambda { body, .. } => {
                self.check_expr(cx, body);
            }
            _ => {}
        }
    }

    fn check_stmt(&mut self, cx: &mut LintContext, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) | Stmt::Let { value: expr, .. } | Stmt::Assign { value: expr, .. } => {
                self.check_expr(cx, expr);
            }
            _ => {}
        }
    }

    fn check_match_arms(&mut self, cx: &mut LintContext, arms: &[MatchArm]) {
        let mut seen_patterns: Vec<&Pattern> = Vec::new();

        for arm in arms {
            // Check if this pattern is covered by a previous one
            for seen in &seen_patterns {
                if self.patterns_overlap(seen, &arm.pattern) {
                    cx.report(LintViolation::new(
                        self.name(),
                        self.default_level(),
                        "unreachable pattern",
                        Span::new(0, 0),
                    ));
                    break;
                }
            }

            seen_patterns.push(&arm.pattern);

            // Recursively check the arm body
            self.check_expr(cx, &arm.body);
        }
    }

    fn patterns_overlap(&self, p1: &Pattern, p2: &Pattern) -> bool {
        // Check if pattern p1 covers pattern p2 (making p2 unreachable)
        match (p1, p2) {
            // Wildcard covers everything
            (Pattern::Wildcard(_), _) => true,
            // Same identifier
            (Pattern::Ident { name: n1, .. }, Pattern::Ident { name: n2, .. }) => {
                n1.name == n2.name
            }
            // Same literal
            (Pattern::Literal(l1), Pattern::Literal(l2)) => l1 == l2,
            // Same enum variant
            (
                Pattern::Enum {
                    path: p1,
                    variant: v1,
                    ..
                },
                Pattern::Enum {
                    path: p2,
                    variant: v2,
                    ..
                },
            ) => {
                p1.segments.last().map(|s| &s.name) == p2.segments.last().map(|s| &s.name)
                    && v1.name == v2.name
            }
            // Or pattern - if any branch overlaps, the whole pattern overlaps
            (Pattern::Or(left, right), p2) => {
                self.patterns_overlap(left, p2) || self.patterns_overlap(right, p2)
            }
            (p1, Pattern::Or(left, right)) => {
                self.patterns_overlap(p1, left) || self.patterns_overlap(p1, right)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span;
    use jet_parser::ast::{Ident, Literal, Pattern};

    #[test]
    fn test_unreachable_code_detection() {
        // This test would verify unreachable code detection
        // In a real test, we'd parse actual source code
    }

    #[test]
    fn test_pattern_overlap() {
        let lint = DuplicateMatchArmsLint;

        // Wildcard covers everything
        let wildcard = Pattern::Wildcard(Span::new(0, 1));
        let ident = Pattern::Ident {
            mutable: false,
            name: Ident::new("x", Span::new(0, 1)),
        };

        assert!(lint.patterns_overlap(&wildcard, &ident));

        // Same literals overlap
        let lit1 = Pattern::Literal(Literal::Integer(42));
        let lit2 = Pattern::Literal(Literal::Integer(42));
        assert!(lint.patterns_overlap(&lit1, &lit2));

        // Different literals don't overlap
        let lit3 = Pattern::Literal(Literal::Integer(43));
        assert!(!lint.patterns_overlap(&lit1, &lit3));
    }
}
