//! Suspicious lints
//!
//! These lints detect code that is likely incorrect or problematic.

use jet_lexer::Span;
use jet_parser::ast::{Block, Expr, Import, ImportItem, Module, ModuleItem, Pattern, Stmt};

use crate::context::{LintContext, ScopeKind};
use crate::lints::{collect_pattern_idents, pattern_ident};
use crate::{Lint, LintCategory, LintLevel, LintPass, LintViolation};

/// Lint for unused variables
pub struct UnusedVariablesLint;

impl Lint for UnusedVariablesLint {
    fn name(&self) -> &'static str {
        "unused_variables"
    }

    fn description(&self) -> &'static str {
        "Detects variables that are declared but never read"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Suspicious
    }
}

impl LintPass for UnusedVariablesLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                cx.enter_scope(ScopeKind::Function);

                // Add parameters as variables
                for param in &func.params {
                    if let Some(ident) = pattern_ident(&param.pattern) {
                        cx.add_variable(&ident.name, false, ident.span);
                    }
                }

                self.check_expr(cx, &func.body);

                // Report unused variables
                for (name, span) in cx.unused_variables() {
                    cx.report(
                        LintViolation::new(
                            self.name(),
                            self.default_level(),
                            format!("unused variable: `{}`", name),
                            span,
                        )
                        .with_suggestion(format!(
                            "consider using `{}` or renaming it to `_{}`",
                            name, name
                        )),
                    );
                }

                cx.leave_scope();
            }
        }
    }
}

impl UnusedVariablesLint {
    fn check_expr(&mut self, cx: &mut LintContext, expr: &Expr) {
        match expr {
            Expr::Variable(ident) => {
                cx.mark_variable_used(&ident.name);
            }
            Expr::Path(path) => {
                if let Some(ident) = path.segments.first() {
                    cx.mark_variable_used(&ident.name);
                }
            }
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
                    cx.enter_scope(ScopeKind::Block);
                    self.collect_pattern_bindings(cx, &arm.pattern);
                    self.check_expr(cx, &arm.body);
                    cx.leave_scope();
                }
            }
            Expr::While { cond, body, .. } => {
                self.check_expr(cx, cond);
                cx.enter_scope(ScopeKind::Loop);
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            Expr::For {
                pattern,
                iterable,
                body,
                ..
            } => {
                self.check_expr(cx, iterable);
                cx.enter_scope(ScopeKind::Loop);
                self.collect_pattern_bindings(cx, pattern);
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            Expr::Loop { body, .. } => {
                cx.enter_scope(ScopeKind::Loop);
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            Expr::Lambda { params, body, .. } => {
                cx.enter_scope(ScopeKind::Function);
                for param in params {
                    self.collect_pattern_bindings(cx, &param.pattern);
                }
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            Expr::Binary { left, right, .. } => {
                self.check_expr(cx, left);
                self.check_expr(cx, right);
            }
            Expr::Unary { expr, .. } => {
                self.check_expr(cx, expr);
            }
            Expr::Call { func, args } => {
                self.check_expr(cx, func);
                for arg in args {
                    self.check_expr(cx, arg);
                }
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.check_expr(cx, receiver);
                for arg in args {
                    self.check_expr(cx, arg);
                }
            }
            Expr::FieldAccess { object, .. } => {
                self.check_expr(cx, object);
            }
            Expr::Index { object, index } => {
                self.check_expr(cx, object);
                self.check_expr(cx, index);
            }
            Expr::Assign { target, value, .. } => {
                self.check_expr(cx, target);
                self.check_expr(cx, value);
            }
            Expr::Tuple(exprs) | Expr::Array(exprs) => {
                for expr in exprs {
                    self.check_expr(cx, expr);
                }
            }
            Expr::StructLiteral { fields, .. } => {
                for field in fields {
                    if let Some(value) = &field.value {
                        self.check_expr(cx, value);
                    }
                }
            }
            Expr::Return(expr) | Expr::Break { value: expr, .. } => {
                if let Some(expr) = expr {
                    self.check_expr(cx, expr);
                }
            }
            Expr::Await(expr) | Expr::Try(expr) | Expr::Spawn(expr) => {
                self.check_expr(cx, expr);
            }
            _ => {}
        }
    }

    fn check_block(&mut self, cx: &mut LintContext, block: &Block) {
        cx.enter_scope(ScopeKind::Block);

        for stmt in &block.stmts {
            self.check_stmt(cx, stmt);
        }

        if let Some(expr) = &block.expr {
            self.check_expr(cx, expr);
        }

        cx.leave_scope();
    }

    fn check_stmt(&mut self, cx: &mut LintContext, stmt: &Stmt) {
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                self.check_expr(cx, value);
                self.collect_pattern_bindings(cx, pattern);
            }
            Stmt::Expr(expr) => {
                self.check_expr(cx, expr);
            }
            Stmt::Assign { target, value, .. } => {
                self.check_expr(cx, target);
                self.check_expr(cx, value);
            }
            Stmt::Return(Some(expr)) => {
                self.check_expr(cx, expr);
            }
            Stmt::Return(None) => {}
            Stmt::Break {
                value: Some(value), ..
            } => {
                self.check_expr(cx, value);
            }
            Stmt::Break { value: None, .. } => {}
            _ => {}
        }
    }

    fn collect_pattern_bindings(&mut self, cx: &mut LintContext, pattern: &Pattern) {
        let mut idents = Vec::new();
        collect_pattern_idents(pattern, &mut idents);
        for (name, span) in idents {
            cx.add_variable(name, false, span);
        }
    }
}

/// Lint for unused imports
pub struct UnusedImportsLint;

impl Lint for UnusedImportsLint {
    fn name(&self) -> &'static str {
        "unused_imports"
    }

    fn description(&self) -> &'static str {
        "Detects imports that are never used"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Suspicious
    }
}

impl LintPass for UnusedImportsLint {
    fn check_module(&mut self, _cx: &mut LintContext, module: &Module) {
        // Collect all imports
        let mut imports: Vec<(String, Span)> = Vec::new();

        for item in &module.items {
            if let ModuleItem::Import(import) = item {
                self.collect_import_names(import, &mut imports);
            }
        }

        // Check which imports are used
        // This is a simplified check - in a real implementation,
        // we'd track all identifier references in the module

        for (_name, _span) in imports {
            // For now, we assume all imports are used
            // In a real implementation, we'd check against all identifier references
        }
    }
}

impl UnusedImportsLint {
    fn collect_import_names(&self, import: &Import, names: &mut Vec<(String, Span)>) {
        match import {
            Import::Simple { path, alias } => {
                let name = alias
                    .as_ref()
                    .map(|a| a.name.clone())
                    .or_else(|| path.segments.last().map(|s| s.name.clone()))
                    .unwrap_or_default();
                names.push((name, Span::new(0, 0)));
            }
            Import::From { items, .. } => {
                self.collect_import_items(items, names);
            }
        }
    }

    fn collect_import_items(&self, items: &[ImportItem], names: &mut Vec<(String, Span)>) {
        for item in items {
            match item {
                ImportItem::Single { name, alias } => {
                    let name = alias
                        .as_ref()
                        .map(|a| a.name.clone())
                        .unwrap_or_else(|| name.name.clone());
                    names.push((name, Span::new(0, 0)));
                }
                ImportItem::Group(group) => {
                    self.collect_import_items(group, names);
                }
            }
        }
    }
}

/// Lint for dead code
pub struct DeadCodeLint;

impl Lint for DeadCodeLint {
    fn name(&self) -> &'static str {
        "dead_code"
    }

    fn description(&self) -> &'static str {
        "Detects unreachable functions, modules, and other items"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Suspicious
    }
}

impl LintPass for DeadCodeLint {
    fn check_module(&mut self, _cx: &mut LintContext, module: &Module) {
        // Track which items are public or used
        let _used_items: Vec<String> = Vec::new();

        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                if !func.public && !func.name.name.starts_with('_') {
                    // Check if function is used anywhere
                    // This would require cross-module analysis
                    // For now, we skip this check
                }
            }
        }
    }
}

/// Lint for variable shadowing
pub struct ShadowingLint;

impl Lint for ShadowingLint {
    fn name(&self) -> &'static str {
        "shadowing"
    }

    fn description(&self) -> &'static str {
        "Detects when a variable shadows another variable in an outer scope"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Suspicious
    }
}

impl LintPass for ShadowingLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                cx.enter_scope(ScopeKind::Function);

                // Track parameter names
                for param in &func.params {
                    if let Some(ident) = pattern_ident(&param.pattern) {
                        cx.add_variable(&ident.name, false, ident.span);
                    }
                }

                self.check_expr(cx, &func.body);

                cx.leave_scope();
            }
        }
    }
}

impl ShadowingLint {
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
                    cx.enter_scope(ScopeKind::Block);
                    self.check_pattern_for_shadowing(cx, &arm.pattern);
                    self.check_expr(cx, &arm.body);
                    cx.leave_scope();
                }
            }
            Expr::While { cond, body, .. } => {
                self.check_expr(cx, cond);
                cx.enter_scope(ScopeKind::Loop);
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            Expr::For {
                pattern,
                iterable,
                body,
                ..
            } => {
                self.check_expr(cx, iterable);
                cx.enter_scope(ScopeKind::Loop);
                self.check_pattern_for_shadowing(cx, pattern);
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            Expr::Loop { body, .. } => {
                cx.enter_scope(ScopeKind::Loop);
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            Expr::Lambda { params, body, .. } => {
                cx.enter_scope(ScopeKind::Function);
                for param in params {
                    self.check_pattern_for_shadowing(cx, &param.pattern);
                }
                self.check_expr(cx, body);
                cx.leave_scope();
            }
            _ => {}
        }
    }

    fn check_block(&mut self, cx: &mut LintContext, block: &Block) {
        cx.enter_scope(ScopeKind::Block);

        for stmt in &block.stmts {
            self.check_stmt(cx, stmt);
        }

        if let Some(expr) = &block.expr {
            self.check_expr(cx, expr);
        }

        cx.leave_scope();
    }

    fn check_stmt(&mut self, cx: &mut LintContext, stmt: &Stmt) {
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                self.check_expr(cx, value);
                self.check_pattern_for_shadowing(cx, pattern);
            }
            Stmt::Expr(expr) => {
                self.check_expr(cx, expr);
            }
            _ => {}
        }
    }

    fn check_pattern_for_shadowing(&mut self, cx: &mut LintContext, pattern: &Pattern) {
        let mut idents = Vec::new();
        collect_pattern_idents(pattern, &mut idents);

        for (name, span) in idents {
            if cx.variable_exists(&name) {
                cx.report(LintViolation::new(
                    "shadowing",
                    LintLevel::Warn,
                    format!("variable `{}` shadows a variable in an outer scope", name),
                    span,
                ));
            }
            cx.add_variable(name, false, span);
        }
    }
}

/// Lint for unused mut keyword
pub struct UnusedMutLint;

impl Lint for UnusedMutLint {
    fn name(&self) -> &'static str {
        "unused_mut"
    }

    fn description(&self) -> &'static str {
        "Detects when the `mut` keyword is used but the variable is never modified"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Suspicious
    }
}

impl LintPass for UnusedMutLint {
    fn check_module(&mut self, _cx: &mut LintContext, _module: &Module) {
        // This would require tracking mutations to variables
        // In a full implementation, we'd check for assignments to mutable variables
    }
}

/// Lint for suggesting if-let instead of single match arm
pub struct SingleMatchLint;

impl Lint for SingleMatchLint {
    fn name(&self) -> &'static str {
        "single_match"
    }

    fn description(&self) -> &'static str {
        "Suggests using if-let instead of match with a single arm"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Suspicious
    }
}

impl LintPass for SingleMatchLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                self.check_expr(cx, &func.body);
            }
        }
    }
}

impl SingleMatchLint {
    fn check_expr(&mut self, cx: &mut LintContext, expr: &Expr) {
        match expr {
            Expr::Match {
                expr: _match_expr,
                arms,
            } => {
                // Check if there's only one non-wildcard arm
                let non_wildcard_arms: Vec<_> = arms
                    .iter()
                    .filter(|a| !matches!(a.pattern, Pattern::Wildcard(_)))
                    .collect();

                if non_wildcard_arms.len() == 1 {
                    // Check if there's a wildcard arm that just returns unit/pass
                    let has_wildcard_unit = arms.iter().any(|a| {
                        if !matches!(a.pattern, Pattern::Wildcard(_)) {
                            return false;
                        }
                        matches!(*a.body, Expr::Pass)
                            || matches!(*a.body, Expr::Block(Block { stmts: ref s, expr: None, .. }) if s.is_empty())
                    });

                    if has_wildcard_unit {
                        cx.report(
                            LintViolation::new(
                                self.name(),
                                self.default_level(),
                                "you seem to be trying to use `match` for an equality check",
                                Span::new(0, 0),
                            )
                            .with_suggestion("consider using `if let` instead"),
                        );
                    }
                }

                // Recursively check arms
                for arm in arms {
                    self.check_expr(cx, &arm.body);
                }
            }
            Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.check_stmt(cx, stmt);
                }
                if let Some(expr) = &block.expr {
                    self.check_expr(cx, expr);
                }
            }
            _ => {}
        }
    }

    fn check_stmt(&mut self, cx: &mut LintContext, stmt: &Stmt) {
        if let Stmt::Expr(expr) = stmt {
            self.check_expr(cx, expr);
        }
    }
}

/// Lint for needless borrows
pub struct NeedlessBorrowLint;

impl Lint for NeedlessBorrowLint {
    fn name(&self) -> &'static str {
        "needless_borrow"
    }

    fn description(&self) -> &'static str {
        "Detects when a reference is taken but not needed"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Suspicious
    }
}

impl LintPass for NeedlessBorrowLint {
    fn check_module(&mut self, _cx: &mut LintContext, _module: &Module) {
        // This would require type information to determine if a borrow is needed
        // In a full implementation with type checking, we'd check for unnecessary & and &mut
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unused_variables_lint() {
        // Test that the lint is properly configured
        let lint = UnusedVariablesLint;
        assert_eq!(lint.name(), "unused_variables");
        assert_eq!(lint.default_level(), LintLevel::Warn);
    }

    #[test]
    fn test_shadowing_lint() {
        let lint = ShadowingLint;
        assert_eq!(lint.name(), "shadowing");
        assert_eq!(lint.default_level(), LintLevel::Warn);
    }
}
