//! Style lints
//!
//! These lints enforce naming conventions and code style.

use jet_parser::ast::{
    ConstDef, EnumDef, Function, Module, ModuleItem, StructDef, TraitDef, TypeAlias,
};

use crate::context::LintContext;
use crate::lints::{is_camel_case, is_snake_case, is_upper_case, pattern_ident};
use crate::{Lint, LintCategory, LintLevel, LintPass, LintViolation};

/// Lint for naming conventions
pub struct NamingConventionsLint;

impl Lint for NamingConventionsLint {
    fn name(&self) -> &'static str {
        "naming_conventions"
    }

    fn description(&self) -> &'static str {
        "Enforces naming conventions: snake_case for variables/functions, CamelCase for types, UPPER_CASE for constants"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Style
    }
}

impl LintPass for NamingConventionsLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            match item {
                ModuleItem::Function(func) => {
                    self.check_function(cx, func);
                }
                ModuleItem::Struct(struct_def) => {
                    self.check_struct(cx, struct_def);
                }
                ModuleItem::Enum(enum_def) => {
                    self.check_enum(cx, enum_def);
                }
                ModuleItem::Trait(trait_def) => {
                    self.check_trait(cx, trait_def);
                }
                ModuleItem::Const(const_def) => {
                    self.check_const(cx, const_def);
                }
                ModuleItem::TypeAlias(type_alias) => {
                    self.check_type_alias(cx, type_alias);
                }
                _ => {}
            }
        }
    }
}

impl NamingConventionsLint {
    fn check_function(&mut self, cx: &mut LintContext, func: &Function) {
        // Functions should be snake_case
        if !is_snake_case(&func.name.name) {
            cx.report(
                LintViolation::new(
                    "non_snake_case",
                    LintLevel::Warn,
                    format!(
                        "function `{}` should have a snake_case name",
                        func.name.name
                    ),
                    func.name.span,
                )
                .with_suggestion(format!(
                    "consider renaming to `{}`",
                    to_snake_case(&func.name.name)
                )),
            );
        }

        // Check parameters
        for param in &func.params {
            if let Some(ident) = pattern_ident(&param.pattern) {
                if !is_snake_case(&ident.name) && !ident.name.starts_with('_') {
                    cx.report(LintViolation::new(
                        "non_snake_case",
                        LintLevel::Warn,
                        format!("parameter `{}` should have a snake_case name", ident.name),
                        ident.span,
                    ));
                }
            }
        }
    }

    fn check_struct(&mut self, cx: &mut LintContext, struct_def: &StructDef) {
        // Structs should be CamelCase
        if !is_camel_case(&struct_def.name.name) {
            cx.report(
                LintViolation::new(
                    "non_camel_case_types",
                    LintLevel::Warn,
                    format!(
                        "struct `{}` should have a CamelCase name",
                        struct_def.name.name
                    ),
                    struct_def.name.span,
                )
                .with_suggestion(format!(
                    "consider renaming to `{}`",
                    to_camel_case(&struct_def.name.name)
                )),
            );
        }

        // Fields should be snake_case
        for field in &struct_def.fields {
            if !is_snake_case(&field.name.name) {
                cx.report(LintViolation::new(
                    "non_snake_case",
                    LintLevel::Warn,
                    format!("field `{}` should have a snake_case name", field.name.name),
                    field.name.span,
                ));
            }
        }
    }

    fn check_enum(&mut self, cx: &mut LintContext, enum_def: &EnumDef) {
        // Enums should be CamelCase
        if !is_camel_case(&enum_def.name.name) {
            cx.report(LintViolation::new(
                "non_camel_case_types",
                LintLevel::Warn,
                format!("enum `{}` should have a CamelCase name", enum_def.name.name),
                enum_def.name.span,
            ));
        }

        // Variants should be CamelCase
        for variant in &enum_def.variants {
            if !is_camel_case(&variant.name.name) {
                cx.report(LintViolation::new(
                    "non_camel_case_types",
                    LintLevel::Warn,
                    format!(
                        "variant `{}` should have a CamelCase name",
                        variant.name.name
                    ),
                    variant.name.span,
                ));
            }
        }
    }

    fn check_trait(&mut self, cx: &mut LintContext, trait_def: &TraitDef) {
        // Traits should be CamelCase
        if !is_camel_case(&trait_def.name.name) {
            cx.report(LintViolation::new(
                "non_camel_case_types",
                LintLevel::Warn,
                format!(
                    "trait `{}` should have a CamelCase name",
                    trait_def.name.name
                ),
                trait_def.name.span,
            ));
        }
    }

    fn check_const(&mut self, cx: &mut LintContext, const_def: &ConstDef) {
        // Constants should be UPPER_CASE
        if !is_upper_case(&const_def.name.name) {
            cx.report(
                LintViolation::new(
                    "non_upper_case_globals",
                    LintLevel::Warn,
                    format!(
                        "constant `{}` should have an UPPER_CASE name",
                        const_def.name.name
                    ),
                    const_def.name.span,
                )
                .with_suggestion(format!(
                    "consider renaming to `{}`",
                    to_upper_case(&const_def.name.name)
                )),
            );
        }
    }

    fn check_type_alias(&mut self, cx: &mut LintContext, type_alias: &TypeAlias) {
        // Type aliases should be CamelCase
        if !is_camel_case(&type_alias.name.name) {
            cx.report(LintViolation::new(
                "non_camel_case_types",
                LintLevel::Warn,
                format!(
                    "type alias `{}` should have a CamelCase name",
                    type_alias.name.name
                ),
                type_alias.name.span,
            ));
        }
    }
}

/// Lint for missing documentation
pub struct MissingDocsLint;

impl Lint for MissingDocsLint {
    fn name(&self) -> &'static str {
        "missing_docs"
    }

    fn description(&self) -> &'static str {
        "Detects public items without documentation"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Style
    }
}

impl LintPass for MissingDocsLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            match item {
                ModuleItem::Function(func) if func.public => self.check_public_item_docs(
                    cx,
                    "function",
                    &func.name.name,
                    func.name.span,
                    func.span,
                ),
                ModuleItem::Struct(struct_def) if struct_def.public => self.check_public_item_docs(
                    cx,
                    "struct",
                    &struct_def.name.name,
                    struct_def.name.span,
                    struct_def.span,
                ),
                ModuleItem::Enum(enum_def) if enum_def.public => self.check_public_item_docs(
                    cx,
                    "enum",
                    &enum_def.name.name,
                    enum_def.name.span,
                    enum_def.span,
                ),
                ModuleItem::Trait(trait_def) if trait_def.public => self.check_public_item_docs(
                    cx,
                    "trait",
                    &trait_def.name.name,
                    trait_def.name.span,
                    trait_def.span,
                ),
                ModuleItem::Const(const_def) if const_def.public => self.check_public_item_docs(
                    cx,
                    "const",
                    &const_def.name.name,
                    const_def.name.span,
                    const_def.span,
                ),
                _ => {}
            }
        }
    }
}

impl MissingDocsLint {
    fn check_public_item_docs(
        &mut self,
        cx: &mut LintContext,
        kind: &str,
        name: &str,
        name_span: jet_lexer::Span,
        item_span: jet_lexer::Span,
    ) {
        if !has_preceding_doc_comment(cx.source(), item_span.start) {
            cx.report(LintViolation::new(
                "missing_docs",
                LintLevel::Warn,
                format!("public {} `{}` is missing a doc comment", kind, name),
                name_span,
            ));
        }
    }
}

fn has_preceding_doc_comment(source: &str, item_start: usize) -> bool {
    let prefix = &source[..item_start.min(source.len())];
    let mut lines = prefix.lines().rev();

    for line in &mut lines {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        return trimmed.starts_with("###");
    }

    false
}

/// Lint for trivial casts
pub struct TrivialCastsLint;

impl Lint for TrivialCastsLint {
    fn name(&self) -> &'static str {
        "trivial_casts"
    }

    fn description(&self) -> &'static str {
        "Detects casts that are unnecessary or always succeed"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Style
    }
}

impl LintPass for TrivialCastsLint {
    fn check_module(&mut self, _cx: &mut LintContext, _module: &Module) {
        // This would require type information to detect trivial casts
        // In a full implementation with type checking, we'd check for:
        // - Casting to the same type
        // - Upcasts that are always safe
    }
}

/// Convert a string to snake_case
fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    let mut prev_is_upper = false;

    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 && !prev_is_upper {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap_or(c));
            prev_is_upper = true;
        } else {
            result.push(c);
            prev_is_upper = false;
        }
    }

    result
}

/// Convert a string to CamelCase
fn to_camel_case(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = true;

    for c in s.chars() {
        if c == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_uppercase().next().unwrap_or(c));
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }

    result
}

/// Convert a string to UPPER_CASE
fn to_upper_case(s: &str) -> String {
    s.to_uppercase()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_snake_case() {
        assert_eq!(to_snake_case("CamelCase"), "camel_case");
        assert_eq!(to_snake_case("Simple"), "simple");
        assert_eq!(to_snake_case("snake_case"), "snake_case");
        assert_eq!(to_snake_case("XMLParser"), "xmlparser");
    }

    #[test]
    fn test_to_camel_case() {
        assert_eq!(to_camel_case("snake_case"), "SnakeCase");
        assert_eq!(to_camel_case("simple"), "Simple");
        assert_eq!(to_camel_case("CamelCase"), "CamelCase");
    }

    #[test]
    fn test_to_upper_case() {
        assert_eq!(to_upper_case("snake_case"), "SNAKE_CASE");
        assert_eq!(to_upper_case("CamelCase"), "CAMELCASE");
        assert_eq!(to_upper_case("simple"), "SIMPLE");
    }

    #[test]
    fn test_naming_conventions_lint() {
        let lint = NamingConventionsLint;
        assert_eq!(lint.name(), "naming_conventions");
        assert_eq!(lint.default_level(), LintLevel::Warn);
    }
}
