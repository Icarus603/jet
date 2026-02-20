//! Effect-Based Testing for Jet
//!
//! This module generates tests that cover all effect paths including both
//! success and error cases. It analyzes the effect types of functions and
//! generates test cases that trigger each possible effect.
//!
//! # Example
//!
//! ```jet
//! @test_for(IOError)
//! fn read_file_test(path: string) -> string ! IOError:
//!     return read_file(path)
//! ```
//!
//! This will generate tests for:
//! - Success case: file exists and is readable
//! - IOError cases: file not found, permission denied, etc.

use crate::annotation::Annotation;
use crate::generate::{TestCase, TestGenConfig, TestGenResult};
use crate::TestGenConfig as Config;
use jet_effect::{BuiltinEffect, EffectInstance, EffectSet};
use jet_parser::ast::{Expr, Function, Type};
use std::collections::HashMap;

/// Represents a path through effect handling.
#[derive(Debug, Clone, PartialEq)]
pub enum EffectPath {
    /// Success path - no effects performed.
    Success,
    /// Error path with a specific effect.
    Error(String),
    /// Multiple effects in sequence.
    Sequence(Vec<EffectPath>),
    /// Choice between different effect paths.
    Choice(Vec<EffectPath>),
}

impl EffectPath {
    /// Returns a human-readable description of this path.
    pub fn description(&self) -> String {
        match self {
            EffectPath::Success => "success".to_string(),
            EffectPath::Error(effect) => format!("error({})", effect),
            EffectPath::Sequence(paths) => {
                let parts: Vec<String> = paths.iter().map(|p| p.description()).collect();
                format!("seq({})", parts.join(" → "))
            }
            EffectPath::Choice(paths) => {
                let parts: Vec<String> = paths.iter().map(|p| p.description()).collect();
                format!("choice({})", parts.join(" | "))
            }
        }
    }

    /// Flattens a sequence of paths into a list.
    pub fn flatten(&self) -> Vec<&EffectPath> {
        match self {
            EffectPath::Sequence(paths) => {
                let mut result = Vec::new();
                for path in paths {
                    result.extend(path.flatten());
                }
                result
            }
            _ => vec![self],
        }
    }
}

/// A test case for effect paths.
#[derive(Debug, Clone)]
pub struct EffectTestCase {
    /// The base test case.
    pub base: TestCase,
    /// The effect path being tested.
    pub path: EffectPath,
    /// Expected effects to be performed.
    pub expected_effects: Vec<String>,
    /// Whether this is a success or error case.
    pub should_succeed: bool,
}

/// Generator for effect-based tests.
#[derive(Debug)]
pub struct EffectTestGenerator {
    config: Config,
}

impl EffectTestGenerator {
    /// Creates a new effect test generator.
    pub fn new(config: Config) -> Self {
        Self { config }
    }

    /// Generates tests from an annotation targeting a specific effect.
    pub fn generate_tests(
        &self,
        annotation: &Annotation,
        effect: &str,
    ) -> TestGenResult<Vec<TestCase>> {
        let mut cases = Vec::new();

        if let Some(func) = &annotation.function {
            // Generate success case
            cases.push(self.create_success_case(annotation, func));

            // Generate error cases for the target effect
            let error_cases = self.create_error_cases(annotation, func, effect);
            cases.extend(error_cases);

            // Generate boundary cases
            let boundary_cases = self.create_boundary_cases(annotation, func, effect);
            cases.extend(boundary_cases);
        }

        Ok(cases)
    }

    /// Creates a success case test.
    fn create_success_case(&self, annotation: &Annotation, func: &Function) -> TestCase {
        let mut inputs = HashMap::new();

        // Generate valid inputs that should not trigger effects
        for param in &func.params {
            let value = self.generate_valid_input(&param.ty);
            if let Some(name) = self.param_name(&param.pattern) {
                inputs.insert(name, value);
            }
        }

        TestCase {
            name: format!("{}_success", annotation.target),
            inputs,
            expected_output: None,
            expected_effects: vec![],
        }
    }

    /// Creates error cases for a specific effect.
    fn create_error_cases(
        &self,
        annotation: &Annotation,
        func: &Function,
        effect: &str,
    ) -> Vec<TestCase> {
        let mut cases = Vec::new();

        // Get error variants for this effect
        let error_variants = self.get_error_variants(effect);

        for (i, variant) in error_variants.iter().enumerate() {
            let mut inputs = HashMap::new();

            // Generate inputs that trigger this error variant
            for param in &func.params {
                let value = self.generate_error_input(&param.ty, effect, variant);
                if let Some(name) = self.param_name(&param.pattern) {
                    inputs.insert(name, value);
                }
            }

            cases.push(TestCase {
                name: format!("{}_error_{}_{}", annotation.target, effect, i),
                inputs,
                expected_output: None,
                expected_effects: vec![effect.to_string()],
            });
        }

        cases
    }

    /// Creates boundary cases for effect testing.
    fn create_boundary_cases(
        &self,
        annotation: &Annotation,
        func: &Function,
        effect: &str,
    ) -> Vec<TestCase> {
        let mut cases = Vec::new();

        // Generate boundary values that might trigger effects
        let boundary_inputs = self.generate_boundary_inputs(func, effect);

        for (i, inputs) in boundary_inputs.iter().enumerate() {
            cases.push(TestCase {
                name: format!("{}_boundary_{}_{}", annotation.target, effect, i),
                inputs: inputs.clone(),
                expected_output: None,
                expected_effects: vec![effect.to_string()],
            });
        }

        cases
    }

    /// Gets error variants for a given effect type.
    fn get_error_variants(&self, effect: &str) -> Vec<String> {
        match effect {
            "IOError" => vec![
                "FileNotFound".to_string(),
                "PermissionDenied".to_string(),
                "AlreadyExists".to_string(),
                "InvalidInput".to_string(),
                "UnexpectedEOF".to_string(),
                "WriteZero".to_string(),
                "Interrupted".to_string(),
                "Other".to_string(),
            ],
            "NetworkError" => vec![
                "ConnectionRefused".to_string(),
                "ConnectionReset".to_string(),
                "ConnectionAborted".to_string(),
                "NotConnected".to_string(),
                "AddrInUse".to_string(),
                "AddrNotAvailable".to_string(),
                "BrokenPipe".to_string(),
                "TimedOut".to_string(),
            ],
            "ParseError" => vec![
                "InvalidSyntax".to_string(),
                "UnexpectedToken".to_string(),
                "MissingField".to_string(),
                "InvalidValue".to_string(),
            ],
            "Overflow" => vec![
                "AddOverflow".to_string(),
                "SubOverflow".to_string(),
                "MulOverflow".to_string(),
                "DivByZero".to_string(),
            ],
            "Async" => vec!["Cancelled".to_string(), "TimedOut".to_string()],
            _ => vec!["Generic".to_string()],
        }
    }

    /// Generates a valid input for a type.
    fn generate_valid_input(&self, ty: &Type) -> jet_parser::ast::Literal {
        use jet_parser::ast::Literal;

        match ty {
            Type::Path(path) => {
                let name = path
                    .segments
                    .last()
                    .map(|s| s.name.as_str())
                    .unwrap_or("unknown");
                match name {
                    "int" | "i64" | "i32" | "i16" | "i8" => Literal::Integer(42),
                    "uint" | "u64" | "u32" | "u16" | "u8" => Literal::Integer(42),
                    "float" | "f64" | "f32" => Literal::Float(3.14),
                    "bool" => Literal::Bool(true),
                    "char" => Literal::Char('a'),
                    "string" => Literal::String("valid_input".to_string()),
                    _ => Literal::Unit,
                }
            }
            Type::Generic(base, _) => {
                if let Type::Path(path) = base.as_ref() {
                    let name = path
                        .segments
                        .last()
                        .map(|s| s.name.as_str())
                        .unwrap_or("unknown");
                    match name {
                        "Option" => Literal::Unit, // None
                        "Result" => Literal::Unit, // Ok(())
                        "Vec" | "List" => Literal::String("[]".to_string()),
                        _ => Literal::Unit,
                    }
                } else {
                    Literal::Unit
                }
            }
            Type::Array(_, _) => Literal::String("[]".to_string()),
            Type::Tuple(_) => Literal::String("()".to_string()),
            _ => Literal::Unit,
        }
    }

    /// Generates an input that triggers an error.
    fn generate_error_input(
        &self,
        ty: &Type,
        effect: &str,
        variant: &str,
    ) -> jet_parser::ast::Literal {
        use jet_parser::ast::Literal;

        // Generate inputs based on the effect and variant
        match (effect, variant) {
            ("IOError", "FileNotFound") => {
                if self.is_string_type(ty) {
                    Literal::String("/nonexistent/path/to/file.txt".to_string())
                } else {
                    self.generate_valid_input(ty)
                }
            }
            ("IOError", "PermissionDenied") => {
                if self.is_string_type(ty) {
                    Literal::String("/root/secret.txt".to_string())
                } else {
                    self.generate_valid_input(ty)
                }
            }
            ("NetworkError", "ConnectionRefused") => {
                if self.is_string_type(ty) {
                    Literal::String("localhost:99999".to_string())
                } else {
                    self.generate_valid_input(ty)
                }
            }
            ("NetworkError", "TimedOut") => {
                if self.is_string_type(ty) {
                    Literal::String("192.0.2.1:80".to_string()) // TEST-NET-1 address
                } else {
                    self.generate_valid_input(ty)
                }
            }
            ("ParseError", _) => {
                if self.is_string_type(ty) {
                    Literal::String("invalid syntax {{[".to_string())
                } else {
                    self.generate_valid_input(ty)
                }
            }
            ("Overflow", "DivByZero") => {
                if self.is_numeric_type(ty) {
                    Literal::Integer(0)
                } else {
                    self.generate_valid_input(ty)
                }
            }
            _ => self.generate_valid_input(ty),
        }
    }

    /// Generates boundary inputs for a function.
    fn generate_boundary_inputs(
        &self,
        func: &Function,
        _effect: &str,
    ) -> Vec<HashMap<String, jet_parser::ast::Literal>> {
        let mut inputs_list = Vec::new();

        // Generate empty input set
        let empty: HashMap<String, jet_parser::ast::Literal> = HashMap::new();
        inputs_list.push(empty);

        // Generate inputs with boundary values
        let mut boundary = HashMap::new();
        for param in &func.params {
            if let Some(name) = self.param_name(&param.pattern) {
                let value = self.generate_boundary_value(&param.ty);
                boundary.insert(name, value);
            }
        }
        inputs_list.push(boundary);

        inputs_list
    }

    /// Generates a boundary value for a type.
    fn generate_boundary_value(&self, ty: &Type) -> jet_parser::ast::Literal {
        use jet_parser::ast::Literal;

        match ty {
            Type::Path(path) => {
                let name = path
                    .segments
                    .last()
                    .map(|s| s.name.as_str())
                    .unwrap_or("unknown");
                match name {
                    "int" | "i64" => Literal::Integer(i64::MAX),
                    "i32" => Literal::Integer(i32::MAX as i64),
                    "i16" => Literal::Integer(i16::MAX as i64),
                    "i8" => Literal::Integer(i8::MAX as i64),
                    "uint" | "u64" => Literal::Integer(-1), // Wraps to max
                    "u32" => Literal::Integer(u32::MAX as i64),
                    "u16" => Literal::Integer(u16::MAX as i64),
                    "u8" => Literal::Integer(u8::MAX as i64),
                    "float" | "f64" => Literal::Float(f64::INFINITY),
                    "f32" => Literal::Float(f64::INFINITY),
                    "string" => Literal::String("".to_string()),
                    _ => Literal::Unit,
                }
            }
            _ => Literal::Unit,
        }
    }

    /// Checks if a type is a string.
    fn is_string_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::Path(path) if path.segments.last().map(|s| s.name.as_str()) == Some("string"))
    }

    /// Checks if a type is numeric.
    fn is_numeric_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::Path(path) if {
            let name = path.segments.last().map(|s| s.name.as_str()).unwrap_or("");
            matches!(name, "int" | "i64" | "i32" | "i16" | "i8" |
                     "uint" | "u64" | "u32" | "u16" | "u8" |
                     "float" | "f64" | "f32")
        })
    }

    /// Extracts the name from a pattern.
    fn param_name(&self, pattern: &jet_parser::ast::Pattern) -> Option<String> {
        match pattern {
            jet_parser::ast::Pattern::Ident { name, .. } => Some(name.name.clone()),
            _ => None,
        }
    }

    /// Analyzes a function to determine all possible effect paths.
    pub fn analyze_effect_paths(&self, func: &Function) -> Vec<EffectPath> {
        let mut paths = vec![EffectPath::Success];

        // Get declared effects from function signature
        for effect in &func.effects {
            if let Type::Path(path) = effect {
                let effect_name = path
                    .segments
                    .last()
                    .map(|s| s.name.clone())
                    .unwrap_or_default();
                paths.push(EffectPath::Error(effect_name));
            }
        }

        paths
    }

    /// Generates a test handler for an effect.
    pub fn generate_effect_handler(&self, effect: &str, variant: &str) -> String {
        format!(
            r#"handle {{
    {{
        // Test body that may perform {}
    }}
}} with
    {}({}) => resume (Error("{}"))
    _ => resume (Error("unexpected effect"))
end"#,
            effect, effect, variant, variant
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span;
    use jet_parser::ast::{Ident, Pattern};

    fn create_test_function() -> Function {
        Function {
            public: false,
            attributes: vec![],
            name: Ident::new("test_func", Span::default()),
            generics: vec![],
            params: vec![],
            return_type: None,
            effects: vec![],
            where_clause: vec![],
            contract: None,
            body: Expr::Block(jet_parser::ast::Block {
                stmts: vec![],
                expr: None,
                span: Span::default(),
            }),
            span: Span::default(),
        }
    }

    #[test]
    fn test_effect_path_description() {
        let path = EffectPath::Sequence(vec![
            EffectPath::Success,
            EffectPath::Error("IOError".to_string()),
        ]);

        assert_eq!(path.description(), "seq(success → error(IOError))");
    }

    #[test]
    fn test_get_io_error_variants() {
        let generator = EffectTestGenerator::new(Config::default());
        let variants = generator.get_error_variants("IOError");

        assert!(variants.contains(&"FileNotFound".to_string()));
        assert!(variants.contains(&"PermissionDenied".to_string()));
        assert!(variants.len() >= 8);
    }

    #[test]
    fn test_generate_error_input_file_not_found() {
        let generator = EffectTestGenerator::new(Config::default());
        let ty = Type::Path(jet_parser::ast::Path::single(Ident::new(
            "string",
            Span::default(),
        )));

        let input = generator.generate_error_input(&ty, "IOError", "FileNotFound");

        match input {
            jet_parser::ast::Literal::String(s) => {
                assert!(s.contains("nonexistent"));
            }
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn test_analyze_effect_paths() {
        let generator = EffectTestGenerator::new(Config::default());
        let mut func = create_test_function();

        // Add an IOError effect
        func.effects
            .push(Type::Path(jet_parser::ast::Path::single(Ident::new(
                "IOError",
                Span::default(),
            ))));

        let paths = generator.analyze_effect_paths(&func);

        assert_eq!(paths.len(), 2);
        assert!(matches!(paths[0], EffectPath::Success));
        assert!(matches!(paths[1], EffectPath::Error(ref e) if e == "IOError"));
    }
}
