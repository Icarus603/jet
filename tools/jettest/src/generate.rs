//! Test Generation Infrastructure
//!
//! This module provides the core types and functions for generating test cases
//! from Jet source code. It supports property-based tests, effect-based tests,
//! and mutation testing.

use crate::mutation::Mutation;
use jet_parser::ast::Literal;
use std::collections::HashMap;

/// Configuration for test generation.
pub type TestGenConfig = crate::TestGenConfig;

/// Result type for test generation operations.
pub type TestGenResult<T> = crate::TestGenResult<T>;

/// A generated test case.
#[derive(Debug, Clone)]
pub struct TestCase {
    /// The name of the test case.
    pub name: String,
    /// Input values for the test.
    pub inputs: HashMap<String, Literal>,
    /// Expected output (None if not specified).
    pub expected_output: Option<Literal>,
    /// Expected effects to be performed.
    pub expected_effects: Vec<String>,
}

impl TestCase {
    /// Creates a new test case.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            inputs: HashMap::new(),
            expected_output: None,
            expected_effects: Vec::new(),
        }
    }

    /// Adds an input value.
    pub fn with_input(mut self, name: impl Into<String>, value: Literal) -> Self {
        self.inputs.insert(name.into(), value);
        self
    }

    /// Sets the expected output.
    pub fn with_expected_output(mut self, output: Literal) -> Self {
        self.expected_output = Some(output);
        self
    }

    /// Adds an expected effect.
    pub fn with_expected_effect(mut self, effect: impl Into<String>) -> Self {
        self.expected_effects.push(effect.into());
        self
    }

    /// Generates Jet code for this test case.
    pub fn to_jet_code(&self, func_name: &str) -> String {
        let mut code = format!("fn {}():\n", self.name);
        code.push_str("    # Setup\n");

        // Generate input bindings
        for (name, value) in &self.inputs {
            code.push_str(&format!("    let {} = {}\n", name, literal_to_jet(value)));
        }

        // Generate the function call
        let args: Vec<String> = self.inputs.keys().cloned().collect();
        code.push_str(&format!("\n    # Execute\n"));
        code.push_str(&format!(
            "    let result = {}({})\n",
            func_name,
            args.join(", ")
        ));

        // Generate assertions
        code.push_str(&format!("\n    # Verify\n"));
        if let Some(expected) = &self.expected_output {
            code.push_str(&format!(
                "    assert result == {}\n",
                literal_to_jet(expected)
            ));
        }

        if !self.expected_effects.is_empty() {
            code.push_str("    # Expected effects: ");
            code.push_str(&self.expected_effects.join(", "));
            code.push('\n');
        }

        code
    }
}

/// Converts a literal to Jet source code.
fn literal_to_jet(lit: &Literal) -> String {
    match lit {
        Literal::Integer(n) => n.to_string(),
        Literal::Float(f) => f.to_string(),
        Literal::String(s) => format!("\"{}\"", s.replace('"', "\\\"")),
        Literal::Char(c) => format!("'{}'", c),
        Literal::Bool(b) => b.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

/// A suite of generated tests.
#[derive(Debug, Clone, Default)]
pub struct TestSuite {
    /// Property-based test cases.
    pub property_tests: Vec<TestCase>,
    /// Effect-based test cases.
    pub effect_tests: Vec<TestCase>,
    /// Mutation test cases.
    pub mutation_tests: Vec<TestCase>,
    /// Mutations for mutation testing.
    pub mutations: Vec<Mutation>,
    /// Metadata about the test suite.
    pub metadata: TestSuiteMetadata,
}

/// Metadata about a test suite.
#[derive(Debug, Clone, Default)]
pub struct TestSuiteMetadata {
    /// The source file or module.
    pub source: String,
    /// Number of functions tested.
    pub function_count: usize,
    /// Generation timestamp.
    pub generated_at: String,
    /// Generator version.
    pub version: String,
}

impl TestSuite {
    /// Creates a new empty test suite.
    pub fn new() -> Self {
        Self {
            property_tests: Vec::new(),
            effect_tests: Vec::new(),
            mutation_tests: Vec::new(),
            mutations: Vec::new(),
            metadata: TestSuiteMetadata {
                version: env!("CARGO_PKG_VERSION").to_string(),
                ..Default::default()
            },
        }
    }

    /// Adds test cases to the suite.
    pub fn add_tests(&mut self, tests: Vec<TestCase>) {
        // Categorize tests based on their names
        for test in tests {
            if test.name.contains("_success") || test.name.contains("_error_") {
                self.effect_tests.push(test);
            } else {
                self.property_tests.push(test);
            }
        }
    }

    /// Adds mutations to the suite.
    pub fn add_mutations(&mut self, mutations: Vec<Mutation>) {
        self.mutations.extend(mutations);
    }

    /// Returns all test cases in the suite.
    pub fn all_tests(&self) -> Vec<&TestCase> {
        let mut all = Vec::new();
        all.extend(&self.property_tests);
        all.extend(&self.effect_tests);
        all.extend(&self.mutation_tests);
        all
    }

    /// Returns the total number of tests.
    pub fn total_tests(&self) -> usize {
        self.property_tests.len() + self.effect_tests.len() + self.mutation_tests.len()
    }

    /// Generates a summary of the test suite.
    pub fn summary(&self) -> String {
        format!(
            "Test Suite Summary:\n\
             - Property tests: {}\n\
             - Effect tests: {}\n\
             - Mutation tests: {}\n\
             - Mutations: {}\n\
             - Total: {}",
            self.property_tests.len(),
            self.effect_tests.len(),
            self.mutation_tests.len(),
            self.mutations.len(),
            self.total_tests()
        )
    }

    /// Generates Jet source code for all tests.
    pub fn to_jet_source(&self) -> String {
        let mut source = String::new();

        // Add header comment
        source.push_str("# Auto-generated test file\n");
        source.push_str(&format!("# Generated: {}\n", self.metadata.generated_at));
        source.push_str(&format!("# Version: {}\n\n", self.metadata.version));

        // Add property tests
        if !self.property_tests.is_empty() {
            source.push_str("# Property-based tests\n");
            for test in &self.property_tests {
                source.push_str(&test.to_jet_code("test_func"));
                source.push('\n');
            }
        }

        // Add effect tests
        if !self.effect_tests.is_empty() {
            source.push_str("# Effect-based tests\n");
            for test in &self.effect_tests {
                source.push_str(&test.to_jet_code("test_func"));
                source.push('\n');
            }
        }

        source
    }

    /// Merges another test suite into this one.
    pub fn merge(&mut self, other: TestSuite) {
        self.property_tests.extend(other.property_tests);
        self.effect_tests.extend(other.effect_tests);
        self.mutation_tests.extend(other.mutation_tests);
        self.mutations.extend(other.mutations);
    }
}

/// A test generator for a specific function.
#[derive(Debug)]
pub struct TestGenerator {
    config: TestGenConfig,
}

impl TestGenerator {
    /// Creates a new test generator.
    pub fn new(config: TestGenConfig) -> Self {
        Self { config }
    }

    /// Generates a test case for a specific input.
    pub fn generate_for_input(
        &self,
        name: impl Into<String>,
        inputs: HashMap<String, Literal>,
    ) -> TestCase {
        TestCase::new(name).with_inputs(inputs)
    }

    /// Generates edge case tests for a type.
    pub fn generate_edge_cases(&self, _ty: &jet_typeck::TypeId) -> Vec<TestCase> {
        // In a real implementation, this would analyze the type
        // and generate edge cases like min/max values, empty collections, etc.
        Vec::new()
    }
}

/// Extension trait for TestCase to add inputs in bulk.
pub trait TestCaseExt {
    /// Adds multiple inputs at once.
    fn with_inputs(self, inputs: HashMap<String, Literal>) -> Self;
}

impl TestCaseExt for TestCase {
    fn with_inputs(mut self, inputs: HashMap<String, Literal>) -> Self {
        self.inputs.extend(inputs);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_test_case_creation() {
        let test = TestCase::new("test_add")
            .with_input("a", Literal::Integer(1))
            .with_input("b", Literal::Integer(2))
            .with_expected_output(Literal::Integer(3));

        assert_eq!(test.name, "test_add");
        assert_eq!(test.inputs.get("a"), Some(&Literal::Integer(1)));
        assert_eq!(test.inputs.get("b"), Some(&Literal::Integer(2)));
        assert_eq!(test.expected_output, Some(Literal::Integer(3)));
    }

    #[test]
    fn test_literal_to_jet() {
        assert_eq!(literal_to_jet(&Literal::Integer(42)), "42");
        assert_eq!(literal_to_jet(&Literal::Bool(true)), "true");
        assert_eq!(
            literal_to_jet(&Literal::String("hello".to_string())),
            "\"hello\""
        );
        assert_eq!(literal_to_jet(&Literal::Char('a')), "'a'");
        assert_eq!(literal_to_jet(&Literal::Unit), "()");
    }

    #[test]
    fn test_test_suite() {
        let mut suite = TestSuite::new();

        let test1 = TestCase::new("test1");
        let test2 = TestCase::new("test2");

        suite.add_tests(vec![test1, test2]);

        assert_eq!(suite.total_tests(), 2);
        assert!(suite.summary().contains("Total: 2"));
    }

    #[test]
    fn test_test_case_to_jet_code() {
        let test = TestCase::new("test_add")
            .with_input("a", Literal::Integer(1))
            .with_input("b", Literal::Integer(2))
            .with_expected_output(Literal::Integer(3));

        let code = test.to_jet_code("add");

        assert!(code.contains("fn test_add():"));
        assert!(code.contains("let a = 1"));
        assert!(code.contains("let b = 2"));
        assert!(code.contains("let result = add("));
        assert!(code.contains("assert result == 3"));
    }

    #[test]
    fn test_suite_merge() {
        let mut suite1 = TestSuite::new();
        suite1.add_tests(vec![TestCase::new("test1")]);

        let mut suite2 = TestSuite::new();
        suite2.add_tests(vec![TestCase::new("test2")]);

        suite1.merge(suite2);

        assert_eq!(suite1.total_tests(), 2);
    }
}
