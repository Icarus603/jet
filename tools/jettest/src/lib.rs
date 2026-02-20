//! Jet Test - Automatic Test Generation for the Jet Programming Language
//!
//! This crate provides automatic test generation capabilities for Jet, including:
//!
//! - **Property-based testing**: Generate tests from type information using proptest-style generation
//! - **Effect-based testing**: Generate tests covering all effect paths (success and error)
//! - **Mutation testing**: Implement mutation operators to test test-suite effectiveness
//!
//! # Example
//!
//! ```jet
//! @test_auto
//! fn reverse_properties(list: [int]) {
//!     assert list == reverse(reverse(list))
//! }
//! ```
//!
//! This will automatically generate property tests using proptest-style generation.
#![allow(dead_code, unused_imports, unused_variables, unused_mut)]
#![allow(
    clippy::approx_constant,
    clippy::inherent_to_string,
    clippy::manual_range_contains,
    clippy::useless_format
)]

pub mod annotation;
pub mod effect;
pub mod generate;
pub mod mutation;
pub mod property;
pub mod runner;

pub use annotation::{Annotation, AnnotationKind, AnnotationParser};
pub use effect::{EffectPath, EffectTestGenerator};
pub use generate::{TestCase, TestGenerator, TestSuite};
pub use mutation::{Mutation, MutationOperator, MutationTester};
pub use property::{Arbitrary, PropertyTest, PropertyTestGenerator, Strategy};
pub use runner::{TestConfig, TestResult, TestRunner};

use jet_diagnostics::Diagnostic;
use jet_parser::ast::Module;

/// Result type for test generation operations.
pub type TestGenResult<T> = Result<T, Vec<Diagnostic>>;

/// Configuration for automatic test generation.
#[derive(Debug, Clone)]
pub struct TestGenConfig {
    /// Whether to generate property-based tests.
    pub generate_properties: bool,
    /// Whether to generate effect-based tests.
    pub generate_effect_tests: bool,
    /// Whether to run mutation testing.
    pub run_mutation_tests: bool,
    /// Number of test cases to generate per property.
    pub property_test_cases: usize,
    /// Maximum depth for recursive type generation.
    pub max_recursion_depth: usize,
    /// Seed for random number generation (for reproducibility).
    pub seed: Option<u64>,
    /// Output directory for generated tests.
    pub output_dir: Option<std::path::PathBuf>,
}

impl Default for TestGenConfig {
    fn default() -> Self {
        Self {
            generate_properties: true,
            generate_effect_tests: true,
            run_mutation_tests: false,
            property_test_cases: 100,
            max_recursion_depth: 5,
            seed: None,
            output_dir: None,
        }
    }
}

impl TestGenConfig {
    /// Creates a new configuration with all features enabled.
    pub fn full() -> Self {
        Self {
            generate_properties: true,
            generate_effect_tests: true,
            run_mutation_tests: true,
            property_test_cases: 1000,
            max_recursion_depth: 10,
            seed: None,
            output_dir: None,
        }
    }

    /// Creates a new configuration for quick testing.
    pub fn quick() -> Self {
        Self {
            generate_properties: true,
            generate_effect_tests: false,
            run_mutation_tests: false,
            property_test_cases: 10,
            max_recursion_depth: 3,
            seed: Some(42),
            output_dir: None,
        }
    }
}

/// The main test generator for Jet modules.
pub struct JetTestGenerator {
    config: TestGenConfig,
    annotation_parser: AnnotationParser,
    property_generator: PropertyTestGenerator,
    effect_generator: EffectTestGenerator,
    mutation_tester: MutationTester,
}

impl JetTestGenerator {
    /// Creates a new test generator with the given configuration.
    pub fn new(config: TestGenConfig) -> Self {
        Self {
            config: config.clone(),
            annotation_parser: AnnotationParser::new(),
            property_generator: PropertyTestGenerator::new(config.clone()),
            effect_generator: EffectTestGenerator::new(config.clone()),
            mutation_tester: MutationTester::new(config),
        }
    }

    /// Generates tests for a Jet module.
    ///
    /// This is the main entry point for test generation. It parses annotations
    /// in the module and generates appropriate tests based on the configuration.
    pub fn generate_tests(&mut self, module: &Module) -> TestGenResult<TestSuite> {
        let mut suite = TestSuite::new();

        // Parse annotations from the module
        let annotations = self.annotation_parser.parse_module(module)?;

        for annotation in annotations {
            match annotation.kind {
                AnnotationKind::TestAuto => {
                    if self.config.generate_properties {
                        let tests = self.property_generator.generate_tests(&annotation)?;
                        suite.add_tests(tests);
                    }
                }
                AnnotationKind::TestFor(ref effect) => {
                    if self.config.generate_effect_tests {
                        let tests = self.effect_generator.generate_tests(&annotation, effect)?;
                        suite.add_tests(tests);
                    }
                }
                AnnotationKind::MutationTest => {
                    if self.config.run_mutation_tests {
                        let mutations = self.mutation_tester.generate_mutations(&annotation)?;
                        suite.add_mutations(mutations);
                    }
                }
            }
        }

        Ok(suite)
    }

    /// Runs the generated tests and returns the results.
    pub fn run_tests(&self, suite: &TestSuite) -> Vec<TestResult> {
        let runner = TestRunner::new(TestConfig::from(&self.config));
        runner.run_suite(suite)
    }
}

/// Generates tests for a module and returns the test suite.
///
/// This is a convenience function for the common case of generating tests
/// with default configuration.
pub fn generate_tests(module: &Module) -> TestGenResult<TestSuite> {
    let mut generator = JetTestGenerator::new(TestGenConfig::default());
    generator.generate_tests(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_parser::ast::{Function, Ident, ModuleItem, Pattern};

    fn create_empty_module() -> Module {
        Module {
            attributes: vec![],
            items: vec![],
            span: jet_lexer::Span::default(),
        }
    }

    #[test]
    fn test_default_config() {
        let config = TestGenConfig::default();
        assert!(config.generate_properties);
        assert!(config.generate_effect_tests);
        assert!(!config.run_mutation_tests);
        assert_eq!(config.property_test_cases, 100);
    }

    #[test]
    fn test_full_config() {
        let config = TestGenConfig::full();
        assert!(config.generate_properties);
        assert!(config.generate_effect_tests);
        assert!(config.run_mutation_tests);
        assert_eq!(config.property_test_cases, 1000);
    }

    #[test]
    fn test_quick_config() {
        let config = TestGenConfig::quick();
        assert!(config.generate_properties);
        assert!(!config.generate_effect_tests);
        assert!(!config.run_mutation_tests);
        assert_eq!(config.property_test_cases, 10);
        assert_eq!(config.seed, Some(42));
    }
}
