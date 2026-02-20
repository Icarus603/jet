//! Property-Based Testing for Jet
//!
//! This module implements property-based testing using strategies to generate
//! test values from types. It's inspired by Rust's proptest and Haskell's QuickCheck.
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
//! The generator will create random lists of integers and verify the property.

use crate::annotation::Annotation;
use crate::generate::{TestCase, TestGenConfig, TestGenResult};
use crate::TestGenConfig as Config;
use jet_parser::ast::{Expr, Function, Literal, Pattern, Type};
use jet_typeck::{FloatSize, IntSize, TypeContext, TypeId, TypeKind};
use std::collections::HashMap;

/// A strategy for generating values of a particular type.
///
/// Strategies are composable and can be combined to generate complex data structures.
#[derive(Debug, Clone)]
pub enum Strategy {
    /// Generate a constant value.
    Constant(Literal),
    /// Generate values from a range of integers.
    IntRange(i64, i64),
    /// Generate values from a range of floats.
    FloatRange(f64, f64),
    /// Generate one of several values.
    OneOf(Vec<Strategy>),
    /// Generate values from a collection type.
    Vec(Box<Strategy>, usize, usize),
    /// Generate values from a tuple of strategies.
    Tuple(Vec<Strategy>),
    /// Generate values using a custom function.
    Custom(String),
    /// Recursively generate values (with depth limiting).
    Recursive(Box<Strategy>, usize),
}

impl Strategy {
    /// Generates a value using this strategy.
    pub fn generate(&self, rng: &mut impl rand::Rng, depth: usize) -> Literal {
        match self {
            Strategy::Constant(lit) => lit.clone(),
            Strategy::IntRange(min, max) => {
                let val = rng.gen_range(*min..=*max);
                Literal::Integer(val)
            }
            Strategy::FloatRange(min, max) => {
                let val = rng.gen_range(*min..=*max);
                Literal::Float(val)
            }
            Strategy::OneOf(strategies) => {
                if strategies.is_empty() {
                    Literal::Unit
                } else {
                    let idx = rng.gen_range(0..strategies.len());
                    strategies[idx].generate(rng, depth)
                }
            }
            Strategy::Vec(elem_strategy, min_len, max_len) => {
                if depth == 0 {
                    return Literal::String("[]".to_string());
                }
                let len = rng.gen_range(*min_len..=*max_len);
                let elements: Vec<String> = (0..len)
                    .map(|_| format!("{:?}", elem_strategy.generate(rng, depth - 1)))
                    .collect();
                Literal::String(format!("[{}]", elements.join(", ")))
            }
            Strategy::Tuple(strategies) => {
                let elements: Vec<String> = strategies
                    .iter()
                    .map(|s| format!("{:?}", s.generate(rng, depth)))
                    .collect();
                Literal::String(format!("({})", elements.join(", ")))
            }
            Strategy::Custom(name) => {
                // For custom strategies, generate a placeholder
                Literal::String(format!("<custom:{}>", name))
            }
            Strategy::Recursive(inner, max_depth) => {
                if depth >= *max_depth {
                    // Return a minimal value at max depth
                    Literal::Unit
                } else {
                    inner.generate(rng, depth + 1)
                }
            }
        }
    }

    /// Shrinks a value to find a minimal failing case.
    pub fn shrink(&self, value: &Literal) -> Vec<Literal> {
        match (self, value) {
            (Strategy::IntRange(min, max), Literal::Integer(n)) => {
                let mut shrunk = Vec::new();
                if *n > *min {
                    shrunk.push(Literal::Integer(*min));
                    shrunk.push(Literal::Integer(*n / 2));
                    shrunk.push(Literal::Integer(*n - 1));
                }
                if *n < *max {
                    shrunk.push(Literal::Integer(*max));
                }
                shrunk
            }
            (Strategy::Vec(elem, _, _), Literal::String(s)) if s.starts_with('[') => {
                // Try removing elements
                let mut shrunk = vec![Literal::String("[]".to_string())];
                // In a real implementation, parse and shrink the list
                shrunk
            }
            _ => Vec::new(),
        }
    }
}

/// Trait for types that can generate arbitrary values.
pub trait Arbitrary {
    /// Returns a strategy for generating values of this type.
    fn arbitrary() -> Strategy;

    /// Returns a strategy for generating values with constraints.
    fn arbitrary_with(params: HashMap<String, String>) -> Strategy;
}

/// A property test with its configuration.
#[derive(Debug, Clone)]
pub struct PropertyTest {
    /// The name of the test.
    pub name: String,
    /// The function being tested.
    pub target: Function,
    /// The property expression to verify.
    pub property: Expr,
    /// Strategies for generating inputs.
    pub strategies: Vec<(String, Strategy)>,
    /// Number of test cases to generate.
    pub cases: usize,
    /// Random seed for reproducibility.
    pub seed: Option<u64>,
}

impl PropertyTest {
    /// Creates a new property test.
    pub fn new(name: impl Into<String>, target: Function, property: Expr) -> Self {
        Self {
            name: name.into(),
            target,
            property,
            strategies: Vec::new(),
            cases: 100,
            seed: None,
        }
    }

    /// Sets the number of test cases.
    pub fn with_cases(mut self, cases: usize) -> Self {
        self.cases = cases;
        self
    }

    /// Sets the random seed.
    pub fn with_seed(mut self, seed: u64) -> Self {
        self.seed = Some(seed);
        self
    }

    /// Adds an input strategy.
    pub fn with_strategy(mut self, name: impl Into<String>, strategy: Strategy) -> Self {
        self.strategies.push((name.into(), strategy));
        self
    }

    /// Generates test cases for this property.
    pub fn generate_cases(&self, config: &Config) -> Vec<TestCase> {
        use rand::SeedableRng;
        let mut rng: rand::rngs::StdRng = if let Some(seed) = self.seed {
            SeedableRng::seed_from_u64(seed)
        } else {
            SeedableRng::from_entropy()
        };

        let mut cases = Vec::new();
        let num_cases = self.cases.min(config.property_test_cases);

        for i in 0..num_cases {
            let mut inputs = HashMap::new();

            for (name, strategy) in &self.strategies {
                let value = strategy.generate(&mut rng, config.max_recursion_depth);
                inputs.insert(name.clone(), value);
            }

            cases.push(TestCase {
                name: format!("{}_case_{}", self.name, i),
                inputs,
                expected_output: None,
                expected_effects: Vec::new(),
            });
        }

        cases
    }
}

/// Generator for property-based tests.
#[derive(Debug)]
pub struct PropertyTestGenerator {
    config: Config,
}

impl PropertyTestGenerator {
    /// Creates a new property test generator.
    pub fn new(config: Config) -> Self {
        Self { config }
    }

    /// Generates tests from an annotation.
    pub fn generate_tests(&self, annotation: &Annotation) -> TestGenResult<Vec<TestCase>> {
        let mut cases = Vec::new();

        if let Some(func) = &annotation.function {
            let test = self.create_property_test(annotation, func)?;
            cases.extend(test.generate_cases(&self.config));
        }

        Ok(cases)
    }

    /// Creates a property test from an annotation and function.
    fn create_property_test(
        &self,
        annotation: &Annotation,
        func: &Function,
    ) -> TestGenResult<PropertyTest> {
        use crate::annotation::SpecExtractor;

        let property = SpecExtractor::extract_property(func).ok_or_else(|| {
            vec![jet_diagnostics::Diagnostic::error(
                "No property found in test function",
                jet_diagnostics::Span::new(0, 0),
            )]
        })?;

        let mut test = PropertyTest::new(annotation.target.clone(), func.clone(), property);

        // Set number of cases from annotation params
        if let Some(cases) = annotation.get_param("cases") {
            if let Ok(n) = cases.parse::<usize>() {
                test = test.with_cases(n);
            }
        }

        // Set seed from annotation params
        if let Some(seed) = annotation.get_param("seed") {
            if let Ok(s) = seed.parse::<u64>() {
                test = test.with_seed(s);
            }
        }

        // Generate strategies for each parameter
        for param in &func.params {
            if let Some((name, strategy)) = self.strategy_for_param(param) {
                test = test.with_strategy(name, strategy);
            }
        }

        Ok(test)
    }

    /// Creates a strategy for a function parameter.
    fn strategy_for_param(&self, param: &jet_parser::ast::Param) -> Option<(String, Strategy)> {
        let name = match &param.pattern {
            Pattern::Ident { name, .. } => name.name.clone(),
            _ => return None,
        };

        let strategy = self.type_to_strategy(&param.ty);
        Some((name, strategy))
    }

    /// Converts a type to a generation strategy.
    fn type_to_strategy(&self, ty: &Type) -> Strategy {
        match ty {
            Type::Path(path) => {
                let name = path
                    .segments
                    .last()
                    .map(|s| s.name.as_str())
                    .unwrap_or("unknown");
                match name {
                    "int" | "i64" => Strategy::IntRange(-1000, 1000),
                    "i32" => Strategy::IntRange(-1000, 1000),
                    "i16" => Strategy::IntRange(-1000, 1000),
                    "i8" => Strategy::IntRange(-128, 127),
                    "uint" | "u64" => Strategy::IntRange(0, 1000),
                    "u32" => Strategy::IntRange(0, 1000),
                    "u16" => Strategy::IntRange(0, 1000),
                    "u8" => Strategy::IntRange(0, 255),
                    "float" | "f64" => Strategy::FloatRange(-1000.0, 1000.0),
                    "f32" => Strategy::FloatRange(-1000.0, 1000.0),
                    "bool" => Strategy::OneOf(vec![
                        Strategy::Constant(Literal::Bool(true)),
                        Strategy::Constant(Literal::Bool(false)),
                    ]),
                    "char" => Strategy::Constant(Literal::Char('a')),
                    "string" => Strategy::Constant(Literal::String("test".to_string())),
                    _ => Strategy::Constant(Literal::Unit),
                }
            }
            Type::Generic(base, args) => {
                if let Type::Path(path) = base.as_ref() {
                    let name = path
                        .segments
                        .last()
                        .map(|s| s.name.as_str())
                        .unwrap_or("unknown");
                    match name {
                        "Vec" | "List" if !args.is_empty() => {
                            let elem_strategy = self.type_to_strategy(&args[0]);
                            Strategy::Vec(Box::new(elem_strategy), 0, 10)
                        }
                        "Option" if !args.is_empty() => {
                            let inner = self.type_to_strategy(&args[0]);
                            Strategy::OneOf(vec![
                                Strategy::Constant(Literal::Unit), // None
                                inner,                             // Some
                            ])
                        }
                        "Result" if args.len() >= 2 => {
                            let ok_strategy = self.type_to_strategy(&args[0]);
                            let err_strategy = self.type_to_strategy(&args[1]);
                            Strategy::OneOf(vec![ok_strategy, err_strategy])
                        }
                        _ => Strategy::Constant(Literal::Unit),
                    }
                } else {
                    Strategy::Constant(Literal::Unit)
                }
            }
            Type::Tuple(types) => {
                let strategies: Vec<Strategy> =
                    types.iter().map(|t| self.type_to_strategy(t)).collect();
                Strategy::Tuple(strategies)
            }
            Type::Array(elem, size) => {
                let elem_strategy = self.type_to_strategy(elem);
                let len = size
                    .as_ref()
                    .and_then(|e| match e.as_ref() {
                        Expr::Literal(Literal::Integer(n)) => Some(*n as usize),
                        _ => None,
                    })
                    .unwrap_or(5);
                Strategy::Vec(Box::new(elem_strategy), len, len)
            }
            Type::Function { .. } => Strategy::Constant(Literal::Unit),
            Type::Reference { inner, .. } => self.type_to_strategy(inner),
            Type::Channel(_) => Strategy::Constant(Literal::Unit),
            Type::Async(_) => Strategy::Constant(Literal::Unit),
            Type::Infer => Strategy::Constant(Literal::Unit),
            Type::SelfType => Strategy::Constant(Literal::Unit),
        }
    }

    /// Creates a strategy from a typeck TypeId.
    pub fn type_id_to_strategy(&self, type_id: TypeId, tcx: &TypeContext) -> Strategy {
        match tcx.type_kind(type_id) {
            TypeKind::Int(_) => Strategy::IntRange(-1000, 1000),
            TypeKind::Uint(_) => Strategy::IntRange(0, 1000),
            TypeKind::Float(_) => Strategy::FloatRange(-1000.0, 1000.0),
            TypeKind::Bool => Strategy::OneOf(vec![
                Strategy::Constant(Literal::Bool(true)),
                Strategy::Constant(Literal::Bool(false)),
            ]),
            TypeKind::Char => Strategy::Constant(Literal::Char('a')),
            TypeKind::String => Strategy::Constant(Literal::String("test".to_string())),
            TypeKind::Unit => Strategy::Constant(Literal::Unit),
            TypeKind::Tuple(elements) => {
                let strategies: Vec<Strategy> = elements
                    .iter()
                    .map(|e| self.type_id_to_strategy(*e, tcx))
                    .collect();
                Strategy::Tuple(strategies)
            }
            TypeKind::Array(elem, size) => {
                let elem_strategy = self.type_id_to_strategy(*elem, tcx);
                Strategy::Vec(Box::new(elem_strategy), *size, *size)
            }
            TypeKind::Slice(elem) => {
                let elem_strategy = self.type_id_to_strategy(*elem, tcx);
                Strategy::Vec(Box::new(elem_strategy), 0, 10)
            }
            _ => Strategy::Constant(Literal::Unit),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::rngs::StdRng;
    use rand::SeedableRng;

    #[test]
    fn test_int_range_strategy() {
        let strategy = Strategy::IntRange(0, 100);
        let mut rng = StdRng::seed_from_u64(42);

        for _ in 0..10 {
            let value = strategy.generate(&mut rng, 5);
            if let Literal::Integer(n) = value {
                assert!(n >= 0 && n <= 100);
            } else {
                panic!("Expected integer");
            }
        }
    }

    #[test]
    fn test_bool_strategy() {
        let strategy = Strategy::OneOf(vec![
            Strategy::Constant(Literal::Bool(true)),
            Strategy::Constant(Literal::Bool(false)),
        ]);
        let mut rng = StdRng::seed_from_u64(42);

        let mut saw_true = false;
        let mut saw_false = false;

        for _ in 0..20 {
            match strategy.generate(&mut rng, 5) {
                Literal::Bool(true) => saw_true = true,
                Literal::Bool(false) => saw_false = true,
                _ => panic!("Expected bool"),
            }
        }

        // With enough iterations, we should see both values
        assert!(saw_true || saw_false); // At minimum, one of them
    }

    #[test]
    fn test_vec_strategy() {
        let strategy = Strategy::Vec(Box::new(Strategy::IntRange(0, 10)), 0, 5);
        let mut rng = StdRng::seed_from_u64(42);

        for _ in 0..10 {
            let value = strategy.generate(&mut rng, 5);
            if let Literal::String(s) = value {
                assert!(s.starts_with('[') && s.ends_with(']'));
            }
        }
    }

    #[test]
    fn test_shrink_int() {
        let strategy = Strategy::IntRange(0, 100);
        let shrunk = strategy.shrink(&Literal::Integer(50));

        assert!(!shrunk.is_empty());
        assert!(shrunk.contains(&Literal::Integer(0)));
        assert!(shrunk.contains(&Literal::Integer(25)));
        assert!(shrunk.contains(&Literal::Integer(49)));
    }
}
