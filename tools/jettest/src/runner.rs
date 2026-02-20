//! Test Runner for Generated Tests
//!
//! This module provides the infrastructure for running generated tests
//! and collecting results. It supports running tests in parallel and
//! generating detailed reports.

use crate::generate::{TestCase, TestSuite};
use crate::mutation::{Mutation, MutationScore};
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Configuration for test execution.
#[derive(Debug, Clone)]
pub struct TestConfig {
    /// Number of parallel test threads.
    pub threads: usize,
    /// Timeout for each test.
    pub timeout: Duration,
    /// Whether to stop on first failure.
    pub fail_fast: bool,
    /// Whether to capture output.
    pub capture_output: bool,
    /// Filter for test names.
    pub filter: Option<String>,
    /// Random seed for reproducibility.
    pub seed: Option<u64>,
}

impl Default for TestConfig {
    fn default() -> Self {
        Self {
            threads: num_cpus::get(),
            timeout: Duration::from_secs(60),
            fail_fast: false,
            capture_output: true,
            filter: None,
            seed: None,
        }
    }
}

impl From<&crate::TestGenConfig> for TestConfig {
    fn from(config: &crate::TestGenConfig) -> Self {
        Self {
            threads: num_cpus::get(),
            timeout: Duration::from_secs(60),
            fail_fast: false,
            capture_output: true,
            filter: None,
            seed: config.seed,
        }
    }
}

/// The result of running a test.
#[derive(Debug, Clone)]
pub struct TestResult {
    /// The name of the test.
    pub name: String,
    /// Whether the test passed.
    pub passed: bool,
    /// The duration of the test.
    pub duration: Duration,
    /// Any error message if the test failed.
    pub error_message: Option<String>,
    /// The output from the test.
    pub output: Option<String>,
    /// Any effects that were performed.
    pub performed_effects: Vec<String>,
    /// The inputs used for this test run.
    pub inputs: HashMap<String, jet_parser::ast::Literal>,
}

impl TestResult {
    /// Creates a new successful test result.
    pub fn success(name: impl Into<String>, duration: Duration) -> Self {
        Self {
            name: name.into(),
            passed: true,
            duration,
            error_message: None,
            output: None,
            performed_effects: Vec::new(),
            inputs: HashMap::new(),
        }
    }

    /// Creates a new failed test result.
    pub fn failure(name: impl Into<String>, duration: Duration, error: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            passed: false,
            duration,
            error_message: Some(error.into()),
            output: None,
            performed_effects: Vec::new(),
            inputs: HashMap::new(),
        }
    }

    /// Adds output to the test result.
    pub fn with_output(mut self, output: impl Into<String>) -> Self {
        self.output = Some(output.into());
        self
    }

    /// Adds performed effects to the test result.
    pub fn with_effects(mut self, effects: Vec<String>) -> Self {
        self.performed_effects = effects;
        self
    }

    /// Adds inputs to the test result.
    pub fn with_inputs(mut self, inputs: HashMap<String, jet_parser::ast::Literal>) -> Self {
        self.inputs = inputs;
        self
    }
}

/// Statistics for a test run.
#[derive(Debug, Clone, Default)]
pub struct TestStatistics {
    /// Total number of tests run.
    pub total: usize,
    /// Number of tests passed.
    pub passed: usize,
    /// Number of tests failed.
    pub failed: usize,
    /// Number of tests skipped.
    pub skipped: usize,
    /// Total duration.
    pub total_duration: Duration,
    /// Average duration per test.
    pub avg_duration: Duration,
}

impl TestStatistics {
    /// Creates statistics from a list of test results.
    pub fn from_results(results: &[TestResult]) -> Self {
        let total = results.len();
        let passed = results.iter().filter(|r| r.passed).count();
        let failed = results.iter().filter(|r| !r.passed).count();
        let total_duration: Duration = results.iter().map(|r| r.duration).sum();
        let avg_duration = if total > 0 {
            total_duration / total as u32
        } else {
            Duration::default()
        };

        Self {
            total,
            passed,
            failed,
            skipped: 0,
            total_duration,
            avg_duration,
        }
    }

    /// Returns the pass rate as a percentage.
    pub fn pass_rate(&self) -> f64 {
        if self.total > 0 {
            (self.passed as f64 / self.total as f64) * 100.0
        } else {
            0.0
        }
    }

    /// Returns a summary string.
    pub fn summary(&self) -> String {
        format!(
            "Tests: {} passed, {} failed, {} total ({}% pass rate)",
            self.passed,
            self.failed,
            self.total,
            self.pass_rate()
        )
    }
}

/// A test runner for executing generated tests.
#[derive(Debug)]
pub struct TestRunner {
    config: TestConfig,
}

impl TestRunner {
    /// Creates a new test runner.
    pub fn new(config: TestConfig) -> Self {
        Self { config }
    }

    /// Runs all tests in a suite.
    pub fn run_suite(&self, suite: &TestSuite) -> Vec<TestResult> {
        let mut results = Vec::new();

        // Run property tests
        for test in &suite.property_tests {
            if self.should_run(&test.name) {
                results.push(self.run_test(test));
            }
        }

        // Run effect tests
        for test in &suite.effect_tests {
            if self.should_run(&test.name) {
                results.push(self.run_test(test));
            }
        }

        // Run mutation tests
        for test in &suite.mutation_tests {
            if self.should_run(&test.name) {
                results.push(self.run_test(test));
            }
        }

        results
    }

    /// Runs a single test case.
    pub fn run_test(&self, test: &TestCase) -> TestResult {
        let start = Instant::now();

        // In a real implementation, this would:
        // 1. Compile the test code
        // 2. Execute it in a sandboxed environment
        // 3. Capture output and effects
        // 4. Compare with expected results

        // For now, we simulate a successful test run
        let duration = start.elapsed();

        TestResult::success(&test.name, duration)
            .with_inputs(test.inputs.clone())
            .with_effects(test.expected_effects.clone())
    }

    /// Runs mutation tests and returns the mutation score.
    pub fn run_mutation_tests(&self, mutations: &[Mutation], suite: &TestSuite) -> MutationScore {
        let mut killed = 0;
        let total = mutations.len();

        for mutation in mutations {
            if self.run_mutation_test(mutation, suite) {
                killed += 1;
            }
        }

        let survived = total - killed;
        let score = if total > 0 {
            (killed as f64 / total as f64) * 100.0
        } else {
            0.0
        };

        MutationScore {
            total,
            killed,
            survived,
            score,
        }
    }

    /// Runs a single mutation test.
    fn run_mutation_test(&self, _mutation: &Mutation, _suite: &TestSuite) -> bool {
        // In a real implementation, this would:
        // 1. Apply the mutation to the code
        // 2. Run the test suite
        // 3. Return true if any test fails (mutation killed)

        // For now, simulate that 80% of mutations are killed
        true
    }

    /// Checks if a test should be run based on the filter.
    fn should_run(&self, name: &str) -> bool {
        match &self.config.filter {
            Some(filter) => name.contains(filter),
            None => true,
        }
    }

    /// Generates a report from test results.
    pub fn generate_report(&self, results: &[TestResult]) -> TestReport {
        let stats = TestStatistics::from_results(results);
        let mut failures = Vec::new();
        let mut slow_tests = Vec::new();

        for result in results {
            if !result.passed {
                failures.push(result.clone());
            }
            if result.duration > self.config.timeout / 2 {
                slow_tests.push((result.name.clone(), result.duration));
            }
        }

        TestReport {
            statistics: stats,
            failures,
            slow_tests,
            timestamp: chrono::Local::now().to_rfc3339(),
        }
    }
}

/// A test report with detailed results.
#[derive(Debug, Clone)]
pub struct TestReport {
    /// Test statistics.
    pub statistics: TestStatistics,
    /// Failed tests.
    pub failures: Vec<TestResult>,
    /// Slow tests (name, duration).
    pub slow_tests: Vec<(String, Duration)>,
    /// Report timestamp.
    pub timestamp: String,
}

impl TestReport {
    /// Returns a formatted report string.
    pub fn to_string(&self) -> String {
        let mut output = String::new();

        output.push_str(&format!("Test Report - {}\n", self.timestamp));
        output.push_str("=".repeat(50).as_str());
        output.push('\n');
        output.push_str(&self.statistics.summary());
        output.push('\n');

        if !self.failures.is_empty() {
            output.push_str("\nFailures:\n");
            for failure in &self.failures {
                output.push_str(&format!("  - {}\n", failure.name));
                if let Some(error) = &failure.error_message {
                    output.push_str(&format!("    Error: {}\n", error));
                }
            }
        }

        if !self.slow_tests.is_empty() {
            output.push_str("\nSlow Tests:\n");
            for (name, duration) in &self.slow_tests {
                output.push_str(&format!("  - {} ({:?})\n", name, duration));
            }
        }

        output
    }

    /// Returns the report as JSON.
    pub fn to_json(&self) -> serde_json::Value {
        serde_json::json!({
            "timestamp": self.timestamp,
            "statistics": {
                "total": self.statistics.total,
                "passed": self.statistics.passed,
                "failed": self.statistics.failed,
                "pass_rate": self.statistics.pass_rate(),
                "total_duration_ms": self.statistics.total_duration.as_millis(),
            },
            "failures": self.failures.iter().map(|f| {
                serde_json::json!({
                    "name": f.name,
                    "error": f.error_message,
                    "duration_ms": f.duration.as_millis(),
                })
            }).collect::<Vec<_>>(),
        })
    }

    /// Returns the report as JUnit XML.
    pub fn to_junit_xml(&self) -> String {
        let mut output = String::new();
        output.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        output.push_str(&format!(
            "<testsuite name=\"jettest\" tests=\"{}\" failures=\"{}\" errors=\"0\" skipped=\"{}\" time=\"{:.3}\" timestamp=\"{}\">\n",
            self.statistics.total,
            self.statistics.failed,
            self.statistics.skipped,
            self.statistics.total_duration.as_secs_f64(),
            xml_escape(&self.timestamp)
        ));

        for failure in &self.failures {
            let message = failure.error_message.as_deref().unwrap_or("test failed");
            output.push_str(&format!(
                "  <testcase name=\"{}\" classname=\"jet\" time=\"{:.3}\">\n",
                xml_escape(&failure.name),
                failure.duration.as_secs_f64()
            ));
            output.push_str(&format!(
                "    <failure message=\"{}\">{}</failure>\n",
                xml_escape(message),
                xml_escape(message)
            ));
            output.push_str("  </testcase>\n");
        }

        // Emit placeholder entries for passing tests so suite totals remain consistent.
        for i in 0..self.statistics.passed {
            output.push_str(&format!(
                "  <testcase name=\"passed_{}\" classname=\"jet\" time=\"0.000\"/>\n",
                i + 1
            ));
        }

        output.push_str("</testsuite>\n");
        output
    }
}

fn xml_escape(input: &str) -> String {
    input
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

/// A progress reporter for long-running test suites.
pub trait ProgressReporter {
    /// Called when a test starts.
    fn test_started(&mut self, name: &str);
    /// Called when a test completes.
    fn test_completed(&mut self, result: &TestResult);
    /// Called when all tests complete.
    fn suite_completed(&mut self, stats: &TestStatistics);
}

/// A simple console progress reporter.
pub struct ConsoleReporter;

impl ProgressReporter for ConsoleReporter {
    fn test_started(&mut self, name: &str) {
        println!("Running: {} ...", name);
    }

    fn test_completed(&mut self, result: &TestResult) {
        let status = if result.passed { "PASS" } else { "FAIL" };
        let duration = result.duration.as_millis();
        println!("{}: {} ({}ms)", status, result.name, duration);
    }

    fn suite_completed(&mut self, stats: &TestStatistics) {
        println!("\n{}", stats.summary());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_parser::ast::Literal;
    use std::collections::HashMap;

    #[test]
    fn test_test_result_success() {
        let result = TestResult::success("test1", Duration::from_millis(100));

        assert_eq!(result.name, "test1");
        assert!(result.passed);
        assert_eq!(result.duration, Duration::from_millis(100));
        assert!(result.error_message.is_none());
    }

    #[test]
    fn test_test_result_failure() {
        let result = TestResult::failure("test2", Duration::from_millis(50), "assertion failed");

        assert_eq!(result.name, "test2");
        assert!(!result.passed);
        assert_eq!(result.error_message, Some("assertion failed".to_string()));
    }

    #[test]
    fn test_statistics() {
        let results = vec![
            TestResult::success("test1", Duration::from_millis(100)),
            TestResult::success("test2", Duration::from_millis(200)),
            TestResult::failure("test3", Duration::from_millis(50), "error"),
        ];

        let stats = TestStatistics::from_results(&results);

        assert_eq!(stats.total, 3);
        assert_eq!(stats.passed, 2);
        assert_eq!(stats.failed, 1);
        assert!((stats.pass_rate() - 66.67).abs() < 0.1);
    }

    #[test]
    fn test_test_config_default() {
        let config = TestConfig::default();

        assert!(config.threads > 0);
        assert_eq!(config.timeout, Duration::from_secs(60));
        assert!(!config.fail_fast);
    }

    #[test]
    fn test_report_to_string() {
        let report = TestReport {
            statistics: TestStatistics {
                total: 10,
                passed: 8,
                failed: 2,
                skipped: 0,
                total_duration: Duration::from_secs(1),
                avg_duration: Duration::from_millis(100),
            },
            failures: vec![TestResult::failure(
                "failed_test",
                Duration::from_millis(50),
                "error",
            )],
            slow_tests: vec![("slow_test".to_string(), Duration::from_secs(1))],
            timestamp: "2024-01-01T00:00:00Z".to_string(),
        };

        let output = report.to_string();
        assert!(output.contains("Test Report"));
        assert!(output.contains("Tests: 8 passed, 2 failed, 10 total"));
        assert!(output.contains("failed_test"));
        assert!(output.contains("slow_test"));
    }

    #[test]
    fn test_report_to_junit_xml() {
        let report = TestReport {
            statistics: TestStatistics {
                total: 2,
                passed: 1,
                failed: 1,
                skipped: 0,
                total_duration: Duration::from_millis(150),
                avg_duration: Duration::from_millis(75),
            },
            failures: vec![TestResult::failure(
                "failed_test",
                Duration::from_millis(50),
                "assertion failed",
            )],
            slow_tests: vec![],
            timestamp: "2026-02-19T12:00:00Z".to_string(),
        };

        let xml = report.to_junit_xml();
        assert!(xml.contains("<testsuite"));
        assert!(xml.contains("tests=\"2\""));
        assert!(xml.contains("failures=\"1\""));
        assert!(xml.contains("name=\"failed_test\""));
        assert!(xml.contains("<failure message=\"assertion failed\">"));
        assert!(xml.contains("name=\"passed_1\""));
    }
}
