//! Jet Test CLI - Automatic Test Generation and Execution
//!
//! This tool provides automatic test generation for the Jet programming language.
//!
//! # Usage
//!
//! ```bash
//! # Generate tests for a file
//! jettest generate src/my_module.jet
//!
//! # Run generated tests
//! jettest run src/my_module.jet
//!
//! # Run with mutation testing
//! jettest run --mutation src/my_module.jet
//!
//! # Generate property tests only
//! jettest generate --properties src/my_module.jet
//! ```
#![allow(unused_imports)]
#![allow(clippy::nonminimal_bool, clippy::too_many_arguments)]

use anyhow::{Context, Result};
use clap::{Parser, Subcommand, ValueEnum};
use jet_diagnostics::Diagnostic;
use jet_lexer::Lexer;
use jettest::{
    AnnotationParser, EffectTestGenerator, JetTestGenerator, MutationTester, PropertyTestGenerator,
    TestGenConfig, TestSuite,
};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Parser)]
#[command(name = "jettest")]
#[command(about = "Automatic test generation for the Jet programming language")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Increase verbosity
    #[arg(short, long, global = true, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Suppress output
    #[arg(short, long, global = true)]
    quiet: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate tests from source code
    Generate {
        /// Source file to generate tests for
        file: PathBuf,

        /// Output directory for generated tests
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Generate only property-based tests
        #[arg(long)]
        properties: bool,

        /// Generate only effect-based tests
        #[arg(long)]
        effects: bool,

        /// Number of test cases to generate
        #[arg(short, long, default_value = "100")]
        cases: usize,

        /// Random seed for reproducibility
        #[arg(long)]
        seed: Option<u64>,

        /// Maximum recursion depth for type generation
        #[arg(long, default_value = "5")]
        max_depth: usize,
    },

    /// Run generated tests
    Run {
        /// Source file to test
        file: PathBuf,

        /// Run mutation testing
        #[arg(long)]
        mutation: bool,

        /// Filter tests by name
        #[arg(short, long)]
        filter: Option<String>,

        /// Stop on first failure
        #[arg(long)]
        fail_fast: bool,

        /// Number of parallel threads
        #[arg(short, long)]
        threads: Option<usize>,

        /// Output format
        #[arg(short, long, value_enum, default_value = "text")]
        format: OutputFormat,
    },

    /// Analyze test coverage
    Analyze {
        /// Source file to analyze
        file: PathBuf,

        /// Show detailed analysis
        #[arg(long)]
        detailed: bool,
    },

    /// List available annotations in a file
    List {
        /// Source file to analyze
        file: PathBuf,
    },
}

#[derive(ValueEnum, Clone, Debug)]
enum OutputFormat {
    /// Human-readable text output
    Text,
    /// JSON output
    Json,
    /// JUnit XML output
    Junit,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Generate {
            file,
            output,
            properties,
            effects,
            cases,
            seed,
            max_depth,
        } => cmd_generate(
            file, output, properties, effects, cases, seed, max_depth, cli.quiet,
        ),
        Commands::Run {
            file,
            mutation,
            filter,
            fail_fast,
            threads,
            format,
        } => cmd_run(
            file, mutation, filter, fail_fast, threads, format, cli.quiet,
        ),
        Commands::Analyze { file, detailed } => cmd_analyze(file, detailed),
        Commands::List { file } => cmd_list(file),
    }
}

fn cmd_generate(
    file: PathBuf,
    output: Option<PathBuf>,
    properties: bool,
    effects: bool,
    cases: usize,
    seed: Option<u64>,
    max_depth: usize,
    quiet: bool,
) -> Result<()> {
    if !quiet {
        println!("Generating tests for: {}", file.display());
    }

    // Parse the source file
    let module = parse_jet_file(&file)?;

    // Configure test generation
    let config = TestGenConfig {
        generate_properties: properties || (!properties && !effects),
        generate_effect_tests: effects || (!properties && !effects),
        run_mutation_tests: false,
        property_test_cases: cases,
        max_recursion_depth: max_depth,
        seed,
        output_dir: output.clone(),
    };

    // Generate tests
    let mut generator = JetTestGenerator::new(config);
    let suite = generator.generate_tests(&module).map_err(|diagnostics| {
        for diag in diagnostics {
            eprintln!("Error: {:?}", diag);
        }
        anyhow::anyhow!("Failed to generate tests")
    })?;

    if !quiet {
        println!("\n{}", suite.summary());
    }

    // Output generated tests
    let output_path = output.unwrap_or_else(|| {
        let mut path = file.clone();
        path.set_extension("test.jet");
        path
    });

    let generated_code = suite.to_jet_source();
    fs::write(&output_path, generated_code)
        .with_context(|| format!("Failed to write to {}", output_path.display()))?;

    if !quiet {
        println!("\nGenerated tests written to: {}", output_path.display());
    }

    Ok(())
}

fn cmd_run(
    file: PathBuf,
    mutation: bool,
    filter: Option<String>,
    fail_fast: bool,
    threads: Option<usize>,
    format: OutputFormat,
    quiet: bool,
) -> Result<()> {
    if !quiet {
        println!("Running tests for: {}", file.display());
    }

    // Parse the source file
    let module = parse_jet_file(&file)?;

    // Configure test generation
    let config = TestGenConfig {
        generate_properties: true,
        generate_effect_tests: true,
        run_mutation_tests: mutation,
        ..Default::default()
    };

    // Generate tests
    let mut generator = JetTestGenerator::new(config);
    let suite = generator.generate_tests(&module).map_err(|diagnostics| {
        for diag in diagnostics {
            eprintln!("Error: {:?}", diag);
        }
        anyhow::anyhow!("Failed to generate tests")
    })?;

    // Run tests
    let results = generator.run_tests(&suite);

    // Generate report
    use jettest::runner::{TestConfig, TestRunner};
    let runner = TestRunner::new(TestConfig {
        filter,
        fail_fast,
        threads: threads.unwrap_or_else(num_cpus::get),
        ..Default::default()
    });

    let report = runner.generate_report(&results);

    // Output report
    match format {
        OutputFormat::Text => {
            if !quiet {
                println!("\n{}", report.to_string());
            }
        }
        OutputFormat::Json => {
            println!("{}", serde_json::to_string_pretty(&report.to_json())?);
        }
        OutputFormat::Junit => {
            println!("{}", report.to_junit_xml());
        }
    }

    // Exit with error code if tests failed
    if report.statistics.failed > 0 {
        std::process::exit(1);
    }

    Ok(())
}

fn cmd_analyze(file: PathBuf, detailed: bool) -> Result<()> {
    println!("Analyzing: {}", file.display());

    // Parse the source file
    let module = parse_jet_file(&file)?;

    // Parse annotations
    let mut parser = AnnotationParser::new();
    let annotations = parser.parse_module(&module).map_err(|diagnostics| {
        for diag in diagnostics {
            eprintln!("Error: {:?}", diag);
        }
        anyhow::anyhow!("Failed to parse annotations")
    })?;

    println!("\nFound {} annotation(s):", annotations.len());

    for annotation in &annotations {
        println!("  - {:?}: {}", annotation.kind, annotation.target);

        if detailed {
            if let Some(func) = &annotation.function {
                println!("    Function: {}", func.name.name);
                println!("    Parameters: {}", func.params.len());
                if !func.effects.is_empty() {
                    println!("    Effects: {:?}", func.effects);
                }
            }

            if !annotation.params.is_empty() {
                println!("    Parameters:");
                for (key, value) in &annotation.params {
                    println!("      {}: {}", key, value);
                }
            }
        }
    }

    // Analyze effect paths
    let effect_generator = EffectTestGenerator::new(TestGenConfig::default());
    let mut total_paths = 0;

    for annotation in &annotations {
        if let Some(func) = &annotation.function {
            let paths = effect_generator.analyze_effect_paths(func);
            total_paths += paths.len();

            if detailed && !paths.is_empty() {
                println!("\n  Effect paths for {}:", annotation.target);
                for path in &paths {
                    println!("    - {}", path.description());
                }
            }
        }
    }

    println!("\nTotal effect paths: {}", total_paths);

    Ok(())
}

fn cmd_list(file: PathBuf) -> Result<()> {
    println!("Annotations in: {}", file.display());

    // Parse the source file
    let module = parse_jet_file(&file)?;

    // Parse annotations
    let mut parser = AnnotationParser::new();
    let annotations = parser.parse_module(&module).map_err(|diagnostics| {
        for diag in diagnostics {
            eprintln!("Error: {:?}", diag);
        }
        anyhow::anyhow!("Failed to parse annotations")
    })?;

    if annotations.is_empty() {
        println!("\nNo test annotations found.");
        println!("\nAvailable annotations:");
        println!("  @test_auto          - Generate property-based tests");
        println!("  @test_for(Effect)   - Generate effect-based tests");
        println!("  @mutation_test      - Enable mutation testing");
    } else {
        for annotation in annotations {
            match annotation.kind {
                jettest::AnnotationKind::TestAuto => {
                    println!("\n@test_auto");
                    println!("  Target: {}", annotation.target);
                    if let Some(func) = annotation.function {
                        println!("  Function: {}", func.name.name);
                    }
                }
                jettest::AnnotationKind::TestFor(effect) => {
                    println!("\n@test_for({})", effect);
                    println!("  Target: {}", annotation.target);
                }
                jettest::AnnotationKind::MutationTest => {
                    println!("\n@mutation_test");
                    println!("  Target: {}", annotation.target);
                }
            }
        }
    }

    Ok(())
}

/// Parses a Jet source file and returns the AST module.
fn parse_jet_file(path: &Path) -> Result<jet_parser::ast::Module> {
    let source = fs::read_to_string(path)
        .with_context(|| format!("Failed to read file: {}", path.display()))?;

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();

    jet_parser::Parser::new(tokens)
        .parse_module()
        .map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cli_parse() {
        let cli = Cli::parse_from(["jettest", "generate", "test.jet"]);
        match cli.command {
            Commands::Generate { file, .. } => {
                assert_eq!(file, PathBuf::from("test.jet"));
            }
            _ => panic!("Expected Generate command"),
        }
    }

    #[test]
    fn test_output_format() {
        let fmt = OutputFormat::Text;
        match fmt {
            OutputFormat::Text => {}
            _ => panic!("Expected Text format"),
        }
    }
}
