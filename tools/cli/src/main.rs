//! Jet CLI - The command-line interface for the Jet programming language
//!
//! This crate provides the main `jet` command with subcommands for:
//! - `new`: Create new Jet projects
//! - `build`: Build Jet projects
//! - `run`: Build and run Jet projects
//! - `test`: Run tests
//! - `check`: Type check without building
//! - `fmt`: Format code
//! - `lsp`: Start LSP server
//! - `doc`: Generate documentation
//! - `publish`: Publish packages to registry
//! - `clean`: Clean build artifacts
//! - `update`: Update dependencies

#![allow(dead_code)]
#![allow(deprecated)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::ptr_arg)]
#![allow(clippy::manual_strip)]
#![allow(clippy::needless_range_loop)]
#![allow(clippy::collapsible_else_if)]

#[cfg(target_os = "windows")]
compile_error!("Jet does not support Windows in 1.0. Please use macOS or Linux.");

use anyhow::{Context, Result};
use clap::{Parser, Subcommand, ValueEnum};
use std::io::Write;
use std::path::{Path, PathBuf};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use tokio::process::Command as TokioCommand;

// Internal modules
mod dependencies;
mod errors;
mod manifest;
mod pipeline;
mod project;
mod publish;

use errors::{print_success, print_warning, CliError, ErrorCode, ErrorReporter};
use jet_linker::BuildProfile;
use manifest::{read_project_manifest, Manifest};
use pipeline::{check_project, compile_project, PipelineConfig};
use project::{find_project, Project, ProjectOrWorkspace};
use publish::{PublishConfig, Publisher};

#[derive(Parser)]
#[command(name = "jet")]
#[command(about = "The Jet programming language")]
#[command(version)]
#[command(arg_required_else_help = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Change to directory before executing
    #[arg(short = 'C', global = true)]
    change_dir: Option<PathBuf>,

    /// Path to manifest (jet.toml)
    #[arg(long, global = true)]
    manifest_path: Option<PathBuf>,

    /// Control colored output
    #[arg(long, global = true, value_enum)]
    color: Option<ColorOption>,

    /// Run without network access
    #[arg(long, global = true)]
    offline: bool,

    /// Increase verbosity
    #[arg(short, long, global = true, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Decrease verbosity
    #[arg(short, long, global = true, action = clap::ArgAction::Count)]
    quiet: u8,
}

#[derive(ValueEnum, Clone, Copy, Debug)]
enum ColorOption {
    Auto,
    Always,
    Never,
}

#[derive(Subcommand)]
enum Commands {
    /// Create a new Jet project
    New {
        /// Project name
        name: String,
        /// Create a library (default: binary)
        #[arg(long)]
        lib: bool,
        /// Create a workspace
        #[arg(long)]
        workspace: bool,
        /// Project path
        #[arg(short, long)]
        path: Option<PathBuf>,
        /// Use a project template
        #[arg(long)]
        template: Option<String>,
        /// Initialize version control (git or none)
        #[arg(long, default_value = "git")]
        vcs: String,
    },

    /// Build the project
    Build {
        /// Build in release mode
        #[arg(long)]
        release: bool,
        /// Target triple
        #[arg(long)]
        target: Option<String>,
        /// Number of parallel jobs
        #[arg(short, long)]
        jobs: Option<usize>,
        /// Enable feature flags
        #[arg(long)]
        features: Vec<String>,
        /// Enable all features
        #[arg(long)]
        all_features: bool,
        /// Disable default features
        #[arg(long)]
        no_default_features: bool,
        /// Output directory
        #[arg(long)]
        out_dir: Option<PathBuf>,
        /// Emit intermediate representation
        #[arg(long, value_enum)]
        emit: Option<EmitOption>,
        /// Don't link, just compile
        #[arg(long)]
        no_link: bool,
    },

    /// Build and run the project
    Run {
        /// Build in release mode
        #[arg(long)]
        release: bool,
        /// Enable feature flags
        #[arg(long)]
        features: Vec<String>,
        /// Arguments to pass to the program
        #[arg(last = true)]
        args: Vec<String>,
    },

    /// Run tests
    Test {
        /// Run only tests matching pattern
        pattern: Option<String>,
        /// Run in release mode
        #[arg(long)]
        release: bool,
        /// Enable feature flags
        #[arg(long)]
        features: Vec<String>,
        /// Run library tests only
        #[arg(long)]
        lib: bool,
        /// Run doc tests
        #[arg(long)]
        doc: bool,
        /// Run benchmarks
        #[arg(long)]
        bench: bool,
        /// Don't capture test output
        #[arg(long)]
        nocapture: bool,
        /// Number of test threads
        #[arg(long)]
        test_threads: Option<usize>,
    },

    /// Type check without building
    Check {
        /// Enable feature flags
        #[arg(long)]
        features: Vec<String>,
        /// Enable all features
        #[arg(long)]
        all_features: bool,
        /// Library only
        #[arg(long)]
        lib: bool,
        /// Target triple
        #[arg(long)]
        target: Option<String>,
    },

    /// Format code
    Fmt {
        /// Check formatting without modifying files
        #[arg(long)]
        check: bool,
        /// Files or directories to format
        files: Vec<PathBuf>,
        /// Output mode
        #[arg(long, value_enum)]
        emit: Option<EmitMode>,
    },

    /// Start LSP server
    Lsp {
        /// LSP server port (for TCP mode)
        #[arg(long)]
        port: Option<u16>,
    },

    /// Generate documentation
    Doc {
        /// Open docs in browser after generation
        #[arg(long)]
        open: bool,
        /// Don't generate dependency docs
        #[arg(long)]
        no_deps: bool,
        /// Include private items
        #[arg(long)]
        document_private_items: bool,
        /// Output directory
        #[arg(long)]
        output: Option<PathBuf>,
        /// Start local server
        #[arg(long)]
        serve: bool,
        /// Server port
        #[arg(long, default_value = "8080")]
        port: u16,
    },

    /// Run the linter
    Lint {
        /// Automatically fix issues where possible
        #[arg(long)]
        fix: bool,
        /// Treat specific lint as error
        #[arg(long)]
        deny: Vec<String>,
        /// Treat specific lint as warning
        #[arg(long)]
        warn: Vec<String>,
        /// Allow specific lint
        #[arg(long)]
        allow: Vec<String>,
    },

    /// Publish package to registry
    Publish {
        /// Simulate publish without uploading
        #[arg(long)]
        dry_run: bool,
        /// Registry authentication token
        #[arg(long)]
        token: Option<String>,
        /// Target registry
        #[arg(long)]
        registry: Option<String>,
        /// Allow uncommitted changes
        #[arg(long)]
        allow_dirty: bool,
        /// Skip pre-publish verification
        #[arg(long)]
        no_verify: bool,
    },

    /// Login to a registry
    Login {
        /// Registry URL
        #[arg(long)]
        registry: Option<String>,
        /// Authentication token
        token: Option<String>,
    },

    /// Logout from a registry
    Logout {
        /// Registry URL
        #[arg(long)]
        registry: Option<String>,
    },

    /// Update dependencies
    Update {
        /// Update specific package
        #[arg(long)]
        package: Option<String>,
        /// Show what would be updated
        #[arg(long)]
        dry_run: bool,
        /// Aggressive update
        #[arg(long)]
        aggressive: bool,
    },

    /// Clean build artifacts
    Clean {
        /// Clean documentation
        #[arg(long)]
        doc: bool,
        /// Clean release artifacts only
        #[arg(long)]
        release: bool,
    },

    /// Manage Jet installation
    #[command(subcommand)]
    Jet(JetCommands),
}

#[derive(Subcommand)]
enum JetCommands {
    /// Install a package
    Install {
        /// Package name
        name: String,
        /// Package version
        #[arg(long)]
        version: Option<String>,
        /// Registry URL
        #[arg(long)]
        registry: Option<String>,
    },
    /// Search for packages
    Search {
        /// Search query
        query: String,
        /// Limit results
        #[arg(short, long, default_value = "10")]
        limit: usize,
        /// Registry URL
        #[arg(long)]
        registry: Option<String>,
    },
    /// Show package information
    Show {
        /// Package name
        name: String,
        /// Registry URL
        #[arg(long)]
        registry: Option<String>,
    },
}

#[derive(ValueEnum, Clone, Copy, Debug)]
enum EmitOption {
    Ir,
    Asm,
    Obj,
}

#[derive(ValueEnum, Clone, Copy, Debug)]
enum EmitMode {
    Files,
    Stdout,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    // Handle color option
    if let Some(color) = cli.color {
        match color {
            ColorOption::Always => std::env::set_var("CLICOLOR_FORCE", "1"),
            ColorOption::Never => std::env::set_var("NO_COLOR", "1"),
            ColorOption::Auto => {}
        }
    }

    // Change directory if requested
    if let Some(dir) = cli.change_dir {
        if let Err(e) = std::env::set_current_dir(&dir) {
            eprintln!("Error: Cannot change to directory {}: {}", dir.display(), e);
            std::process::exit(1);
        }
    }

    let verbose = cli.verbose > cli.quiet;

    let result = match cli.command {
        Commands::New {
            name,
            lib,
            workspace,
            path,
            template,
            vcs,
        } => cmd_new(name, lib, workspace, path, template, vcs).await,
        Commands::Build {
            release,
            target,
            jobs,
            features,
            all_features,
            no_default_features,
            out_dir,
            emit,
            no_link,
        } => {
            cmd_build(
                release,
                target,
                jobs,
                features,
                all_features,
                no_default_features,
                out_dir,
                emit,
                no_link,
                verbose,
            )
            .await
        }
        Commands::Run {
            release,
            features,
            args,
        } => cmd_run(release, features, args, verbose).await,
        Commands::Test {
            pattern,
            release,
            features,
            lib,
            doc,
            bench,
            nocapture,
            test_threads,
        } => {
            cmd_test(
                pattern,
                release,
                features,
                lib,
                doc,
                bench,
                nocapture,
                test_threads,
            )
            .await
        }
        Commands::Check {
            features,
            all_features,
            lib,
            target,
        } => cmd_check(features, all_features, lib, target, verbose).await,
        Commands::Fmt { check, files, emit } => cmd_fmt(check, files, emit).await,
        Commands::Lsp { port } => cmd_lsp(port).await,
        Commands::Doc {
            open,
            no_deps,
            document_private_items,
            output,
            serve,
            port,
        } => cmd_doc(open, no_deps, document_private_items, output, serve, port).await,
        Commands::Lint {
            fix,
            deny,
            warn,
            allow,
        } => cmd_lint(fix, deny, warn, allow).await,
        Commands::Publish {
            dry_run,
            token,
            registry,
            allow_dirty,
            no_verify,
        } => cmd_publish(dry_run, token, registry, allow_dirty, no_verify).await,
        Commands::Login { registry, token } => cmd_login(registry, token).await,
        Commands::Logout { registry } => cmd_logout(registry).await,
        Commands::Update {
            package,
            dry_run,
            aggressive,
        } => cmd_update(package, dry_run, aggressive).await,
        Commands::Clean { doc, release } => cmd_clean(doc, release).await,
        Commands::Jet(JetCommands::Install {
            name,
            version,
            registry,
        }) => cmd_install(name, version, registry).await,
        Commands::Jet(JetCommands::Search {
            query,
            limit,
            registry,
        }) => cmd_search(query, limit, registry).await,
        Commands::Jet(JetCommands::Show { name, registry }) => cmd_show(name, registry).await,
    };

    if let Err(e) = result {
        if let Some(cli_err) = e.downcast_ref::<CliError>() {
            cli_err.print();
            std::process::exit(cli_err.code.exit_code());
        } else {
            let error = CliError::new(e.to_string());
            error.print();
            std::process::exit(ErrorCode::General.exit_code());
        }
    }
}

// =============================================================================
// New Command
// =============================================================================

async fn cmd_new(
    name: String,
    lib: bool,
    workspace: bool,
    path: Option<PathBuf>,
    _template: Option<String>,
    vcs: String,
) -> Result<()> {
    let path = path.unwrap_or_else(|| std::env::current_dir().unwrap().join(&name));

    // Validate project name
    if !is_valid_project_name(&name) {
        return Err(CliError::new(format!(
            "Invalid project name '{}'. Project names must start with a letter \
             and contain only alphanumeric characters, hyphens, and underscores.",
            name
        ))
        .into());
    }

    // Check if directory already exists
    if path.exists() {
        return Err(CliError::new(format!("Directory '{}' already exists", path.display())).into());
    }

    // Create directory structure
    std::fs::create_dir_all(&path)?;
    std::fs::create_dir_all(path.join("src"))?;

    // Create jet.toml
    let toml = if workspace {
        format!(
            r#"[package]
name = "{}"
version = "1.0.0"
edition = "2024"

[workspace]
members = ["crates/*"]
"#,
            name
        )
    } else {
        format!(
            r#"[package]
name = "{}"
version = "1.0.0"
edition = "2024"

[dependencies]
"#,
            name
        )
    };
    std::fs::write(path.join("jet.toml"), toml)?;

    // Create source files
    if workspace {
        // Create placeholder directories for workspace
        std::fs::create_dir_all(path.join("crates"))?;
    } else if lib {
        let lib_content = r#"pub fn hello() -> string:
    "Hello from Jet!"
"#;
        std::fs::write(path.join("src/lib.jet"), lib_content)?;
    } else {
        let main_content = r#"import stdlib

fn main():
    print("Hello, Jet!")
"#;
        std::fs::write(path.join("src/main.jet"), main_content)?;
    }

    // Create .gitignore
    std::fs::write(path.join(".gitignore"), "target/\n")?;

    // Initialize git if requested
    if vcs == "git" {
        std::process::Command::new("git")
            .args(["init", "-q"])
            .current_dir(&path)
            .status()
            .ok();
    }

    // Print success message
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
    write!(&mut stdout, "Created")?;
    stdout.reset()?;

    if workspace {
        writeln!(&mut stdout, " workspace '{}'", name)?;
    } else if lib {
        writeln!(&mut stdout, " library '{}'", name)?;
    } else {
        writeln!(&mut stdout, " binary '{}'", name)?;
    }

    writeln!(&mut stdout, "  {}", path.canonicalize()?.display())?;

    Ok(())
}

fn is_valid_project_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }

    let first = name.chars().next().unwrap();
    if !first.is_ascii_alphabetic() {
        return false;
    }

    name.chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
}

// =============================================================================
// Build Command
// =============================================================================

async fn cmd_build(
    release: bool,
    target: Option<String>,
    jobs: Option<usize>,
    features: Vec<String>,
    all_features: bool,
    _no_default_features: bool,
    out_dir: Option<PathBuf>,
    emit: Option<EmitOption>,
    no_link: bool,
    verbose: bool,
) -> Result<()> {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    let project_or_workspace = find_project(std::env::current_dir()?.as_path())?;

    let profile = if release {
        BuildProfile::Release
    } else {
        BuildProfile::Debug
    };
    let jobs = jobs.unwrap_or_else(num_cpus::get);
    let emit_llvm_ir = matches!(emit, Some(EmitOption::Ir));
    let emit_asm = matches!(emit, Some(EmitOption::Asm));
    let emit_obj = matches!(emit, Some(EmitOption::Obj));

    let mut had_errors = false;

    match project_or_workspace {
        ProjectOrWorkspace::Project(project) => {
            compile_single_project(
                &project,
                &mut stdout,
                profile,
                target,
                features,
                all_features,
                emit_llvm_ir,
                emit_asm,
                emit_obj,
                no_link,
                jobs,
                verbose,
                out_dir,
            )
            .await?;
        }
        ProjectOrWorkspace::Workspace(workspace) => {
            writeln!(
                &mut stdout,
                "Building workspace with {} members...",
                workspace.members.len()
            )?;
            if workspace.members.is_empty() {
                return Err(anyhow::anyhow!("Workspace has no members"));
            }
            for member in workspace.members {
                let result = compile_single_project(
                    &member,
                    &mut stdout,
                    profile,
                    target.clone(),
                    features.clone(),
                    all_features,
                    emit_llvm_ir,
                    emit_asm,
                    emit_obj,
                    no_link,
                    jobs,
                    verbose,
                    out_dir.clone(),
                )
                .await;
                if let Err(e) = result {
                    eprintln!("error: failed to compile {}: {}", member.name(), e);
                    had_errors = true;
                }
            }
        }
    }

    if had_errors {
        std::process::exit(ErrorCode::Compilation.exit_code());
    }

    Ok(())
}

#[allow(clippy::too_many_arguments)]
async fn compile_single_project(
    project: &Project,
    stdout: &mut StandardStream,
    profile: BuildProfile,
    target: Option<String>,
    features: Vec<String>,
    all_features: bool,
    emit_llvm_ir: bool,
    emit_asm: bool,
    emit_obj: bool,
    no_link: bool,
    jobs: usize,
    verbose: bool,
    out_dir: Option<PathBuf>,
) -> Result<()> {
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
    write!(stdout, "Compiling")?;
    stdout.reset()?;
    writeln!(stdout, " {} v{}", project.name(), project.version())?;

    let mut config = PipelineConfig {
        profile,
        target_triple: target,
        features,
        emit_llvm_ir,
        emit_asm,
        emit_obj,
        no_link,
        jobs,
        verbose,
        out_dir,
    };

    if all_features {
        config.features = project.manifest.features.keys().cloned().collect();
    }

    let result = compile_project(project, config).await?;

    if !result.success {
        let mut reporter = ErrorReporter::new();
        for diagnostic in result.diagnostics {
            reporter.error(CliError::new(diagnostic.message).with_code(ErrorCode::Compilation));
        }
        reporter.report();
        return Err(anyhow::anyhow!(
            "compilation failed for project {}",
            project.name()
        ));
    }

    Ok(())
}

// =============================================================================
// Run Command
// =============================================================================

async fn cmd_run(
    release: bool,
    _features: Vec<String>,
    args: Vec<String>,
    verbose: bool,
) -> Result<()> {
    // Build first
    cmd_build(
        release,
        None,
        None,
        vec![],
        false,
        false,
        None,
        None,
        false,
        verbose,
    )
    .await?;

    // Find project
    let project = match find_project(std::env::current_dir()?.as_path())? {
        ProjectOrWorkspace::Project(p) => p,
        ProjectOrWorkspace::Workspace(w) => w
            .members
            .into_iter()
            .next()
            .ok_or_else(|| anyhow::anyhow!("Workspace has no members"))?,
    };

    // Find executable
    let profile = if release { "release" } else { "debug" };
    let exe_path = project.executable_path(profile);

    if !exe_path.exists() {
        return Err(CliError::new(format!("Executable not found: {}", exe_path.display())).into());
    }

    // Run
    let mut cmd = std::process::Command::new(&exe_path);
    cmd.args(args);

    let status = cmd
        .status()
        .with_context(|| format!("Failed to run {}", exe_path.display()))?;

    std::process::exit(status.code().unwrap_or(1));
}

// =============================================================================
// Test Command
// =============================================================================

async fn cmd_test(
    pattern: Option<String>,
    _release: bool,
    _features: Vec<String>,
    lib: bool,
    doc: bool,
    bench: bool,
    nocapture: bool,
    test_threads: Option<usize>,
) -> Result<()> {
    let project_or_workspace = find_project(std::env::current_dir()?.as_path())?;

    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    // Determine what to run
    let run_mode = if bench {
        TestMode::Benchmark
    } else if doc {
        TestMode::DocTest
    } else if lib {
        TestMode::Unit
    } else {
        TestMode::All
    };

    // Collect all test files
    let test_files = collect_test_files(&project_or_workspace, &run_mode)?;

    if test_files.is_empty() {
        match run_mode {
            TestMode::Benchmark => print_warning("No benchmark files found in benches/"),
            TestMode::DocTest => print_warning("No documentation tests found"),
            _ => print_warning("No test files found in tests/"),
        }
        return Ok(());
    }

    // Print header
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
    write!(&mut stdout, "Running")?;
    stdout.reset()?;
    match run_mode {
        TestMode::Benchmark => writeln!(&mut stdout, " benchmarks...")?,
        TestMode::DocTest => writeln!(&mut stdout, " documentation tests...")?,
        _ => writeln!(&mut stdout, " tests...")?,
    }

    if let Some(ref pat) = pattern {
        writeln!(&mut stdout, "  Filter: {}", pat)?;
    }

    if nocapture {
        writeln!(&mut stdout, "  Output capture: disabled")?;
    }

    if let Some(threads) = test_threads {
        writeln!(&mut stdout, "  Test threads: {}", threads)?;
    }

    writeln!(&mut stdout)?;

    // Run tests
    let mut test_runner = TestRunner::new(TestRunnerConfig {
        pattern,
        nocapture,
        test_threads: test_threads.unwrap_or_else(num_cpus::get),
    });

    let results = test_runner.run_tests(&test_files).await?;

    // Print results
    print_test_results(&results)?;

    // Exit with appropriate code
    if results.failed > 0 {
        std::process::exit(101);
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TestMode {
    Unit,
    Integration,
    DocTest,
    Benchmark,
    All,
}

fn collect_test_files(
    project_or_workspace: &ProjectOrWorkspace,
    mode: &TestMode,
) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    match project_or_workspace {
        ProjectOrWorkspace::Project(project) => {
            collect_project_test_files(project, mode, &mut files)?;
        }
        ProjectOrWorkspace::Workspace(workspace) => {
            for member in &workspace.members {
                collect_project_test_files(member, mode, &mut files)?;
            }
        }
    }

    Ok(files)
}

fn collect_project_test_files(
    project: &Project,
    mode: &TestMode,
    files: &mut Vec<PathBuf>,
) -> Result<()> {
    match mode {
        TestMode::Unit | TestMode::All => {
            // Include test files from tests/ directory
            files.extend(project.test_files.clone());

            // Also include inline tests in source files
            for source_file in &project.source_files {
                if has_test_functions(&source_file.path)? {
                    files.push(source_file.path.clone());
                }
            }
        }
        TestMode::Integration => {
            files.extend(project.test_files.clone());
        }
        TestMode::Benchmark => {
            files.extend(project.bench_files.clone());
        }
        TestMode::DocTest => {
            // Doc tests are extracted from source files
            for source_file in &project.source_files {
                if has_doc_tests(&source_file.path)? {
                    files.push(source_file.path.clone());
                }
            }
        }
    }

    Ok(())
}

fn has_test_functions(path: &PathBuf) -> Result<bool> {
    let content = std::fs::read_to_string(path)?;
    // Simple check for #[test] attribute
    Ok(content.contains("#"))
}

fn has_doc_tests(path: &PathBuf) -> Result<bool> {
    let content = std::fs::read_to_string(path)?;
    // Simple check for ``` in doc comments
    Ok(content.contains("```"))
}

struct TestRunnerConfig {
    pattern: Option<String>,
    nocapture: bool,
    test_threads: usize,
}

struct TestRunner {
    config: TestRunnerConfig,
}

#[derive(Debug, Default)]
struct TestResults {
    passed: usize,
    failed: usize,
    ignored: usize,
    filtered: usize,
    duration: std::time::Duration,
    test_cases: Vec<TestCaseResult>,
}

#[derive(Debug, Clone)]
struct TestCaseResult {
    name: String,
    status: TestStatus,
    duration: std::time::Duration,
    output: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TestStatus {
    Passed,
    Failed(String),
    Ignored,
}

impl TestRunner {
    fn new(config: TestRunnerConfig) -> Self {
        Self { config }
    }

    async fn run_tests(&mut self, test_files: &[PathBuf]) -> Result<TestResults> {
        let start = std::time::Instant::now();
        let mut results = TestResults::default();

        // Parse and discover tests in each file
        for file in test_files {
            match self.discover_and_run_tests(file).await {
                Ok(file_results) => {
                    for test in file_results {
                        if let Some(ref pattern) = self.config.pattern {
                            if !test.name.contains(pattern) {
                                results.filtered += 1;
                                continue;
                            }
                        }

                        match &test.status {
                            TestStatus::Passed => results.passed += 1,
                            TestStatus::Failed(_) => results.failed += 1,
                            TestStatus::Ignored => results.ignored += 1,
                        }

                        // Print test result
                        self.print_test_result(&test)?;
                        results.test_cases.push(test);
                    }
                }
                Err(e) => {
                    eprintln!("Error running tests in {}: {}", file.display(), e);
                    results.failed += 1;
                }
            }
        }

        results.duration = start.elapsed();
        Ok(results)
    }

    async fn discover_and_run_tests(&self, file: &PathBuf) -> Result<Vec<TestCaseResult>> {
        let mut results = Vec::new();

        // Parse the file to discover test functions
        let content = tokio::fs::read_to_string(file).await?;
        let test_functions = self.parse_test_functions(&content);

        for test_name in test_functions {
            let test_result = self.run_single_test(&test_name, file).await;
            results.push(test_result);
        }

        Ok(results)
    }

    fn parse_test_functions(&self, content: &str) -> Vec<String> {
        let mut tests = Vec::new();

        // Simple regex-like parsing for test functions
        // Look for patterns like: fn test_something() or fn it_does_something()
        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("fn ") {
                // Check if it looks like a test function
                if let Some(name_start) = trimmed.find("fn ") {
                    let after_fn = &trimmed[name_start + 3..];
                    if let Some(name_end) = after_fn.find('(') {
                        let name = &after_fn[..name_end];
                        if name.starts_with("test_") || name.starts_with("it_") {
                            tests.push(name.to_string());
                        }
                    }
                }
            }
        }

        tests
    }

    async fn run_single_test(&self, name: &str, file: &PathBuf) -> TestCaseResult {
        let start = std::time::Instant::now();

        let mut status = TestStatus::Passed;
        let mut output = None;

        match self.execute_test(name, file).await {
            Ok(out) => {
                if self.config.nocapture {
                    output = Some(out);
                }
            }
            Err(e) => {
                status = TestStatus::Failed(e);
            }
        }

        TestCaseResult {
            name: name.to_string(),
            status,
            duration: start.elapsed(),
            output,
        }
    }

    async fn execute_test(
        &self,
        name: &str,
        file: &PathBuf,
    ) -> std::result::Result<String, String> {
        let file_content = tokio::fs::read_to_string(file)
            .await
            .map_err(|e| format!("failed to read {}: {}", file.display(), e))?;

        let temp_dir =
            tempfile::tempdir().map_err(|e| format!("failed to create temp dir: {}", e))?;
        let project_dir = temp_dir.path();
        let src_dir = project_dir.join("src");
        std::fs::create_dir_all(&src_dir)
            .map_err(|e| format!("failed to create src dir: {}", e))?;

        let main_src = format!("{file_content}\n\nfn main():\n    {name}()\n");
        tokio::fs::write(src_dir.join("main.jet"), main_src)
            .await
            .map_err(|e| format!("failed to write main.jet: {}", e))?;
        tokio::fs::write(
            project_dir.join("jet.toml"),
            "[package]\nname = \"jet_test_runner\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .await
        .map_err(|e| format!("failed to write jet.toml: {}", e))?;

        let jet_binary = std::env::current_exe()
            .map_err(|e| format!("failed to locate jet executable: {}", e))?;

        let build = TokioCommand::new(&jet_binary)
            .current_dir(project_dir)
            .arg("build")
            .output()
            .await
            .map_err(|e| format!("failed to run build: {}", e))?;
        if !build.status.success() {
            return Err(String::from_utf8_lossy(&build.stderr).to_string());
        }

        let run = TokioCommand::new(&jet_binary)
            .current_dir(project_dir)
            .arg("run")
            .output()
            .await
            .map_err(|e| format!("failed to run test binary: {}", e))?;
        if !run.status.success() {
            return Err(String::from_utf8_lossy(&run.stderr).to_string());
        }

        Ok(String::from_utf8_lossy(&run.stdout).to_string())
    }

    fn print_test_result(&self, test: &TestCaseResult) -> Result<()> {
        let mut stdout = StandardStream::stdout(ColorChoice::Auto);

        match &test.status {
            TestStatus::Passed => {
                stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
                write!(&mut stdout, "test {}", test.name)?;
                stdout.reset()?;
                writeln!(&mut stdout, " ... ok")?;
            }
            TestStatus::Failed(msg) => {
                stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
                write!(&mut stdout, "test {}", test.name)?;
                stdout.reset()?;
                writeln!(&mut stdout, " ... FAILED")?;
                writeln!(&mut stdout, "    {}", msg)?;
            }
            TestStatus::Ignored => {
                stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;
                write!(&mut stdout, "test {}", test.name)?;
                stdout.reset()?;
                writeln!(&mut stdout, " ... ignored")?;
            }
        }

        Ok(())
    }
}

fn print_test_results(results: &TestResults) -> Result<()> {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    writeln!(&mut stdout)?;

    // Print summary
    writeln!(
        &mut stdout,
        "test result: {}. {} passed; {} failed; {} ignored; {} filtered out; finished in {:.2}s",
        if results.failed == 0 { "ok" } else { "FAILED" },
        results.passed,
        results.failed,
        results.ignored,
        results.filtered,
        results.duration.as_secs_f64()
    )?;

    if results.failed > 0 {
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
        writeln!(&mut stdout, "\nfailures:")?;
        stdout.reset()?;

        for test in &results.test_cases {
            if let TestStatus::Failed(msg) = &test.status {
                writeln!(&mut stdout, "    {}: {}", test.name, msg)?;
            }
        }
    }

    Ok(())
}

// =============================================================================
// Check Command
// =============================================================================

async fn cmd_check(
    _features: Vec<String>,
    _all_features: bool,
    _lib: bool,
    _target: Option<String>,
    verbose: bool,
) -> Result<()> {
    let project = match find_project(std::env::current_dir()?.as_path())? {
        ProjectOrWorkspace::Project(p) => p,
        ProjectOrWorkspace::Workspace(w) => w
            .members
            .into_iter()
            .next()
            .ok_or_else(|| anyhow::anyhow!("Workspace has no members"))?,
    };

    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
    write!(&mut stdout, "Checking")?;
    stdout.reset()?;
    writeln!(&mut stdout, " {} v{}", project.name(), project.version())?;

    let diagnostics = check_project(&project, verbose).await?;

    if diagnostics.is_empty() {
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
        write!(&mut stdout, "Check")?;
        stdout.reset()?;
        writeln!(&mut stdout, " complete. No errors found.")?;
        Ok(())
    } else {
        let mut reporter = ErrorReporter::new();
        for diagnostic in diagnostics {
            reporter.error(CliError::new(diagnostic.message).with_code(ErrorCode::Compilation));
        }
        reporter.report();
        std::process::exit(ErrorCode::Compilation.exit_code());
    }
}

// =============================================================================
// Format Command
// =============================================================================

async fn cmd_fmt(check: bool, files: Vec<PathBuf>, emit: Option<EmitMode>) -> Result<()> {
    let project = match find_project(std::env::current_dir()?.as_path())? {
        ProjectOrWorkspace::Project(p) => p,
        ProjectOrWorkspace::Workspace(w) => w
            .members
            .into_iter()
            .next()
            .ok_or_else(|| anyhow::anyhow!("Workspace has no members"))?,
    };

    let files_to_format = if files.is_empty() {
        project
            .source_files
            .iter()
            .map(|f| f.path.clone())
            .collect()
    } else {
        files
    };

    if files_to_format.is_empty() {
        return Err(CliError::new("No Jet source files found").into());
    }

    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    if check {
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
        write!(&mut stdout, "Checking")?;
        stdout.reset()?;
        writeln!(&mut stdout, " formatting...")?;
    } else {
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
        write!(&mut stdout, "Formatting")?;
        stdout.reset()?;
        writeln!(&mut stdout, " {} files...", files_to_format.len())?;
    }

    let mut formatted_count = 0;
    let mut unchanged_count = 0;
    let mut error_count = 0;

    for file in &files_to_format {
        match format_file(file, check, emit).await {
            Ok(FormattedResult::Changed) => {
                formatted_count += 1;
                if check {
                    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;
                    writeln!(&mut stdout, "  Would reformat: {}", file.display())?;
                    stdout.reset()?;
                } else {
                    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
                    writeln!(&mut stdout, "  Reformatted: {}", file.display())?;
                    stdout.reset()?;
                }
            }
            Ok(FormattedResult::Unchanged) => {
                unchanged_count += 1;
            }
            Ok(FormattedResult::Written) => {
                formatted_count += 1;
                stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
                writeln!(&mut stdout, "  Written: {}", file.display())?;
                stdout.reset()?;
            }
            Err(e) => {
                error_count += 1;
                stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
                writeln!(&mut stdout, "  Error formatting {}: {}", file.display(), e)?;
                stdout.reset()?;
            }
        }
    }

    // Print summary
    writeln!(&mut stdout)?;
    if check && formatted_count > 0 {
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)).set_bold(true))?;
        write!(&mut stdout, "Warning")?;
        stdout.reset()?;
        writeln!(&mut stdout, ": {} files need formatting", formatted_count)?;
        std::process::exit(1);
    } else {
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
        write!(&mut stdout, "Finished")?;
        stdout.reset()?;
        writeln!(
            &mut stdout,
            ": {} reformatted, {} unchanged, {} errors",
            formatted_count, unchanged_count, error_count
        )?;
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FormattedResult {
    Changed,
    Unchanged,
    Written,
}

async fn format_file(
    path: &PathBuf,
    check: bool,
    emit: Option<EmitMode>,
) -> Result<FormattedResult> {
    let source = tokio::fs::read_to_string(path)
        .await
        .with_context(|| format!("Failed to read {}", path.display()))?;

    let formatted = jet_fmt::format_source(&source).map_err(|e| anyhow::anyhow!("{}", e))?;

    if source == formatted {
        return Ok(FormattedResult::Unchanged);
    }

    if check {
        return Ok(FormattedResult::Changed);
    }

    match emit {
        Some(EmitMode::Stdout) => {
            print!("{}", formatted);
            Ok(FormattedResult::Written)
        }
        Some(EmitMode::Files) | None => {
            tokio::fs::write(path, formatted)
                .await
                .with_context(|| format!("Failed to write {}", path.display()))?;
            Ok(FormattedResult::Changed)
        }
    }
}

// =============================================================================
// LSP Command
// =============================================================================

async fn cmd_lsp(port: Option<u16>) -> Result<()> {
    // Try to find and run the jet-lsp binary
    let lsp_binary = if cfg!(windows) {
        "jet-lsp.exe"
    } else {
        "jet-lsp"
    };

    // Check if jet-lsp exists in the same directory as jet
    let current_exe = std::env::current_exe()?;
    let lsp_path = current_exe
        .parent()
        .unwrap_or(Path::new("."))
        .join(lsp_binary);

    let mut cmd = if lsp_path.exists() {
        std::process::Command::new(&lsp_path)
    } else {
        // Fallback: try to run from PATH
        std::process::Command::new(lsp_binary)
    };

    if let Some(p) = port {
        cmd.arg("--port").arg(p.to_string());
    }

    let status = cmd
        .status()
        .with_context(|| "Failed to start LSP server. Is jet-lsp installed?")?;

    if !status.success() {
        anyhow::bail!("LSP server exited with code {}", status.code().unwrap_or(1));
    }

    Ok(())
}

// =============================================================================
// Doc Command
// =============================================================================

async fn cmd_doc(
    open: bool,
    no_deps: bool,
    document_private_items: bool,
    output: Option<PathBuf>,
    serve: bool,
    port: u16,
) -> Result<()> {
    let project_or_workspace = find_project(std::env::current_dir()?.as_path())?;
    let project_root = project_or_workspace.root().to_path_buf();
    let manifest = project_or_workspace.manifest().clone();

    let docs_dir = output.unwrap_or_else(|| project_root.join("target/doc"));
    std::fs::create_dir_all(&docs_dir)?;

    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
    write!(&mut stdout, "Documenting")?;
    stdout.reset()?;
    writeln!(
        &mut stdout,
        " {} v{}",
        manifest.package.name, manifest.package.version
    )?;

    if document_private_items {
        writeln!(&mut stdout, "  Including private items")?;
    }
    if no_deps {
        writeln!(&mut stdout, "  Skipping dependency docs")?;
    }

    // Create CSS file
    let css = generate_doc_css();
    std::fs::write(docs_dir.join("style.css"), css)?;

    // Generate documentation for each source file
    let source_files: Vec<_> = match &project_or_workspace {
        ProjectOrWorkspace::Project(p) => p.source_files.iter().map(|f| f.path.clone()).collect(),
        ProjectOrWorkspace::Workspace(w) => w
            .members
            .iter()
            .flat_map(|m| m.source_files.iter().map(|f| f.path.clone()))
            .collect(),
    };

    let mut documented_items = 0;

    for file in &source_files {
        match generate_file_docs(file, &docs_dir, document_private_items).await {
            Ok(count) => {
                documented_items += count;
            }
            Err(e) => {
                stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;
                writeln!(
                    &mut stdout,
                    "  Warning: Could not document {}: {}",
                    file.display(),
                    e
                )?;
                stdout.reset()?;
            }
        }
    }

    // Generate index.html
    let index_html = generate_index_html(&manifest, &source_files, documented_items);
    std::fs::write(docs_dir.join("index.html"), index_html)?;

    // Generate search index
    let search_index = generate_search_index(&source_files).await?;
    std::fs::write(docs_dir.join("search-index.js"), search_index)?;

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
    write!(&mut stdout, "Finished")?;
    stdout.reset()?;
    writeln!(
        &mut stdout,
        " documentation at {} ({} items)",
        docs_dir.display(),
        documented_items
    )?;

    if open {
        let url = format!("file://{}", docs_dir.canonicalize()?.display());

        #[cfg(target_os = "macos")]
        std::process::Command::new("open").arg(&url).spawn()?;

        #[cfg(target_os = "linux")]
        std::process::Command::new("xdg-open").arg(&url).spawn()?;

        #[cfg(target_os = "windows")]
        std::process::Command::new("cmd")
            .args(["/C", "start", &url])
            .spawn()?;
    }

    if serve {
        start_doc_server(&docs_dir, port).await?;
    }

    Ok(())
}

fn generate_doc_css() -> &'static str {
    r#"/* Jet Documentation Styles */
:root {
    --bg-color: #1e1e1e;
    --fg-color: #d4d4d4;
    --link-color: #4ec9b0;
    --link-hover: #6ad9c2;
    --header-bg: #252526;
    --sidebar-bg: #252526;
    --code-bg: #2d2d2d;
    --border-color: #3e3e3e;
    --keyword-color: #569cd6;
    --string-color: #ce9178;
    --comment-color: #6a9955;
    --type-color: #4ec9b0;
    --function-color: #dcdcaa;
}

* {
    box-sizing: border-box;
}

body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    margin: 0;
    padding: 0;
    background: var(--bg-color);
    color: var(--fg-color);
    line-height: 1.6;
}

.container {
    display: flex;
    min-height: 100vh;
}

.sidebar {
    width: 280px;
    background: var(--sidebar-bg);
    border-right: 1px solid var(--border-color);
    padding: 1rem;
    overflow-y: auto;
    position: fixed;
    height: 100vh;
}

.sidebar h2 {
    font-size: 1rem;
    margin-top: 1.5rem;
    margin-bottom: 0.5rem;
    color: var(--fg-color);
}

.sidebar a {
    color: var(--link-color);
    text-decoration: none;
    display: block;
    padding: 0.25rem 0;
}

.sidebar a:hover {
    color: var(--link-hover);
}

.main-content {
    flex: 1;
    margin-left: 280px;
    padding: 2rem;
    max-width: 900px;
}

header {
    background: var(--header-bg);
    padding: 1rem 2rem;
    border-bottom: 1px solid var(--border-color);
    margin: -2rem -2rem 2rem -2rem;
}

header h1 {
    margin: 0;
    font-size: 1.5rem;
}

.module-item {
    margin-bottom: 2rem;
    padding: 1rem;
    background: var(--header-bg);
    border-radius: 4px;
}

.module-item h3 {
    margin-top: 0;
    color: var(--function-color);
}

.module-item code {
    background: var(--code-bg);
    padding: 0.125rem 0.25rem;
    border-radius: 3px;
    font-family: "JetBrains Mono", "Fira Code", monospace;
}

.doc-comment {
    color: #9cdcfe;
    margin: 0.5rem 0;
}

.keyword { color: var(--keyword-color); }
.string { color: var(--string-color); }
.comment { color: var(--comment-color); }
.type { color: var(--type-color); }
.function { color: var(--function-color); }

pre {
    background: var(--code-bg);
    padding: 1rem;
    border-radius: 4px;
    overflow-x: auto;
}

pre code {
    background: none;
    padding: 0;
}

.search-box {
    width: 100%;
    padding: 0.5rem;
    background: var(--bg-color);
    border: 1px solid var(--border-color);
    color: var(--fg-color);
    border-radius: 4px;
    margin-bottom: 1rem;
}

footer {
    margin-top: 3rem;
    padding-top: 1rem;
    border-top: 1px solid var(--border-color);
    color: #888;
    font-size: 0.875rem;
}
"#
}

async fn generate_file_docs(
    file: &PathBuf,
    docs_dir: &PathBuf,
    include_private: bool,
) -> Result<usize> {
    let content = tokio::fs::read_to_string(file).await?;
    let file_stem = file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");

    // Parse the file to extract documentation
    let items = parse_doc_items(&content, include_private);

    if items.is_empty() {
        return Ok(0);
    }

    // Generate HTML for this file
    let html = generate_module_html(file_stem, &items);

    let output_path = docs_dir.join(format!("{}.html", file_stem));
    tokio::fs::write(output_path, html).await?;

    Ok(items.len())
}

#[derive(Debug)]
struct DocItem {
    name: String,
    kind: DocItemKind,
    signature: String,
    doc_comment: Option<String>,
    is_public: bool,
}

#[derive(Debug, Clone, Copy)]
enum DocItemKind {
    Function,
    Struct,
    Enum,
    Trait,
    Const,
    Type,
    Module,
}

fn parse_doc_items(content: &str, _include_private: bool) -> Vec<DocItem> {
    let mut items = Vec::new();
    let mut current_doc: Option<String> = None;

    for line in content.lines() {
        let trimmed = line.trim();

        // Capture doc comments
        if trimmed.starts_with("///") || trimmed.starts_with("# ") {
            let doc = if trimmed.starts_with("///") {
                trimmed.strip_prefix("///").unwrap_or("").trim()
            } else {
                trimmed.strip_prefix('#').unwrap_or("").trim()
            };

            current_doc = Some(match current_doc {
                Some(existing) => format!("{} {}", existing, doc),
                None => doc.to_string(),
            });
            continue;
        }

        // Parse item definitions
        let is_pub = trimmed.starts_with("pub ");
        let rest = if is_pub {
            trimmed.strip_prefix("pub ").unwrap_or(trimmed)
        } else {
            trimmed
        };

        let (kind, name) = if rest.starts_with("fn ") {
            let after_fn = &rest[3..];
            if let Some(name_end) = after_fn.find(|c: char| !c.is_alphanumeric() && c != '_') {
                (DocItemKind::Function, after_fn[..name_end].to_string())
            } else {
                continue;
            }
        } else if rest.starts_with("struct ") {
            let after = &rest[7..];
            if let Some(name_end) = after.find(|c: char| !c.is_alphanumeric() && c != '_') {
                (DocItemKind::Struct, after[..name_end].to_string())
            } else {
                continue;
            }
        } else if rest.starts_with("enum ") {
            let after = &rest[5..];
            if let Some(name_end) = after.find(|c: char| !c.is_alphanumeric() && c != '_') {
                (DocItemKind::Enum, after[..name_end].to_string())
            } else {
                continue;
            }
        } else if rest.starts_with("trait ") {
            let after = &rest[6..];
            if let Some(name_end) = after.find(|c: char| !c.is_alphanumeric() && c != '_') {
                (DocItemKind::Trait, after[..name_end].to_string())
            } else {
                continue;
            }
        } else if rest.starts_with("const ") {
            let after = &rest[6..];
            if let Some(name_end) = after.find(|c: char| !c.is_alphanumeric() && c != '_') {
                (DocItemKind::Const, after[..name_end].to_string())
            } else {
                continue;
            }
        } else if rest.starts_with("type ") {
            let after = &rest[5..];
            if let Some(name_end) = after.find(|c: char| !c.is_alphanumeric() && c != '_') {
                (DocItemKind::Type, after[..name_end].to_string())
            } else {
                continue;
            }
        } else {
            continue;
        };

        items.push(DocItem {
            name,
            kind,
            signature: trimmed.to_string(),
            doc_comment: current_doc.take(),
            is_public: is_pub,
        });
    }

    items
}

fn generate_module_html(module_name: &str, items: &[DocItem]) -> String {
    let items_html: String = items
        .iter()
        .map(|item| {
            let kind_str = format!("{:?}", item.kind).to_lowercase();
            let doc_html = item
                .doc_comment
                .as_ref()
                .map(|d| format!(r#"<p class="doc-comment">{}</p>"#, html_escape(d)))
                .unwrap_or_default();

            format!(
                r#"<div class="module-item" id="{}">
    <h3><span class="keyword">{}</span> {}</h3>
    <pre><code>{}</code></pre>
    {}
</div>"#,
                item.name,
                kind_str,
                item.name,
                html_escape(&item.signature),
                doc_html
            )
        })
        .collect();

    format!(
        r#"<!DOCTYPE html>
<html>
<head>
    <title>Module {} - Documentation</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <div class="container">
        <aside class="sidebar">
            <h2>Modules</h2>
            <a href="index.html">← Back to index</a>
            <h2>Items</h2>
            {}
        </aside>
        <main class="main-content">
            <header>
                <h1>Module {}</h1>
            </header>
            {}
        </main>
    </div>
</body>
</html>"#,
        module_name,
        items
            .iter()
            .map(|i| format!("<a href=\"#{}\">{}</a>", i.name, i.name))
            .collect::<String>(),
        module_name,
        items_html
    )
}

fn generate_index_html(manifest: &Manifest, source_files: &[PathBuf], item_count: usize) -> String {
    let modules_html: String = source_files
        .iter()
        .filter_map(|f| {
            f.file_stem()
                .and_then(|s| s.to_str())
                .map(|name| format!(r#"<li><a href="{}.html">{}</a></li>"#, name, name))
        })
        .collect();

    format!(
        r#"<!DOCTYPE html>
<html>
<head>
    <title>{} {} - Documentation</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <div class="container">
        <aside class="sidebar">
            <input type="text" class="search-box" placeholder="Search..." id="search">
            <h2>Modules</h2>
            <ul>{}</ul>
        </aside>
        <main class="main-content">
            <header>
                <h1>{} {}</h1>
            </header>
            <p>{} items documented across {} modules.</p>
            <h2>Crate Information</h2>
            <table>
                <tr><td><strong>Name</strong></td><td>{}</td></tr>
                <tr><td><strong>Version</strong></td><td>{}</td></tr>
                <tr><td><strong>Edition</strong></td><td>{}</td></tr>
            </table>
            <footer>
                Generated by jet doc
            </footer>
        </main>
    </div>
    <script src="search-index.js"></script>
</body>
</html>"#,
        manifest.package.name,
        manifest.package.version,
        modules_html,
        manifest.package.name,
        manifest.package.version,
        item_count,
        source_files.len(),
        manifest.package.name,
        manifest.package.version,
        &manifest.package.edition
    )
}

async fn generate_search_index(source_files: &[PathBuf]) -> Result<String> {
    let mut items = Vec::new();

    for file in source_files {
        let content = tokio::fs::read_to_string(file).await?;
        let file_stem = file
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("fn ") || trimmed.starts_with("pub fn ") {
                if let Some(name) = trimmed.split_whitespace().nth(1) {
                    let name = name.trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_');
                    items.push(format!(
                        r#"{{"name":"{}","path":"{}.html#{}","type":"function"}}"#,
                        name, file_stem, name
                    ));
                }
            }
        }
    }

    Ok(format!("const SEARCH_INDEX = [{}];", items.join(",")))
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

async fn start_doc_server(docs_dir: &PathBuf, port: u16) -> Result<()> {
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    use tokio::net::TcpListener;

    let addr = format!("127.0.0.1:{}", port);
    let listener = TcpListener::bind(&addr).await?;

    println!("Documentation server running at http://localhost:{}", port);
    println!("Press Ctrl+C to stop");

    let docs_dir = docs_dir.clone();

    tokio::spawn(async move {
        loop {
            match listener.accept().await {
                Ok((mut socket, _)) => {
                    let docs_dir = docs_dir.clone();
                    tokio::spawn(async move {
                        let mut buffer = [0u8; 1024];
                        if let Ok(n) = socket.read(&mut buffer).await {
                            let request = String::from_utf8_lossy(&buffer[..n]);

                            // Parse the request path
                            let path = request
                                .lines()
                                .next()
                                .and_then(|line| line.split_whitespace().nth(1))
                                .unwrap_or("/");

                            let file_path = if path == "/" {
                                docs_dir.join("index.html")
                            } else {
                                docs_dir.join(path.trim_start_matches('/'))
                            };

                            let (status, content, content_type) = if let Ok(content) =
                                tokio::fs::read(&file_path).await
                            {
                                let ct = if file_path
                                    .extension()
                                    .map(|e| e == "css")
                                    .unwrap_or(false)
                                {
                                    "text/css"
                                } else if file_path.extension().map(|e| e == "js").unwrap_or(false)
                                {
                                    "application/javascript"
                                } else {
                                    "text/html"
                                };
                                ("200 OK", content, ct)
                            } else {
                                let not_found = b"HTTP/1.1 404 Not Found\r\n\r\nNot Found";
                                let _ = socket.write_all(not_found).await;
                                return;
                            };

                            let response = format!(
                                "HTTP/1.1 {}\r\nContent-Type: {}\r\nContent-Length: {}\r\n\r\n",
                                status,
                                content_type,
                                content.len()
                            );

                            let _ = socket.write_all(response.as_bytes()).await;
                            let _ = socket.write_all(&content).await;
                        }
                    });
                }
                Err(e) => {
                    eprintln!("Error accepting connection: {}", e);
                }
            }
        }
    });

    // Wait for Ctrl+C
    tokio::signal::ctrl_c().await?;
    println!("\nShutting down server...");

    Ok(())
}

// =============================================================================
// Lint Command
// =============================================================================

async fn cmd_lint(
    fix: bool,
    deny: Vec<String>,
    warn: Vec<String>,
    allow: Vec<String>,
) -> Result<()> {
    let project_or_workspace = find_project(std::env::current_dir()?.as_path())?;

    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
    write!(&mut stdout, "Linting")?;
    stdout.reset()?;

    match &project_or_workspace {
        ProjectOrWorkspace::Project(p) => writeln!(&mut stdout, " {} v{}", p.name(), p.version())?,
        ProjectOrWorkspace::Workspace(w) => {
            writeln!(&mut stdout, " workspace with {} members", w.members.len())?
        }
    }

    // Configure lint levels
    let config = LintConfig {
        fix,
        deny_lints: deny.iter().cloned().collect(),
        warn_lints: warn.iter().cloned().collect(),
        allow_lints: allow.iter().cloned().collect(),
    };

    if fix {
        writeln!(&mut stdout, "  Auto-fix enabled")?;
    }

    // Collect all source files
    let source_files: Vec<_> = match &project_or_workspace {
        ProjectOrWorkspace::Project(p) => p.source_files.iter().map(|f| f.path.clone()).collect(),
        ProjectOrWorkspace::Workspace(w) => w
            .members
            .iter()
            .flat_map(|m| m.source_files.iter().map(|f| f.path.clone()))
            .collect(),
    };

    // Store whether we have deny lints before moving config
    let has_deny_lints = !config.deny_lints.is_empty();

    // Run linter on all files
    let mut linter = Linter::new(config);
    let mut total_violations = 0;
    let mut fixed_violations = 0;

    for file in &source_files {
        match linter.lint_file(file).await {
            Ok(violations) => {
                if !violations.is_empty() {
                    writeln!(&mut stdout, "\n  {}:", file.display())?;
                    for v in &violations {
                        total_violations += 1;
                        print_violation(&mut stdout, v)?;
                        if v.fixed {
                            fixed_violations += 1;
                        }
                    }
                }
            }
            Err(e) => {
                stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;
                writeln!(
                    &mut stdout,
                    "  Warning: Could not lint {}: {}",
                    file.display(),
                    e
                )?;
                stdout.reset()?;
            }
        }
    }

    // Print summary
    writeln!(&mut stdout)?;
    if total_violations == 0 {
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
        writeln!(&mut stdout, "✓ No lint violations found")?;
        stdout.reset()?;
    } else {
        let severity = if has_deny_lints {
            Color::Red
        } else {
            Color::Yellow
        };
        stdout.set_color(ColorSpec::new().set_fg(Some(severity)).set_bold(true))?;
        writeln!(
            &mut stdout,
            "Found {} lint violations ({} fixed)",
            total_violations, fixed_violations
        )?;
        stdout.reset()?;

        if has_deny_lints && total_violations > fixed_violations {
            std::process::exit(1);
        }
    }

    Ok(())
}

#[derive(Debug)]
struct LintConfig {
    fix: bool,
    deny_lints: std::collections::HashSet<String>,
    warn_lints: std::collections::HashSet<String>,
    allow_lints: std::collections::HashSet<String>,
}

#[derive(Debug, Clone)]
struct LintViolation {
    line: usize,
    column: usize,
    rule: String,
    message: String,
    severity: LintSeverity,
    fixed: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LintSeverity {
    Error,
    Warning,
    Note,
}

struct Linter {
    config: LintConfig,
    rules: Vec<Box<dyn LintRule>>,
}

trait LintRule: Send + Sync {
    fn name(&self) -> &'static str;
    fn check(&self, line: &str, line_num: usize) -> Vec<LintViolation>;
    fn fix(&self, line: &str) -> Option<String>;
}

struct UnusedVariableRule;
struct DeadCodeRule;
struct NamingConventionRule;
struct ImportStyleRule;
struct TrailingWhitespaceRule;
struct LineLengthRule;

impl LintRule for UnusedVariableRule {
    fn name(&self) -> &'static str {
        "unused_variable"
    }

    fn check(&self, line: &str, line_num: usize) -> Vec<LintViolation> {
        let mut violations = Vec::new();
        let trimmed = line.trim();

        // Check for unused variables (simplified: starts with underscore but isn't used)
        if trimmed.starts_with("let _") && !trimmed.contains("=") {
            violations.push(LintViolation {
                line: line_num,
                column: 1,
                rule: self.name().to_string(),
                message: "Unused variable without assignment".to_string(),
                severity: LintSeverity::Warning,
                fixed: false,
            });
        }

        violations
    }

    fn fix(&self, _line: &str) -> Option<String> {
        None
    }
}

impl LintRule for DeadCodeRule {
    fn name(&self) -> &'static str {
        "dead_code"
    }

    fn check(&self, line: &str, line_num: usize) -> Vec<LintViolation> {
        let mut violations = Vec::new();
        let trimmed = line.trim();

        // Check for unreachable code patterns
        if trimmed.starts_with("return ") && !line.contains("if") {
            violations.push(LintViolation {
                line: line_num,
                column: 1,
                rule: self.name().to_string(),
                message: "Code after return may be unreachable".to_string(),
                severity: LintSeverity::Warning,
                fixed: false,
            });
        }

        violations
    }

    fn fix(&self, _line: &str) -> Option<String> {
        None
    }
}

impl LintRule for NamingConventionRule {
    fn name(&self) -> &'static str {
        "naming_convention"
    }

    fn check(&self, line: &str, line_num: usize) -> Vec<LintViolation> {
        let mut violations = Vec::new();
        let trimmed = line.trim();

        // Check snake_case for functions
        if trimmed.starts_with("fn ") {
            if let Some(name_start) = trimmed.find("fn ") {
                let after_fn = &trimmed[name_start + 3..];
                if let Some(name_end) = after_fn.find('(') {
                    let name = &after_fn[..name_end];
                    if name.chars().any(|c| c.is_uppercase()) && !name.starts_with("test_") {
                        violations.push(LintViolation {
                            line: line_num,
                            column: name_start + 4,
                            rule: self.name().to_string(),
                            message: format!("Function '{}' should use snake_case", name),
                            severity: LintSeverity::Warning,
                            fixed: false,
                        });
                    }
                }
            }
        }

        violations
    }

    fn fix(&self, _line: &str) -> Option<String> {
        None
    }
}

impl LintRule for ImportStyleRule {
    fn name(&self) -> &'static str {
        "import_style"
    }

    fn check(&self, line: &str, line_num: usize) -> Vec<LintViolation> {
        let mut violations = Vec::new();
        let trimmed = line.trim();

        // Check for wildcard imports
        if trimmed.contains("import *") || trimmed.contains("import * from") {
            violations.push(LintViolation {
                line: line_num,
                column: 1,
                rule: self.name().to_string(),
                message: "Wildcard imports should be avoided".to_string(),
                severity: LintSeverity::Note,
                fixed: false,
            });
        }

        violations
    }

    fn fix(&self, _line: &str) -> Option<String> {
        None
    }
}

impl LintRule for TrailingWhitespaceRule {
    fn name(&self) -> &'static str {
        "trailing_whitespace"
    }

    fn check(&self, line: &str, line_num: usize) -> Vec<LintViolation> {
        let mut violations = Vec::new();

        if line.ends_with(' ') || line.ends_with('\t') {
            violations.push(LintViolation {
                line: line_num,
                column: line.len(),
                rule: self.name().to_string(),
                message: "Trailing whitespace".to_string(),
                severity: LintSeverity::Note,
                fixed: false,
            });
        }

        violations
    }

    fn fix(&self, line: &str) -> Option<String> {
        Some(line.trim_end().to_string())
    }
}

impl LintRule for LineLengthRule {
    fn name(&self) -> &'static str {
        "line_length"
    }

    fn check(&self, line: &str, line_num: usize) -> Vec<LintViolation> {
        let mut violations = Vec::new();
        const MAX_LENGTH: usize = 100;

        if line.len() > MAX_LENGTH {
            violations.push(LintViolation {
                line: line_num,
                column: MAX_LENGTH,
                rule: self.name().to_string(),
                message: format!("Line too long ({} > {} characters)", line.len(), MAX_LENGTH),
                severity: LintSeverity::Warning,
                fixed: false,
            });
        }

        violations
    }

    fn fix(&self, _line: &str) -> Option<String> {
        None
    }
}

impl Linter {
    fn new(config: LintConfig) -> Self {
        let rules: Vec<Box<dyn LintRule>> = vec![
            Box::new(UnusedVariableRule),
            Box::new(DeadCodeRule),
            Box::new(NamingConventionRule),
            Box::new(ImportStyleRule),
            Box::new(TrailingWhitespaceRule),
            Box::new(LineLengthRule),
        ];

        Self { config, rules }
    }

    async fn lint_file(&mut self, file: &PathBuf) -> Result<Vec<LintViolation>> {
        let content = tokio::fs::read_to_string(file).await?;
        let mut violations = Vec::new();
        let mut lines: Vec<String> = content.lines().map(|s| s.to_string()).collect();

        for line_num in 0..lines.len() {
            let line = lines[line_num].clone();
            for rule in &self.rules {
                let mut rule_violations = rule.check(&line, line_num + 1);

                // Determine severity based on configuration
                for v in &mut rule_violations {
                    if self.config.deny_lints.contains(&v.rule) {
                        v.severity = LintSeverity::Error;
                    } else if self.config.allow_lints.contains(&v.rule) {
                        continue;
                    } else if self.config.warn_lints.contains(&v.rule) {
                        v.severity = LintSeverity::Warning;
                    }

                    // Try to auto-fix if enabled
                    if self.config.fix && v.severity != LintSeverity::Error {
                        if let Some(fixed_line) = rule.fix(&line) {
                            lines[line_num] = fixed_line;
                            v.fixed = true;
                        }
                    }

                    violations.push(v.clone());
                }
            }
        }

        // Write fixed content if any fixes were made
        if self.config.fix && violations.iter().any(|v| v.fixed) {
            let fixed_content = lines.join("\n");
            tokio::fs::write(file, fixed_content).await?;
        }

        Ok(violations)
    }
}

fn print_violation(stdout: &mut StandardStream, v: &LintViolation) -> Result<()> {
    let color = match v.severity {
        LintSeverity::Error => Color::Red,
        LintSeverity::Warning => Color::Yellow,
        LintSeverity::Note => Color::Blue,
    };

    stdout.set_color(ColorSpec::new().set_fg(Some(color)))?;
    write!(stdout, "    [{}]", v.line)?;
    stdout.reset()?;

    let severity_str = match v.severity {
        LintSeverity::Error => "error",
        LintSeverity::Warning => "warning",
        LintSeverity::Note => "note",
    };

    write!(stdout, " {}: {}", severity_str, v.message)?;

    if v.fixed {
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
        write!(stdout, " [fixed]")?;
        stdout.reset()?;
    }

    writeln!(stdout)?;

    Ok(())
}

// =============================================================================
// Publish Command
// =============================================================================

async fn cmd_publish(
    dry_run: bool,
    token: Option<String>,
    registry: Option<String>,
    allow_dirty: bool,
    no_verify: bool,
) -> Result<()> {
    if !dry_run {
        return Err(CliError::new(
            "Publishing to remote registry is not supported in Jet 1.0 yet. Use --dry-run for local package verification.",
        )
        .into());
    }

    let project = match find_project(std::env::current_dir()?.as_path())? {
        ProjectOrWorkspace::Project(p) => p,
        ProjectOrWorkspace::Workspace(_) => {
            return Err(CliError::new(
                "Cannot publish a workspace directly. Publish individual members instead.",
            )
            .into());
        }
    };

    let config = PublishConfig {
        registry,
        dry_run,
        allow_dirty,
        no_verify,
        token,
        verbose: false,
    };

    let publisher = Publisher::new(config)?;
    publisher.publish(&project).await?;

    Ok(())
}

async fn cmd_login(registry: Option<String>, token: Option<String>) -> Result<()> {
    let registry = registry.unwrap_or_else(|| {
        std::env::var("JET_REGISTRY").unwrap_or_else(|_| "https://crates.jet-lang.org".to_string())
    });

    let token = match token {
        Some(t) => t,
        None => {
            // Prompt for token
            print!("Enter API token for {}: ", registry);
            std::io::stdout().flush()?;
            let mut input = String::new();
            std::io::stdin().read_line(&mut input)?;
            input.trim().to_string()
        }
    };

    if token.is_empty() {
        return Err(CliError::new("Token cannot be empty").into());
    }

    let config = PublishConfig::default();
    let mut publisher = Publisher::new(config)?;
    publisher.login(&registry, &token)?;

    print_success(format!("Logged in to {}", registry));

    Ok(())
}

async fn cmd_logout(registry: Option<String>) -> Result<()> {
    let registry = registry.unwrap_or_else(|| {
        std::env::var("JET_REGISTRY").unwrap_or_else(|_| "https://crates.jet-lang.org".to_string())
    });

    let config = PublishConfig::default();
    let mut publisher = Publisher::new(config)?;
    publisher.logout(&registry)?;

    print_success(format!("Logged out from {}", registry));

    Ok(())
}

// =============================================================================
// Update Command
// =============================================================================

async fn cmd_update(package: Option<String>, dry_run: bool, aggressive: bool) -> Result<()> {
    if !dry_run {
        return Err(CliError::new(
            "Remote dependency update is not supported in Jet 1.0 yet. Use --dry-run to inspect dependency graph only.",
        )
        .into());
    }

    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
    write!(&mut stdout, "Updating")?;
    stdout.reset()?;
    writeln!(&mut stdout, " dependencies...")?;

    if let Some(ref pkg) = package {
        writeln!(&mut stdout, "  Package: {}", pkg)?;
    }

    if aggressive {
        writeln!(&mut stdout, "  Aggressive mode enabled")?;
    }

    if dry_run {
        writeln!(&mut stdout, "  (Dry run - not actually updating)")?;
    }

    writeln!(
        &mut stdout,
        "  Remote dependency resolution is currently unsupported in Jet 1.0."
    )?;
    writeln!(
        &mut stdout,
        "  This dry-run only reports intent; no lockfile changes were made."
    )?;

    Ok(())
}

// =============================================================================
// Clean Command
// =============================================================================

async fn cmd_clean(doc: bool, release: bool) -> Result<()> {
    let (manifest, project_root) = read_project_manifest()?;

    let target_dir = project_root.join(manifest.target_dir());

    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    if doc {
        let docs_dir = target_dir.join("doc");
        if docs_dir.exists() {
            std::fs::remove_dir_all(&docs_dir)?;
            writeln!(
                &mut stdout,
                "Cleaned documentation at {}",
                docs_dir.display()
            )?;
        }
    } else if release {
        let release_dir = target_dir.join("release");
        if release_dir.exists() {
            std::fs::remove_dir_all(&release_dir)?;
            writeln!(
                &mut stdout,
                "Cleaned release artifacts at {}",
                release_dir.display()
            )?;
        }
    } else {
        if target_dir.exists() {
            std::fs::remove_dir_all(&target_dir)?;
            writeln!(
                &mut stdout,
                "Cleaned target directory at {}",
                target_dir.display()
            )?;
        }
    }

    Ok(())
}

// =============================================================================
// Jet Subcommands (Install, Search, Show)
// =============================================================================

async fn cmd_install(
    name: String,
    version: Option<String>,
    registry: Option<String>,
) -> Result<()> {
    let registry = registry.unwrap_or_else(|| {
        std::env::var("JET_REGISTRY").unwrap_or_else(|_| "https://crates.jet-lang.org".to_string())
    });

    let install_dir = dirs::home_dir()
        .ok_or_else(|| anyhow::anyhow!("Could not determine home directory"))?
        .join(".jet")
        .join("bin");

    std::fs::create_dir_all(&install_dir)?;

    publish::install_package(&registry, &name, version.as_deref(), &install_dir).await?;

    print_success(format!("Installed {} to {}", name, install_dir.display()));

    Ok(())
}

async fn cmd_search(query: String, limit: usize, registry: Option<String>) -> Result<()> {
    let registry = registry.unwrap_or_else(|| {
        std::env::var("JET_REGISTRY").unwrap_or_else(|_| "https://crates.jet-lang.org".to_string())
    });

    let results = publish::search_packages(&registry, &query, limit).await?;

    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    if results.is_empty() {
        writeln!(&mut stdout, "No packages found matching '{}'", query)?;
    } else {
        writeln!(&mut stdout, "Found {} package(s):", results.len())?;
        writeln!(&mut stdout)?;

        for result in results {
            stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
            write!(&mut stdout, "{}", result.name)?;
            stdout.reset()?;
            writeln!(&mut stdout, " = {}", result.version)?;

            if let Some(desc) = result.description {
                writeln!(&mut stdout, "    {}", desc)?;
            }

            writeln!(&mut stdout, "    Downloads: {}", result.downloads)?;
        }
    }

    Ok(())
}

async fn cmd_show(name: String, registry: Option<String>) -> Result<()> {
    let registry = registry.unwrap_or_else(|| {
        std::env::var("JET_REGISTRY").unwrap_or_else(|_| "https://crates.jet-lang.org".to_string())
    });

    let metadata = publish::get_package_metadata(&registry, &name).await?;

    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
    writeln!(&mut stdout, "{}", metadata.name)?;
    stdout.reset()?;

    if let Some(desc) = metadata.description {
        writeln!(&mut stdout, "{}", desc)?;
    }
    writeln!(&mut stdout)?;

    writeln!(&mut stdout, "Versions: {}", metadata.versions.join(", "))?;
    writeln!(&mut stdout, "Downloads: {}", metadata.downloads)?;

    if !metadata.authors.is_empty() {
        writeln!(&mut stdout, "Authors: {}", metadata.authors.join(", "))?;
    }

    if let Some(license) = metadata.license {
        writeln!(&mut stdout, "License: {}", license)?;
    }

    if let Some(repo) = metadata.repository {
        writeln!(&mut stdout, "Repository: {}", repo)?;
    }

    if let Some(docs) = metadata.documentation {
        writeln!(&mut stdout, "Documentation: {}", docs)?;
    }

    Ok(())
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use assert_cmd::Command;
    use predicates::prelude::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_cli_version() {
        let mut cmd = Command::cargo_bin("jet").unwrap();
        cmd.arg("--version");
        cmd.assert().success();
    }

    #[test]
    fn test_cli_help() {
        let mut cmd = Command::cargo_bin("jet").unwrap();
        cmd.arg("--help");
        cmd.assert()
            .success()
            .stdout(predicate::str::contains("The Jet programming language"));
    }

    #[test]
    fn test_new_binary_project() {
        let temp_dir = TempDir::new().unwrap();
        let project_name = "test_project";

        let mut cmd = Command::cargo_bin("jet").unwrap();
        cmd.current_dir(&temp_dir).args(["new", project_name]);

        cmd.assert().success();

        let project_path = temp_dir.path().join(project_name);
        assert!(project_path.exists());
        assert!(project_path.join("src").exists());
        assert!(project_path.join("src/main.jet").exists());
        assert!(project_path.join("jet.toml").exists());
        assert!(project_path.join(".gitignore").exists());

        // Check jet.toml content
        let toml_content = fs::read_to_string(project_path.join("jet.toml")).unwrap();
        assert!(toml_content.contains(&format!("name = \"{}\"", project_name)));
    }

    #[test]
    fn test_new_library_project() {
        let temp_dir = TempDir::new().unwrap();
        let project_name = "test_lib";

        let mut cmd = Command::cargo_bin("jet").unwrap();
        cmd.current_dir(&temp_dir)
            .args(["new", "--lib", project_name]);

        cmd.assert().success();

        let project_path = temp_dir.path().join(project_name);
        assert!(project_path.join("src/lib.jet").exists());

        // Check lib.jet content
        let lib_content = fs::read_to_string(project_path.join("src/lib.jet")).unwrap();
        assert!(lib_content.contains("pub fn hello"));
    }

    #[test]
    fn test_new_invalid_name() {
        let temp_dir = TempDir::new().unwrap();

        let mut cmd = Command::cargo_bin("jet").unwrap();
        cmd.current_dir(&temp_dir).args(["new", "123_invalid"]);

        cmd.assert()
            .failure()
            .stderr(predicate::str::contains("Invalid project name"));
    }

    #[test]
    fn test_valid_project_names() {
        assert!(is_valid_project_name("my_project"));
        assert!(is_valid_project_name("my-project"));
        assert!(is_valid_project_name("myProject"));
        assert!(is_valid_project_name("MyProject123"));
    }

    #[test]
    fn test_invalid_project_names() {
        assert!(!is_valid_project_name(""));
        assert!(!is_valid_project_name("123project"));
        assert!(!is_valid_project_name("my project"));
        assert!(!is_valid_project_name("my.project"));
    }
}
