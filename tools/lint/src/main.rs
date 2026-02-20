//! Jet Linter CLI
//!
//! Command-line interface for linting Jet source files.
//!
//! # Usage
//!
//! ```bash
//! # Lint all .jet files in the current directory
//! jet-lint
//!
//! # Lint specific files
//! jet-lint src/main.jet src/lib.jet
//!
//! # Auto-fix issues where possible
//! jet-lint --fix
//!
//! # Treat warnings as errors
//! jet-lint --deny warnings
//! ```

use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process;

use anyhow::Result;

use jet_lint::{LintConfig, LintLevel, Linter, OutputFormat};

/// Jet linter CLI
#[derive(Debug)]
struct Cli {
    /// Files to lint
    files: Vec<PathBuf>,
    /// Auto-fix issues where possible
    fix: bool,
    /// Deny specific lint
    deny: Vec<String>,
    /// Warn for specific lint
    warn: Vec<String>,
    /// Allow specific lint
    allow: Vec<String>,
    /// Forbid specific lint (unoverrideable)
    forbid: Vec<String>,
    /// Default lint level
    level: Option<LintLevel>,
    /// Output format
    output_format: OutputFormat,
    /// Read from stdin
    stdin: bool,
    /// Verbose output
    verbose: bool,
}

impl Cli {
    fn parse() -> Self {
        let mut args = std::env::args().skip(1);
        let mut files = Vec::new();
        let mut fix = false;
        let mut deny = Vec::new();
        let mut warn = Vec::new();
        let mut allow = Vec::new();
        let mut forbid = Vec::new();
        let mut level = None;
        let mut output_format = OutputFormat::Human;
        let mut stdin = false;
        let mut verbose = false;

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "--fix" => fix = true,
                "--deny" => {
                    if let Some(lint) = args.next() {
                        deny.push(lint);
                    }
                }
                "--warn" => {
                    if let Some(lint) = args.next() {
                        warn.push(lint);
                    }
                }
                "--allow" => {
                    if let Some(lint) = args.next() {
                        allow.push(lint);
                    }
                }
                "--forbid" => {
                    if let Some(lint) = args.next() {
                        forbid.push(lint);
                    }
                }
                "--level" => {
                    if let Some(l) = args.next() {
                        level = LintLevel::from_str(&l);
                    }
                }
                "--output-format" => {
                    if let Some(fmt) = args.next() {
                        if let Some(fmt) = OutputFormat::from_str(&fmt) {
                            output_format = fmt;
                        } else {
                            eprintln!("Warning: unknown output format '{}', using human", fmt);
                        }
                    }
                }
                "--stdin" => stdin = true,
                "-v" | "--verbose" => verbose = true,
                "-h" | "--help" => {
                    print_help();
                    process::exit(0);
                }
                "-V" | "--version" => {
                    println!("jet-lint {}", env!("CARGO_PKG_VERSION"));
                    process::exit(0);
                }
                _ => {
                    if arg.starts_with('-') {
                        eprintln!("Warning: unknown option '{}'", arg);
                    } else {
                        files.push(PathBuf::from(arg));
                    }
                }
            }
        }

        Self {
            files,
            fix,
            deny,
            warn,
            allow,
            forbid,
            level,
            output_format,
            stdin,
            verbose,
        }
    }

    /// Build lint configuration from CLI arguments
    fn build_config(&self) -> LintConfig {
        let mut config = LintConfig::default();

        if let Some(level) = self.level {
            config = config.with_default_level(level);
        }

        for lint in &self.deny {
            if lint == "warnings" {
                config = config.with_deny_warnings(true);
            } else {
                config = config.with_override(lint, LintLevel::Deny);
            }
        }

        for lint in &self.warn {
            config = config.with_override(lint, LintLevel::Warn);
        }

        for lint in &self.allow {
            config = config.with_override(lint, LintLevel::Allow);
        }

        for lint in &self.forbid {
            config = config.with_override(lint, LintLevel::Forbid);
        }

        config = config.with_auto_fix(self.fix);
        config = config.with_output_format(self.output_format);

        config
    }
}

fn print_help() {
    println!(
        r#"Jet Linter {}

USAGE:
    jet-lint [OPTIONS] [FILES...]

ARGS:
    [FILES...]    Files to lint (defaults to all .jet files in current directory)

OPTIONS:
    --fix                   Automatically fix issues where possible
    --deny <LINT>           Treat specific lint as error
    --warn <LINT>           Treat specific lint as warning
    --allow <LINT>          Allow specific lint
    --forbid <LINT>         Forbid specific lint (unoverrideable)
    --level <LEVEL>         Set default lint level: allow|warn|deny|forbid
    --output-format <FMT>   Output format: human|json|short
    --stdin                 Read source from stdin
    -v, --verbose           Verbose output
    -h, --help              Print help
    -V, --version           Print version

EXAMPLES:
    jet-lint                    # Lint all .jet files
    jet-lint src/main.jet       # Lint specific file
    jet-lint --fix              # Fix auto-fixable issues
    jet-lint --deny warnings    # Treat all warnings as errors
    jet-lint --deny unused      # Fail on unused code
"#,
        env!("CARGO_PKG_VERSION")
    );
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    if cli.verbose {
        eprintln!("jet-lint: {:?}", cli);
    }

    let config = cli.build_config();
    let mut linter = Linter::new(config);

    // Handle stdin mode
    if cli.stdin {
        let mut source = String::new();
        io::stdin().read_to_string(&mut source)?;

        match linter.lint_source(&source, Some("stdin".to_string())) {
            Ok(result) => {
                print_results(&result, &cli.output_format, "stdin");
                if result.has_errors() {
                    process::exit(4);
                }
                process::exit(0);
            }
            Err(e) => {
                eprintln!("Error: failed to lint stdin: {}", e);
                process::exit(2);
            }
        }
    }

    // Collect files to lint
    let files = if cli.files.is_empty() {
        collect_jet_files(Path::new("."))?
    } else {
        cli.files.clone()
    };

    if files.is_empty() {
        eprintln!("Error: no .jet files found");
        process::exit(1);
    }

    // Lint files
    let mut total_errors = 0;
    let mut total_warnings = 0;
    let mut total_fixable = 0;
    let mut file_results = Vec::new();

    for file in &files {
        if cli.verbose {
            eprintln!("Linting: {}", file.display());
        }

        match linter.lint_file(file) {
            Ok(result) => {
                total_errors += result.error_count;
                total_warnings += result.warning_count;
                total_fixable += result.fixable_count;
                file_results.push((file.clone(), result));
            }
            Err(e) => {
                eprintln!("Error linting {}: {}", file.display(), e);
            }
        }
    }

    // Print results
    match cli.output_format {
        OutputFormat::Human => {
            for (file, result) in &file_results {
                if !result.violations.is_empty() {
                    println!("\n{}", file.display());
                    for violation in &result.violations {
                        print_violation_human(violation);
                    }
                }
            }

            // Print summary
            if total_errors > 0 || total_warnings > 0 {
                println!("\n");
            }

            if total_errors > 0 {
                println!("error: could not lint due to {} error(s)", total_errors);
            }

            if total_warnings > 0 {
                println!("warning: {} warning(s) found", total_warnings);
            }

            if total_fixable > 0 && !cli.fix {
                println!("note: {} issue(s) can be fixed with `--fix`", total_fixable);
            }

            if total_errors == 0 && total_warnings == 0 {
                println!("✓ No issues found");
            }
        }
        OutputFormat::Short => {
            for (file, result) in &file_results {
                for violation in &result.violations {
                    print_violation_short(file, violation);
                }
            }
        }
        OutputFormat::Json => {
            print_results_json(&file_results);
        }
    }

    // Exit with appropriate code
    if total_errors > 0 {
        process::exit(4);
    }

    Ok(())
}

fn print_violation_human(violation: &jet_lint::LintViolation) {
    let level_str = match violation.level {
        LintLevel::Error | LintLevel::Deny | LintLevel::Forbid => "error".to_string(),
        LintLevel::Warn => "warning".to_string(),
        LintLevel::Info => "info".to_string(),
        LintLevel::Allow => "note".to_string(),
    };

    let color_code = match violation.level {
        LintLevel::Error | LintLevel::Deny | LintLevel::Forbid => "\x1b[31m", // Red
        LintLevel::Warn => "\x1b[33m",                                        // Yellow
        LintLevel::Info => "\x1b[34m",                                        // Blue
        LintLevel::Allow => "\x1b[36m",                                       // Cyan
    };

    let reset_code = "\x1b[0m";

    println!(
        "  {}{}: {}{} [{}]",
        color_code, level_str, reset_code, violation.message, violation.lint_name
    );

    if let Some(ref suggestion) = violation.suggestion {
        println!("  \x1b[32mhelp:\x1b[0m {}", suggestion);
    }
}

fn print_violation_short(file: &Path, violation: &jet_lint::LintViolation) {
    let level_str = match violation.level {
        LintLevel::Error | LintLevel::Deny | LintLevel::Forbid => "error",
        LintLevel::Warn => "warning",
        LintLevel::Info => "info",
        LintLevel::Allow => "note",
    };

    println!(
        "{}:{}: {}: {} [{}]",
        file.display(),
        violation.span.start,
        level_str,
        violation.message,
        violation.lint_name
    );
}

fn print_results(result: &jet_lint::LintResult, format: &OutputFormat, filename: &str) {
    match format {
        OutputFormat::Human => {
            if !result.violations.is_empty() {
                println!("{}", filename);
                for violation in &result.violations {
                    print_violation_human(violation);
                }
            }
        }
        OutputFormat::Short => {
            let path = Path::new(filename);
            for violation in &result.violations {
                print_violation_short(path, violation);
            }
        }
        OutputFormat::Json => {
            // JSON output for single file
            let json = serde_json::json!({
                "file": filename,
                "violations": result.violations.iter().map(|v| {
                    serde_json::json!({
                        "lint": v.lint_name,
                        "level": format!("{:?}", v.level).to_lowercase(),
                        "message": v.message,
                        "span": {
                            "start": v.span.start,
                            "end": v.span.end,
                        },
                        "suggestion": v.suggestion,
                        "fixable": v.fixable,
                    })
                }).collect::<Vec<_>>(),
                "error_count": result.error_count,
                "warning_count": result.warning_count,
                "fixable_count": result.fixable_count,
            });
            println!("{}", serde_json::to_string_pretty(&json).unwrap());
        }
    }
}

fn print_results_json(file_results: &[(PathBuf, jet_lint::LintResult)]) {
    let json = serde_json::json!({
        "files": file_results.iter().map(|(file, result)| {
            serde_json::json!({
                "path": file.to_string_lossy().to_string(),
                "violations": result.violations.iter().map(|v| {
                    serde_json::json!({
                        "lint": v.lint_name,
                        "level": format!("{:?}", v.level).to_lowercase(),
                        "message": v.message,
                        "span": {
                            "start": v.span.start,
                            "end": v.span.end,
                        },
                        "suggestion": v.suggestion,
                        "fixable": v.fixable,
                    })
                }).collect::<Vec<_>>(),
            })
        }).collect::<Vec<_>>(),
    });
    println!("{}", serde_json::to_string_pretty(&json).unwrap());
}

fn collect_jet_files(dir: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    if dir.is_file() {
        if dir.extension().map(|e| e == "jet").unwrap_or(false) {
            files.push(dir.to_path_buf());
        }
        return Ok(files);
    }

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() {
            if path.extension().map(|e| e == "jet").unwrap_or(false) {
                files.push(path);
            }
        } else if path.is_dir() {
            // Skip hidden directories and target
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if !name.starts_with('.') && name != "target" {
                files.extend(collect_jet_files(&path)?);
            }
        }
    }

    Ok(files)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_cli_parse() {
        // This test would verify CLI parsing
        // In a real test, we'd use a more sophisticated approach
    }
}
