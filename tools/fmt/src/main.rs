//! Jet Code Formatter CLI
//!
//! Command-line interface for formatting Jet source files.
//!
//! # Usage
//!
//! ```bash
//! # Format all .jet files in the current directory
//! jet-fmt
//!
//! # Format specific files
//! jet-fmt src/main.jet src/lib.jet
//!
//! # Check formatting without modifying files
//! jet-fmt --check
//!
//! # Read from stdin, write to stdout
//! cat file.jet | jet-fmt --stdin
//! ```

use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process;

use anyhow::{Context, Result};

/// Jet code formatter
#[derive(Debug)]
struct Cli {
    /// Files to format
    files: Vec<PathBuf>,
    /// Check mode - don't modify files, just verify
    check: bool,
    /// Read from stdin
    stdin: bool,
    /// Write to stdout instead of modifying files
    #[allow(dead_code)]
    stdout: bool,
    /// Emit mode
    emit: EmitMode,
    /// Backup original files
    backup: bool,
    /// Verbose output
    verbose: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EmitMode {
    /// Modify files in place
    Files,
    /// Write to stdout
    Stdout,
    /// Check only
    Check,
}

impl Cli {
    fn parse() -> Self {
        let mut args = std::env::args().skip(1);
        let mut files = Vec::new();
        let mut check = false;
        let mut stdin = false;
        let mut stdout = false;
        let mut backup = false;
        let mut verbose = false;

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "--check" => check = true,
                "--stdin" => stdin = true,
                "--stdout" => stdout = true,
                "--emit" => {
                    if let Some(mode) = args.next() {
                        match mode.as_str() {
                            "files" => stdout = false,
                            "stdout" => stdout = true,
                            _ => eprintln!("Warning: unknown emit mode '{}', using default", mode),
                        }
                    }
                }
                "--backup" => backup = true,
                "-v" | "--verbose" => verbose = true,
                "-h" | "--help" => {
                    print_help();
                    process::exit(0);
                }
                "-V" | "--version" => {
                    println!("jet-fmt {}", env!("CARGO_PKG_VERSION"));
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

        let emit = if check {
            EmitMode::Check
        } else if stdout || stdin {
            EmitMode::Stdout
        } else {
            EmitMode::Files
        };

        Self {
            files,
            check,
            stdin,
            stdout,
            emit,
            backup,
            verbose,
        }
    }
}

fn print_help() {
    println!(
        r#"Jet Code Formatter {}

USAGE:
    jet-fmt [OPTIONS] [FILES...]

ARGS:
    [FILES...]    Files to format (defaults to all .jet files in current directory)

OPTIONS:
    --check          Check formatting without modifying files
    --stdin          Read source from stdin
    --stdout         Write formatted source to stdout
    --emit <MODE>    Output mode: files|stdout
    --backup         Create .jet.bak backup files
    -v, --verbose    Verbose output
    -h, --help       Print help
    -V, --version    Print version

EXAMPLES:
    jet-fmt                    # Format all .jet files
    jet-fmt src/main.jet       # Format specific file
    jet-fmt --check            # Check all files
    cat file.jet | jet-fmt --stdin
"#,
        env!("CARGO_PKG_VERSION")
    );
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    if cli.verbose {
        eprintln!("jet-fmt: {:?}", cli);
    }

    // Handle stdin mode
    if cli.stdin {
        let mut source = String::new();
        io::stdin().read_to_string(&mut source)?;

        match jet_fmt::format_source(&source) {
            Ok(formatted) => {
                if cli.check {
                    if source == formatted {
                        println!("stdin: OK");
                        process::exit(0);
                    } else {
                        println!("stdin: Would reformat");
                        process::exit(1);
                    }
                } else {
                    print!("{}", formatted);
                    process::exit(0);
                }
            }
            Err(e) => {
                eprintln!("Error: failed to format stdin: {}", e);
                process::exit(2);
            }
        }
    }

    // Collect files to format
    let files = if cli.files.is_empty() {
        collect_jet_files(Path::new("."))?
    } else {
        cli.files.clone()
    };

    if files.is_empty() {
        eprintln!("Error: no .jet files found");
        process::exit(1);
    }

    // Format files
    let mut exit_code = 0;
    let mut formatted_count = 0;
    let mut unchanged_count = 0;
    let mut error_count = 0;

    for file in &files {
        if cli.verbose {
            eprintln!("Formatting: {}", file.display());
        }

        match format_file(file, &cli) {
            Ok(Formatted::Changed) => {
                formatted_count += 1;
                if cli.check {
                    println!("{}: Would reformat", file.display());
                    exit_code = 1;
                } else if cli.verbose {
                    println!("{}: Reformatted", file.display());
                }
            }
            Ok(Formatted::Unchanged) => {
                unchanged_count += 1;
                if cli.verbose {
                    println!("{}: OK", file.display());
                }
            }
            Err(e) => {
                error_count += 1;
                eprintln!("Error formatting {}: {}", file.display(), e);
                exit_code = 2;
            }
        }
    }

    // Print summary
    if cli.verbose || cli.check {
        eprintln!(
            "\nSummary: {} reformatted, {} unchanged, {} errors",
            formatted_count, unchanged_count, error_count
        );
    }

    process::exit(exit_code);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Formatted {
    Changed,
    Unchanged,
}

fn format_file(path: &Path, cli: &Cli) -> Result<Formatted> {
    let source =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;

    let formatted =
        jet_fmt::format_source(&source).map_err(|e| anyhow::anyhow!("parse error: {}", e))?;

    if source == formatted {
        return Ok(Formatted::Unchanged);
    }

    match cli.emit {
        EmitMode::Check => {
            // Just report, don't modify
        }
        EmitMode::Stdout => {
            print!("{}", formatted);
        }
        EmitMode::Files => {
            // Create backup if requested
            if cli.backup {
                let backup_path = path.with_extension("jet.bak");
                fs::write(&backup_path, &source).with_context(|| {
                    format!("failed to create backup {}", backup_path.display())
                })?;
            }

            // Write formatted content
            fs::write(path, formatted)
                .with_context(|| format!("failed to write {}", path.display()))?;
        }
    }

    Ok(Formatted::Changed)
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
    use super::*;

    #[test]
    fn test_emit_mode() {
        let cli = Cli {
            files: vec![],
            check: true,
            stdin: false,
            stdout: false,
            emit: EmitMode::Check,
            backup: false,
            verbose: false,
        };
        assert_eq!(cli.emit, EmitMode::Check);
    }
}
