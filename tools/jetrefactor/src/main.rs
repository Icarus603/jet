//! CLI for the Jet refactoring tool
//!
//! This tool provides AI-safe refactoring operations for Jet source code.

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use jet_refactor::{RefactoringConfig, RefactoringEngine, RefactoringKind};
use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "jet-refactor")]
#[command(about = "AI-safe refactoring tool for Jet")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Path to the source file
    #[arg(short, long)]
    file: PathBuf,

    /// Start position (line:column)
    #[arg(short, long)]
    start: String,

    /// End position (line:column)
    #[arg(short, long)]
    end: Option<String>,

    /// Don't generate tracking annotations
    #[arg(long)]
    no_annotations: bool,

    /// Don't require semantic verification
    #[arg(long)]
    no_verify: bool,

    /// Who/what is applying the refactoring
    #[arg(short, long, default_value = "cli")]
    applied_by: String,
}

#[derive(Subcommand)]
enum Commands {
    /// Extract selected code into a variable
    ExtractVariable,
    /// Extract selected code into a function
    ExtractFunction,
    /// Inline a variable
    InlineVariable,
    /// Inline a function call
    InlineFunction,
    /// Rename a symbol
    Rename {
        /// The new name
        new_name: String,
    },
    /// List available refactorings at the given position
    List,
    /// Analyze code and suggest refactorings
    Suggest,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Read the source file
    let source = fs::read_to_string(&cli.file)
        .with_context(|| format!("Failed to read file: {}", cli.file.display()))?;

    // Parse positions
    let start_pos = parse_position(&cli.start)?;
    let end_pos = cli
        .end
        .as_ref()
        .map(|e| parse_position(e))
        .transpose()?
        .unwrap_or(start_pos);

    let span = jet_diagnostics::Span::new(start_pos, end_pos);

    // Create configuration
    let config = RefactoringConfig {
        require_semantic_verification: !cli.no_verify,
        generate_tracking_annotations: !cli.no_annotations,
        applied_by: cli.applied_by,
        ..Default::default()
    };

    let engine = RefactoringEngine::with_config(config);

    match cli.command {
        Commands::ExtractVariable => {
            let result = engine
                .apply_refactoring(RefactoringKind::ExtractVariable, &source, span)
                .map_err(|e| anyhow::anyhow!("Refactoring failed: {:?}", e))?;

            println!("{}", result.source);
        }
        Commands::ExtractFunction => {
            let result = engine
                .apply_refactoring(RefactoringKind::ExtractFunction, &source, span)
                .map_err(|e| anyhow::anyhow!("Refactoring failed: {:?}", e))?;

            println!("{}", result.source);
        }
        Commands::InlineVariable => {
            let result = engine
                .apply_refactoring(RefactoringKind::InlineVariable, &source, span)
                .map_err(|e| anyhow::anyhow!("Refactoring failed: {:?}", e))?;

            println!("{}", result.source);
        }
        Commands::InlineFunction => {
            let result = engine
                .apply_refactoring(RefactoringKind::InlineFunction, &source, span)
                .map_err(|e| anyhow::anyhow!("Refactoring failed: {:?}", e))?;

            println!("{}", result.source);
        }
        Commands::Rename { new_name } => {
            let result = jet_refactor::rename::rename_symbol_to(
                &source,
                &parse_ast(&source)?,
                span,
                Some(&new_name),
                &RefactoringConfig::default(),
            )
            .map_err(|e| anyhow::anyhow!("Rename failed: {:?}", e))?;

            println!("{}", result.source);
        }
        Commands::List => {
            let ast = parse_ast(&source)?;
            let refactorings = engine.available_refactorings(&ast, span);

            println!("Available refactorings:");
            for (kind, description) in refactorings {
                println!("  - {}: {}", kind, description);
            }
        }
        Commands::Suggest => {
            let ast = parse_ast(&source)?;
            let analyzer = jet_refactor::suggestions::SuggestionAnalyzer::new(
                RefactoringConfig::default(),
            );
            let suggestions = analyzer.analyze(&ast);

            println!("Refactoring suggestions:");
            for suggestion in suggestions {
                println!(
                    "  - {} (confidence: {:.0}%): {}",
                    suggestion.kind,
                    suggestion.confidence * 100.0,
                    suggestion.description
                );
                if let Some(example) = suggestion.example {
                    println!("    Example: {}", example);
                }
            }
        }
    }

    Ok(())
}

fn parse_position(s: &str) -> Result<usize> {
    // Parse "line:column" format
    let parts: Vec<&str> = s.split(':').collect();
    if parts.len() != 2 {
        anyhow::bail!("Position must be in format 'line:column'");
    }

    let line: usize = parts[0]
        .parse()
        .with_context(|| format!("Invalid line number: {}", parts[0]))?;
    let column: usize = parts[1]
        .parse()
        .with_context(|| format!("Invalid column number: {}", parts[1]))?;

    // Convert line:column to byte offset (simplified)
    // In practice, this would need the actual source to calculate
    Ok(line * 1000 + column)
}

fn parse_ast(source: &str) -> Result<jet_parser::ast::Module> {
    let tokens = jet_lexer::lex(source).map_err(|e| anyhow::anyhow!("Lex error: {:?}", e))?;

    let mut parser = jet_parser::Parser::new(tokens);
    parser
        .parse_module()
        .map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))
}
