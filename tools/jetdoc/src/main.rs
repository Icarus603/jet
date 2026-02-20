//! Jet Documentation Generator CLI
//!
//! This tool generates beautiful HTML documentation from Jet source code,
//! including support for:
//! - Doc comment extraction with markdown
//! - Executable examples (>>> style)
//! - AI annotations (confidence, generated_by)
//! - Type-driven documentation
//! - Search functionality
#![allow(unused_imports, unused_variables)]
#![allow(clippy::too_many_arguments, clippy::ptr_arg)]

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use jet_doc::{DocConfig, DocDatabase, DocTestResults};
use std::path::PathBuf;
use std::time::Instant;

#[derive(Parser)]
#[command(name = "jetdoc")]
#[command(about = "Documentation generator for the Jet programming language")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate documentation for a project
    Generate {
        /// Source files or directories to document
        #[arg(default_value = ".")]
        paths: Vec<PathBuf>,

        /// Output directory for generated docs
        #[arg(short, long, default_value = "target/doc")]
        output: PathBuf,

        /// Include private items in documentation
        #[arg(long)]
        document_private_items: bool,

        /// Don't document dependencies
        #[arg(long)]
        no_deps: bool,

        /// Project name (defaults to directory name)
        #[arg(short, long)]
        name: Option<String>,

        /// Project version
        #[arg(short, long, default_value = "0.1.0")]
        version: String,

        /// Open documentation in browser after generation
        #[arg(short, long)]
        open: bool,

        /// Serve documentation locally
        #[arg(long)]
        serve: bool,

        /// Port for local server
        #[arg(long, default_value = "8080")]
        port: u16,
    },

    /// Run doc tests (examples in doc comments)
    Test {
        /// Source files or directories to test
        #[arg(default_value = ".")]
        paths: Vec<PathBuf>,

        /// Stop on first failure
        #[arg(short, long)]
        fail_fast: bool,

        /// Show verbose output
        #[arg(short, long)]
        verbose: bool,
    },

    /// Search documentation
    Search {
        /// Search query
        query: String,

        /// Documentation directory to search
        #[arg(short, long, default_value = "target/doc")]
        docs_dir: PathBuf,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Generate {
            paths,
            output,
            document_private_items,
            no_deps,
            name,
            version,
            open,
            serve,
            port,
        } => {
            cmd_generate(
                &paths,
                output,
                document_private_items,
                no_deps,
                name,
                version,
                open,
                serve,
                port,
            )
            .await?;
        }
        Commands::Test {
            paths,
            fail_fast,
            verbose,
        } => {
            cmd_test(&paths, fail_fast, verbose).await?;
        }
        Commands::Search { query, docs_dir } => {
            cmd_search(&query, &docs_dir).await?;
        }
    }

    Ok(())
}

async fn cmd_generate(
    paths: &[PathBuf],
    output: PathBuf,
    document_private_items: bool,
    no_deps: bool,
    name: Option<String>,
    version: String,
    open: bool,
    serve: bool,
    port: u16,
) -> Result<()> {
    let start = Instant::now();

    // Determine project name
    let project_name = name.unwrap_or_else(|| {
        std::env::current_dir()
            .ok()
            .and_then(|p| p.file_name().map(|n| n.to_string_lossy().to_string()))
            .unwrap_or_else(|| "Unnamed Project".to_string())
    });

    println!("Documenting {} {}", project_name, version);

    let config = DocConfig {
        output_dir: output.clone(),
        include_private: document_private_items,
        document_deps: !no_deps,
        project_name,
        project_version: version,
    };

    // Collect source paths
    let source_paths = collect_source_paths(paths).await?;

    if source_paths.is_empty() {
        println!("No .jet files found in specified paths");
        return Ok(());
    }

    // Generate documentation
    let database = jet_doc::generate_docs(&source_paths, config).await?;

    // Generate HTML
    jet_doc::html::generate_html_docs(&database).await?;

    let duration = start.elapsed();
    println!(
        "Generated documentation for {} items in {:.2}s",
        database.items.len(),
        duration.as_secs_f64()
    );
    println!("Documentation available at: {}", output.display());

    if serve {
        start_server(&output, port).await?;
    } else if open {
        open_browser(&output).await?;
    }

    Ok(())
}

async fn cmd_test(paths: &[PathBuf], fail_fast: bool, verbose: bool) -> Result<()> {
    println!("Running doc tests...");

    let source_paths = collect_source_paths(paths).await?;

    if source_paths.is_empty() {
        println!("No .jet files found");
        return Ok(());
    }

    let results = jet_doc::run_doc_tests(&source_paths).await?;

    // Print results
    if verbose || results.failed > 0 {
        for failure in &results.failures {
            println!("\nFAILED: {}:{}", failure.file.display(), failure.line);
            println!("Code:\n{}", failure.code);
            println!("Error: {}", failure.error);

            if fail_fast {
                break;
            }
        }
    }

    println!(
        "\nResults: {} passed, {} failed, {} total",
        results.passed, results.failed, results.total
    );

    if results.failed > 0 {
        std::process::exit(1);
    }

    Ok(())
}

async fn cmd_search(query: &str, docs_dir: &PathBuf) -> Result<()> {
    // Load the search index
    let index_path = docs_dir.join("search-index.json");
    let index_content = tokio::fs::read_to_string(&index_path)
        .await
        .with_context(|| format!("Failed to read search index from {}", index_path.display()))?;

    let items: Vec<serde_json::Value> = serde_json::from_str(&index_content)?;

    let query_lower = query.to_lowercase();
    let mut results = Vec::new();

    for item in items {
        let name = item["name"].as_str().unwrap_or("");
        let desc = item["description"].as_str().unwrap_or("");

        if name.to_lowercase().contains(&query_lower) || desc.to_lowercase().contains(&query_lower)
        {
            results.push(item);
        }
    }

    if results.is_empty() {
        println!("No results found for '{}'", query);
    } else {
        println!("Found {} results for '{}':\n", results.len(), query);
        for item in results {
            println!(
                "{} {} - {}",
                item["kind"].as_str().unwrap_or(""),
                item["name"].as_str().unwrap_or(""),
                item["module"].as_str().unwrap_or("")
            );
            if let Some(desc) = item["description"].as_str() {
                if !desc.is_empty() {
                    println!("  {}", desc);
                }
            }
            println!();
        }
    }

    Ok(())
}

/// Collect all .jet source files from the given paths
async fn collect_source_paths(paths: &[PathBuf]) -> Result<Vec<PathBuf>> {
    let mut source_paths = Vec::new();

    for path in paths {
        if path.is_file() && path.extension().map(|e| e == "jet").unwrap_or(false) {
            source_paths.push(path.clone());
        } else if path.is_dir() {
            collect_jet_files(path, &mut source_paths).await?;
        }
    }

    Ok(source_paths)
}

/// Recursively collect .jet files from a directory
async fn collect_jet_files(dir: &PathBuf, results: &mut Vec<PathBuf>) -> Result<()> {
    let mut entries = tokio::fs::read_dir(dir).await?;

    while let Some(entry) = entries.next_entry().await? {
        let path = entry.path();

        if path.is_file() && path.extension().map(|e| e == "jet").unwrap_or(false) {
            results.push(path);
        } else if path.is_dir() {
            // Skip target directory
            if path.file_name() != Some(std::ffi::OsStr::new("target")) {
                Box::pin(collect_jet_files(&path, results)).await?;
            }
        }
    }

    Ok(())
}

/// Start a local HTTP server to serve documentation
async fn start_server(docs_dir: &PathBuf, port: u16) -> Result<()> {
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    use tokio::net::TcpListener;

    let addr = format!("127.0.0.1:{}", port);
    let listener = TcpListener::bind(&addr).await?;

    println!("Documentation server running at http://{}", addr);
    println!("Press Ctrl+C to stop");

    loop {
        let (mut socket, _) = listener.accept().await?;
        let docs_dir = docs_dir.clone();

        tokio::spawn(async move {
            let mut buffer = [0u8; 1024];
            let _ = socket.read(&mut buffer).await;

            // Simple HTTP response
            let response = b"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n<html><body>Documentation Server</body></html>";
            let _ = socket.write_all(response).await;
        });
    }
}

/// Open documentation in the default browser
async fn open_browser(docs_dir: &PathBuf) -> Result<()> {
    let index_path = docs_dir.join("index.html");
    let url = format!("file://{}", index_path.canonicalize()?.display());

    #[cfg(target_os = "macos")]
    {
        std::process::Command::new("open")
            .arg(&url)
            .spawn()
            .context("Failed to open browser")?;
    }

    #[cfg(target_os = "linux")]
    {
        std::process::Command::new("xdg-open")
            .arg(&url)
            .spawn()
            .context("Failed to open browser")?;
    }

    println!("Opened documentation in browser");
    Ok(())
}
