//! Jet REPL - Interactive evaluation environment for the Jet programming language

use anyhow::Result;
use clap::Parser;

#[derive(Parser)]
#[command(name = "jet-repl")]
#[command(about = "Interactive REPL for the Jet programming language")]
struct Args {
    /// Language edition
    #[arg(long, default_value = "2024")]
    edition: String,

    /// Enable features
    #[arg(long)]
    features: Vec<String>,

    /// Execute a command and exit
    #[arg(short, long)]
    command: Option<String>,

    /// Load a file before starting the REPL
    #[arg(short, long)]
    load: Option<String>,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    // If a command is provided, execute it and exit
    if let Some(cmd) = args.command {
        let _repl = jet_repl::Repl::new()?;
        // Execute command logic here
        println!("Executing: {}", cmd);
        return Ok(());
    }

    // Run the REPL
    jet_repl::run_repl()
}
