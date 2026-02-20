//! Jet REPL (Read-Eval-Print Loop)
//!
//! This crate provides an interactive evaluation environment for the Jet programming language.

pub mod commands;
pub mod config;
pub mod evaluator;
pub mod history;

use anyhow::Result;
use colored::Colorize;
use commands::{Command, CommandParser};
use config::ReplConfig;
use evaluator::Evaluator;
use history::HistoryManager;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, EventHandler, KeyCode, KeyEvent, Modifiers};

/// The Jet REPL session
pub struct Repl {
    config: ReplConfig,
    evaluator: Evaluator,
    history: HistoryManager,
    editor: DefaultEditor,
    multiline_buffer: String,
    in_multiline: bool,
}

impl Repl {
    /// Create a new REPL instance
    pub fn new() -> Result<Self> {
        let config = ReplConfig::load()?;
        let evaluator = Evaluator::new();
        let history = HistoryManager::new(&config.history_file, config.history_size)?;

        let mut editor = DefaultEditor::new()?;

        // Load command history
        let _ = editor.load_history(&config.history_file);

        // Bind Ctrl+D to exit
        editor.bind_sequence(
            KeyEvent(KeyCode::Char('d'), Modifiers::CTRL),
            EventHandler::Simple(rustyline::Cmd::EndOfFile),
        );

        Ok(Self {
            config,
            evaluator,
            history,
            editor,
            multiline_buffer: String::new(),
            in_multiline: false,
        })
    }

    /// Run the REPL main loop
    pub fn run(&mut self) -> Result<()> {
        self.print_welcome();

        loop {
            let prompt = if self.in_multiline {
                &self.config.multiline_prompt
            } else {
                &self.config.prompt
            };

            match self.editor.readline(prompt) {
                Ok(line) => {
                    let line = line.trim();

                    if line.is_empty() {
                        if self.in_multiline {
                            // End of multiline input
                            let code = std::mem::take(&mut self.multiline_buffer);
                            self.in_multiline = false;
                            self.handle_input(&code)?;
                        }
                        continue;
                    }

                    // Check for REPL commands (only in single-line mode)
                    if !self.in_multiline && line.starts_with(':') {
                        if let Some(cmd) = CommandParser::parse(line) {
                            if self.handle_command(cmd)? {
                                break;
                            }
                            continue;
                        }
                    }

                    // Check for incomplete input (multiline)
                    if self.is_incomplete_input(line) {
                        self.in_multiline = true;
                        self.multiline_buffer.push_str(line);
                        self.multiline_buffer.push('\n');
                        continue;
                    }

                    // Add to history
                    let _ = self.editor.add_history_entry(line);

                    // Handle the input
                    let code = if self.in_multiline {
                        self.multiline_buffer.push_str(line);
                        std::mem::take(&mut self.multiline_buffer)
                    } else {
                        line.to_string()
                    };
                    self.in_multiline = false;

                    self.handle_input(&code)?;
                }
                Err(ReadlineError::Interrupted) => {
                    // Ctrl+C - cancel current input
                    if self.in_multiline {
                        self.in_multiline = false;
                        self.multiline_buffer.clear();
                        println!("^C");
                    } else {
                        println!("^C");
                    }
                }
                Err(ReadlineError::Eof) => {
                    // Ctrl+D - exit
                    println!();
                    self.print_goodbye();
                    break;
                }
                Err(err) => {
                    eprintln!("Error: {}", err);
                    break;
                }
            }
        }

        // Save history
        let _ = self.editor.save_history(&self.config.history_file);
        let _ = self.history.save();

        Ok(())
    }

    /// Check if the input appears to be incomplete (needs more lines)
    fn is_incomplete_input(&self, line: &str) -> bool {
        // Check for unclosed delimiters
        let mut paren_count = 0;
        let mut brace_count = 0;
        let mut bracket_count = 0;

        for c in line.chars() {
            match c {
                '(' => paren_count += 1,
                ')' => paren_count -= 1,
                '{' => brace_count += 1,
                '}' => brace_count -= 1,
                '[' => bracket_count += 1,
                ']' => bracket_count -= 1,
                _ => {}
            }
        }

        // Check for line continuation
        let ends_with_colon = line.trim_end().ends_with(':');
        let has_unclosed_delimiters = paren_count > 0 || brace_count > 0 || bracket_count > 0;

        has_unclosed_delimiters || ends_with_colon
    }

    /// Handle user input (evaluate code)
    fn handle_input(&mut self, code: &str) -> Result<()> {
        match self.evaluator.evaluate(code) {
            Ok(result) => {
                if !result.is_unit() {
                    println!("{}", result.display().cyan());
                }
            }
            Err(e) => {
                eprintln!("{} {}", "Error:".red().bold(), e);
            }
        }
        Ok(())
    }

    /// Handle REPL commands
    /// Returns true if should exit
    fn handle_command(&mut self, cmd: Command) -> Result<bool> {
        match cmd {
            Command::Quit => {
                self.print_goodbye();
                return Ok(true);
            }
            Command::Help => {
                self.print_help();
            }
            Command::Type(expr) => match self.evaluator.get_type(&expr) {
                Ok(ty) => println!("{}", ty.yellow()),
                Err(e) => eprintln!("{} {}", "Error:".red().bold(), e),
            },
            Command::Doc(symbol) => match self.evaluator.get_documentation(&symbol) {
                Ok(doc) => println!("{}", doc),
                Err(e) => eprintln!("{} {}", "Error:".red().bold(), e),
            },
            Command::Load(path) => match self.evaluator.load_module(&path) {
                Ok(_) => println!("{} {}", "Loaded:".green(), path),
                Err(e) => eprintln!("{} {}", "Error:".red().bold(), e),
            },
            Command::Reload => match self.evaluator.reload_current_module() {
                Ok(_) => println!("{}", "Reloaded current module".green()),
                Err(e) => eprintln!("{} {}", "Error:".red().bold(), e),
            },
            Command::Imports => {
                let imports = self.evaluator.list_imports();
                if imports.is_empty() {
                    println!("No imports");
                } else {
                    println!("{}", "Imports:".bold());
                    for import in imports {
                        println!("  - {}", import);
                    }
                }
            }
            Command::Vars => {
                let vars = self.evaluator.list_variables();
                if vars.is_empty() {
                    println!("No variables defined");
                } else {
                    println!("{}", "Variables:".bold());
                    for (name, ty, value) in vars {
                        println!("  {}: {} = {}", name.yellow(), ty.dimmed(), value.cyan());
                    }
                }
            }
            Command::Clear => {
                // Clear screen using ANSI escape codes
                print!("\x1B[2J\x1B[1;1H");
            }
            Command::History => {
                if let Ok(entries) = self.history.entries() {
                    println!("{}", "Command History:".bold());
                    for (i, entry) in entries.iter().enumerate() {
                        println!("  {}: {}", i + 1, entry);
                    }
                }
            }
        }
        Ok(false)
    }

    /// Print welcome message
    fn print_welcome(&self) {
        println!(
            "{} {}",
            "Jet".bold().cyan(),
            env!("CARGO_PKG_VERSION").to_string().dimmed()
        );
        println!(
            "Type {} for help, {} to exit",
            ":help".yellow(),
            ":quit".yellow()
        );
        println!();
    }

    /// Print goodbye message
    fn print_goodbye(&self) {
        println!("{}", "Goodbye!".dimmed());
    }

    /// Print help information
    fn print_help(&self) {
        println!("{}", "Jet REPL Commands:".bold());
        println!();
        println!("  {}       Show this help message", ":help".yellow());
        println!(
            "  {}        Show the type of an expression",
            ":type <expr>".yellow()
        );
        println!(
            "  {}       Show documentation for a symbol",
            ":doc <symbol>".yellow()
        );
        println!("  {}      Load a module from file", ":load <file>".yellow());
        println!("  {}         Reload the current module", ":reload".yellow());
        println!("  {}       List imported modules", ":imports".yellow());
        println!("  {}          List defined variables", ":vars".yellow());
        println!("  {}         Clear the screen", ":clear".yellow());
        println!("  {}       Show command history", ":history".yellow());
        println!("  {}         Exit the REPL", ":quit | :q".yellow());
        println!();
        println!(
            "Use {} to enter multi-line mode (ends with blank line)",
            "Ctrl+C".dimmed()
        );
        println!("Use {} to exit", "Ctrl+D".dimmed());
    }
}

impl Default for Repl {
    fn default() -> Self {
        Self::new().expect("Failed to create REPL")
    }
}

/// Run the REPL
pub fn run_repl() -> Result<()> {
    let mut repl = Repl::new()?;
    repl.run()
}

/// Run the REPL with an existing instance
pub fn run_repl_with(mut repl: Repl) -> Result<()> {
    repl.run()
}

impl Repl {
    /// Evaluate a code snippet directly
    pub fn evaluate(&mut self, code: &str) -> Result<evaluator::Value> {
        self.evaluator.evaluate(code)
    }

    /// Load a module from file
    pub fn load_module(&mut self, path: &str) -> Result<()> {
        self.evaluator.load_module(path)
    }
}
