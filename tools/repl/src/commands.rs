//! REPL commands
//!
//! This module handles parsing and execution of REPL commands.

/// A REPL command
#[derive(Debug, Clone, PartialEq)]
pub enum Command {
    /// Show help
    Help,
    /// Exit the REPL
    Quit,
    /// Show type of expression
    Type(String),
    /// Show documentation
    Doc(String),
    /// Load a module
    Load(String),
    /// Reload current module
    Reload,
    /// List imports
    Imports,
    /// List variables
    Vars,
    /// Clear screen
    Clear,
    /// Show history
    History,
}

/// Parser for REPL commands
pub struct CommandParser;

impl CommandParser {
    /// Parse a command from input
    pub fn parse(input: &str) -> Option<Command> {
        let input = input.trim();

        if !input.starts_with(':') {
            return None;
        }

        let parts: Vec<&str> = input.splitn(2, ' ').collect();
        let cmd = parts[0];
        let arg = parts.get(1).map(|s| s.trim().to_string());

        match cmd {
            ":help" | ":h" => Some(Command::Help),
            ":quit" | ":q" => Some(Command::Quit),
            ":type" | ":t" => arg.map(Command::Type).or_else(|| {
                eprintln!("Usage: :type <expression>");
                None
            }),
            ":doc" | ":d" => arg.map(Command::Doc).or_else(|| {
                eprintln!("Usage: :doc <symbol>");
                None
            }),
            ":load" | ":l" => arg.map(Command::Load).or_else(|| {
                eprintln!("Usage: :load <file>");
                None
            }),
            ":reload" | ":r" => Some(Command::Reload),
            ":imports" => Some(Command::Imports),
            ":vars" => Some(Command::Vars),
            ":clear" => Some(Command::Clear),
            ":history" => Some(Command::History),
            _ => {
                eprintln!(
                    "Unknown command: {}. Type :help for available commands.",
                    cmd
                );
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_help() {
        assert_eq!(CommandParser::parse(":help"), Some(Command::Help));
        assert_eq!(CommandParser::parse(":h"), Some(Command::Help));
    }

    #[test]
    fn test_parse_quit() {
        assert_eq!(CommandParser::parse(":quit"), Some(Command::Quit));
        assert_eq!(CommandParser::parse(":q"), Some(Command::Quit));
    }

    #[test]
    fn test_parse_type() {
        assert_eq!(
            CommandParser::parse(":type x + 1"),
            Some(Command::Type("x + 1".to_string()))
        );
        assert_eq!(
            CommandParser::parse(":t foo()"),
            Some(Command::Type("foo()".to_string()))
        );
    }

    #[test]
    fn test_parse_load() {
        assert_eq!(
            CommandParser::parse(":load myfile.jet"),
            Some(Command::Load("myfile.jet".to_string()))
        );
    }

    #[test]
    fn test_parse_reload() {
        assert_eq!(CommandParser::parse(":reload"), Some(Command::Reload));
        assert_eq!(CommandParser::parse(":r"), Some(Command::Reload));
    }

    #[test]
    fn test_parse_unknown() {
        assert!(CommandParser::parse(":unknown").is_none());
    }

    #[test]
    fn test_parse_not_command() {
        assert!(CommandParser::parse("not a command").is_none());
    }
}
