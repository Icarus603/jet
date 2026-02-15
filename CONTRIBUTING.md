# Contributing to Jet

Thank you for your interest in contributing to Jet! This guide will help you get started.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Development Setup](#development-setup)
- [Project Structure](#project-structure)
- [Building and Testing](#building-and-testing)
- [Coding Standards](#coding-standards)
- [Pull Request Process](#pull-request-process)
- [Debugging Tips](#debugging-tips)
- [Performance Profiling](#performance-profiling)

---

## Code of Conduct

This project adheres to a code of conduct that we expect all contributors to follow:

- Be respectful and inclusive
- Welcome newcomers and help them learn
- Focus on constructive feedback
- Accept responsibility and apologize when mistakes happen

## Development Setup

### Prerequisites

- **Rust** 1.70 or later: [Install Rust](https://rustup.rs/)
- **LLVM** 21.1.x: Required for code generation
- **Git**: For version control

### Toolchain Support Policy

- Jet supports LLVM **21.1.x** only.
- Compatibility with LLVM 17 and LLVM 21.2+ is not guaranteed.
- LLVM patch upgrades are tracked via automated issue reminders and merged only after full validation.
- Supported host OS for development and CI: macOS and Linux only (Windows is unsupported).
- Distribution channels: macOS via Homebrew tap (`Icarus603/jet`), Linux via release archives + `scripts/install.sh`.

### macOS Setup

```bash
# Install LLVM 21
brew install llvm@21

# Set environment variable (add to ~/.zshrc)
export LLVM_SYS_211_PREFIX=/opt/homebrew/opt/llvm@21

# Clone the repository
git clone https://github.com/Icarus603/jet
cd jet

# Build the project
cargo build --workspace
```

### Linux Setup (Ubuntu/Debian)

```bash
# Install LLVM and dependencies
sudo apt-get update
sudo apt-get install -y llvm-21-dev libclang-21-dev cmake

# Set environment variable
export LLVM_SYS_211_PREFIX=/usr/lib/llvm-21

# Clone and build
git clone https://github.com/Icarus603/jet
cd jet
cargo build --workspace
```

### IDE Setup

We recommend using VS Code with the following extensions:

- **rust-analyzer**: Rust language support
- **CodeLLDB**: Debugging support
- **Even Better TOML**: TOML file support

Recommended settings for `.vscode/settings.json`:

```json
{
  "rust-analyzer.cargo.features": "all",
  "rust-analyzer.check.command": "clippy",
  "rust-analyzer.check.extraArgs": ["--all-targets"],
  "editor.formatOnSave": true
}
```

---

## Project Structure

```
jet/
├── compiler/          # Compiler implementation
│   ├── lexer/        # Tokenization
│   ├── parser/       # AST construction
│   ├── resolve/      # Name resolution
│   ├── typeck/       # Type inference
│   ├── effect/       # Effect checking
│   ├── diagnostics/  # Error reporting
│   ├── ir/           # Intermediate representation
│   ├── lower/        # AST to IR lowering
│   ├── opt/          # Optimizations
│   ├── codegen/      # LLVM code generation
│   └── linker/       # Binary linking
├── runtime/           # Runtime system
│   ├── gc/           # Garbage collector
│   ├── sched/        # M:N task scheduler
│   ├── sync/         # Synchronization primitives
│   └── ffi/          # C interoperability
├── stdlib/            # Standard library (Jet code)
├── tools/             # Developer tools
│   ├── cli/          # jet command
│   ├── lsp/          # Language server
│   └── fmt/          # Code formatter
├── examples/          # Example programs
├── docs/              # Documentation
└── tests/             # Integration tests
```

### Key Dependencies

| Component | Purpose |
|-----------|---------|
| `logos` | Lexer generator |
| `rowan` | Syntax tree library |
| `ena` | Union-find for type unification |
| `inkwell` | LLVM bindings |
| `crossbeam` | Lock-free data structures |

---

## Building and Testing

### Build Commands

```bash
# Build entire workspace
cargo build --workspace

# Build release version
cargo build --workspace --release

# Build specific crate
cargo build -p jet-lexer
cargo build -p jet-parser
```

### Running Tests

```bash
# Run all tests
cargo test --workspace

# Run tests for specific crate
cargo test -p jet-lexer
cargo test -p jet-parser

# Run specific test
cargo test -p jet-parser test_if_simple

# Run with output
cargo test -- --nocapture

# Run integration tests
cargo test -p jet-integration-tests
```

### Code Quality

```bash
# Run clippy (linting)
cargo clippy --workspace

# Fix clippy warnings automatically
cargo clippy --fix --workspace --allow-dirty

# Check formatting
cargo fmt --check

# Format code
cargo fmt

# Run all checks (CI does this)
cargo check --workspace && \
cargo clippy --workspace && \
cargo test --workspace && \
cargo fmt --check
```

---

## Coding Standards

### Rust Code Style

We follow the standard Rust style guidelines:

- Use `rustfmt` for formatting
- Address all `clippy` warnings
- Write documentation comments (`///`) for public APIs
- Use meaningful variable names

Example:

```rust
/// Parses a function declaration from the token stream.
///
/// # Example
/// ```
/// fn foo() -> int { 42 }
/// ```
pub fn parse_function(&mut self) -> ParseResult<Function> {
    let name = self.expect_identifier()?;
    let params = self.parse_parameters()?;
    let return_type = self.parse_return_type()?;
    let body = self.parse_block()?;

    Ok(Function {
        name,
        params,
        return_type,
        body,
    })
}
```

### Jet Code Style

For Jet code in examples and stdlib:

- Use 4 spaces for indentation
- snake_case for variables and functions
- PascalCase for types and traits
- UPPER_SNAKE_CASE for constants
- One space after `:` in type annotations

Example:

```jet
const MAX_RETRIES = 3

struct HttpClient:
    base_url: string
    timeout: int

impl HttpClient:
    fn new(base_url: string) -> HttpClient:
        return HttpClient {
            base_url: base_url,
            timeout: 30
        }

    async fn get(self, path: string) -> Result[Response, Error]:
        let url = f"{self.base_url}{path}"
        return await fetch(url)
```

### Testing Guidelines

- Write unit tests in the same file as the code (in `#[cfg(test)]` modules)
- Write integration tests in `tests/` directories
- Use descriptive test names
- Test both success and error cases
- Use `pretty_assertions` for better diff output

Example:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_positive_numbers() {
        assert_eq!(add(2, 3), 5);
    }

    #[test]
    fn test_add_negative_numbers() {
        assert_eq!(add(-2, -3), -5);
    }

    #[test]
    fn test_add_mixed_signs() {
        assert_eq!(add(-2, 3), 1);
    }
}
```

### Documentation

- All public APIs must have doc comments
- Include examples in documentation
- Keep README files up to date
- Update CHANGELOG.md for user-facing changes

---

## Pull Request Process

1. **Fork and Branch**
   ```bash
   git checkout -b feature/my-feature
   # or
   git checkout -b fix/issue-description
   ```

2. **Make Changes**
   - Write code following our standards
   - Add tests for new functionality
   - Update documentation

3. **Test Locally**
   ```bash
   cargo test --workspace
   cargo clippy --workspace
   cargo fmt --check
   ```

4. **Commit**
   - Use clear, descriptive commit messages
   - Reference issues with `#123`
   - One logical change per commit

   Format:
   ```
   component: Brief description

   Longer explanation if needed.

   Fixes #123
   ```

5. **Push and Create PR**
   ```bash
   git push origin feature/my-feature
   ```

   Then create a PR on GitHub with:
   - Clear title and description
   - Link to related issues
   - Screenshots if UI changes
   - Test results

6. **Code Review**
   - Address review feedback
   - Keep discussions constructive
   - Re-request review when ready

7. **Merge**
   - Squash merge if requested
   - Delete branch after merge

---

## Debugging Tips

### Compiler Debugging

Enable debug output:

```bash
# Set environment variables
export JET_DEBUG=1
export JET_DEBUG_PARSER=1
export JET_DEBUG_TYPECK=1

# Run with backtrace
RUST_BACKTRACE=1 cargo run -- run myfile.jet
```

### Debugging Test Failures

```bash
# Run specific test with output
cargo test test_name -- --nocapture

# Run with debugger
rust-gdb target/debug/deps/jet_parser-xxxxx
```

### AST Debugging

Use the `debug_assert!` macro for assertions that only run in debug builds:

```rust
debug_assert!(!tokens.is_empty(), "Token stream should not be empty");
```

### Common Issues

**"LLVM not found"**
```bash
# Verify LLVM installation
llvm-config --version

# Set correct environment
export LLVM_SYS_211_PREFIX=/path/to/llvm
```

**"Parser stack overflow"**
- Check for infinite recursion in grammar rules
- Increase stack size: `RUST_MIN_STACK=8388608 cargo test`

**"Test timeouts"**
- Check for infinite loops in test cases
- Use `#[timeout = "30s"]` attribute for slow tests

---

## Performance Profiling

### Compile Time Profiling

```bash
# Use cargo's built-in timing
cargo build --workspace --timings

# View report in target/cargo-timings/
```

### Runtime Profiling

```bash
# Build with debug symbols
cargo build --release

# Run with perf (Linux)
perf record -g ./target/release/jet run benchmark.jet
perf report

# Run with Instruments (macOS)
# Use Xcode Instruments with Time Profiler template
```

### Memory Profiling

```bash
# Use valgrind (Linux)
valgrind --tool=massif ./target/release/jet run program.jet

# Use heaptrack (Linux)
heaptrack ./target/release/jet run program.jet
```

### Benchmarking

```bash
# Run benchmarks
cargo bench

# Compare benchmarks
cargo bench -- --baseline main
```

---

## Issue Reporting

### Bug Reports

Include:
- Jet version (`jet --version`)
- Operating system
- Minimal reproduction case
- Expected vs actual behavior
- Error messages
- Backtrace if applicable

### Feature Requests

Include:
- Use case description
- Proposed syntax (if applicable)
- Examples of how it would be used
- Potential alternatives considered

---

## Communication Channels

- **GitHub Issues**: Bug reports and feature requests
- **GitHub Discussions**: General questions and ideas
- **Discord**: Real-time chat (link in README)
- **Mailing List**: Announcements and deep discussions

---

## License

By contributing to Jet, you agree that your contributions will be licensed under the MIT OR Apache-2.0 dual license.

---

Thank you for contributing to Jet! 🚀
