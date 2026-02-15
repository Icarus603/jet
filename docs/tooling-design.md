# Jet Tooling Ecosystem Design

This document specifies the complete tooling ecosystem for the Jet programming language, designed for production-ready AI-human collaboration.

---

## 1. CLI Tool: `jet`

The primary command-line interface for all Jet development tasks.

### 1.1 Subcommands

#### `jet new <project-name> [options]`
Creates a new Jet project with standard structure.

```
Options:
  --lib                 Create a library project (default: binary)
  --workspace           Create a workspace root for monorepos
  --path <dir>          Create project in specified directory
  --template <name>     Use a project template
  --vcs <git|none>      Initialize version control (default: git)

Examples:
  jet new my-app           # Create binary application
  jet new my-lib --lib     # Create library
  jet new my-workspace --workspace
```

**Generated structure (binary):**
```
my-app/
├── jet.toml              # Project manifest
├── jet.lock              # Dependency lock file
├── .gitignore
├── README.md
└── src/
    └── main.jet          # Entry point
```

**Generated structure (library):**
```
my-lib/
├── jet.toml
├── jet.lock
├── .gitignore
├── README.md
└── src/
    └── lib.jet           # Library root
```

#### `jet build [options]`
Compiles the project.

```
Options:
  --release               Build in release mode (optimized)
  --target <triple>       Cross-compile to target triple
  --jobs <n>              Parallel compilation jobs
  --features <list>       Enable feature flags (comma-separated)
  --all-features          Enable all features
  --no-default-features   Disable default features
  --out-dir <path>        Output directory
  --emit <ir|asm|obj>     Emit intermediate representation
  --verbose (-v)          Verbose output
  --quiet (-q)            Suppress output

Examples:
  jet build                    # Debug build
  jet build --release          # Optimized release build
  jet build --target wasm32    # Cross-compile to WASM
```

#### `jet run [options] [--] [args...]`
Builds and runs the project in one step.

```
Options:
  --release               Build/run in release mode
  --features <list>       Enable feature flags
  --target <triple>       Target triple
  --no-build              Skip build (run existing binary)
  --env <key=val>         Set environment variable

Examples:
  jet run
  jet run --release -- --arg1 --arg2
  jet run --features "async,logging"
```

#### `jet test [options] [filter]`
Runs the test suite.

```
Options:
  --release               Test release build
  --features <list>       Enable features for tests
  --lib                   Test library only
  --bin <name>            Test specific binary
  --test <name>           Run specific test file
  --doc                   Run doc tests
  --bench                 Run benchmarks
  --no-run                Compile but don't run
  --nocapture             Don't capture test output
  --test-threads <n>      Number of test threads
  --exact                 Match test name exactly
  --skip <name>           Skip tests matching pattern
  --include-ignored       Run ignored tests
  --coverage              Generate coverage report

Examples:
  jet test                      # Run all tests
  jet test parser               # Run tests matching "parser"
  jet test --coverage           # Generate coverage report
  jet test --bench              # Run benchmarks
```

#### `jet check [options]`
Fast type-checking without full compilation.

```
Options:
  --features <list>       Check with features enabled
  --all-features          Check with all features
  --lib                   Check library only
  --target <triple>       Check for target
  --message-format <fmt>  Error format: short|json|human

Examples:
  jet check               # Fast feedback loop
  jet check --all-features
```

#### `jet fmt [options] [files...]`
Formats Jet source code.

```
Options:
  --check                 Check formatting without modifying
  --emit <files|stdout>   Output mode
  --config-path <path>    Path to formatter config
  --edition <year>        Language edition to use

Examples:
  jet fmt                 # Format all files in project
  jet fmt src/main.jet    # Format specific file
  jet fmt --check         # CI check
```

#### `jet lint [options]`
Runs the linter on the codebase.

```
Options:
  --fix                   Automatically fix issues where possible
  --deny <lint>           Treat specific lint as error
  --warn <lint>           Treat specific lint as warning
  --allow <lint>          Allow specific lint
  --forbid <lint>         Forbid specific lint (unoverrideable)
  --level <level>         Set default lint level: allow|warn|deny
  --lib                   Lint library only
  --target <triple>       Lint for target

Examples:
  jet lint                # Run all lints
  jet lint --fix          # Fix auto-fixable issues
  jet lint --deny unused  # Fail on unused code
```

#### `jet doc [options]`
Generates and serves documentation.

```
Options:
  --open                  Open docs in browser after generation
  --no-deps               Don't generate dependency docs
  --document-private-items  Include private items
  --document-hidden-items   Include hidden items
  --output <path>         Output directory
  --serve                 Start local server
  --port <n>              Server port (default: 8080)

Examples:
  jet doc                 # Generate docs
  jet doc --open          # Generate and open
  jet doc --serve         # Serve docs locally
```

#### `jet publish [options]`
Publishes package to registry.

```
Options:
  --dry-run               Simulate publish without uploading
  --token <token>         Registry authentication token
  --registry <url>        Target registry
  --allow-dirty           Allow uncommitted changes
  --no-verify             Skip pre-publish verification

Examples:
  jet publish --dry-run   # Test publish
  jet publish             # Publish to registry
```

#### `jet clean`
Removes build artifacts.

```
Options:
  --doc                   Clean documentation
  --release               Clean release artifacts only

Examples:
  jet clean               # Remove target/ directory
```

#### `jet update [options]`
Updates dependencies.

```
Options:
  --package <name>        Update specific package
  --aggressive            Update even if semver-compatible
  --dry-run               Show what would be updated

Examples:
  jet update              # Update lock file
  jet update --package serde
```

#### `jet repl [options]`
Starts the interactive REPL.

```
Options:
  --edition <year>        Language edition
  --features <list>       Enable features

Examples:
  jet repl
```

### 1.2 Global Options

```
Options (available on all commands):
  -h, --help              Print help
  -V, --version           Print version
  --color <auto|always|never>  Control colored output
  --offline               Run without network access
  --verbose (-v)          Increase verbosity
  --quiet (-q)            Decrease verbosity
  -C <path>               Change to directory before execution
  --manifest-path <path>  Path to jet.toml
```

### 1.3 Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Compilation error |
| 3 | Test failure |
| 4 | Lint violation (with --deny) |
| 5 | Format check failed |
| 101 | Internal compiler error |

---

## 2. Package Manager: `jetpkg`

Integrated package management via `jet.toml` manifest.

### 2.1 Manifest Format (`jet.toml`)

```toml
# Required metadata
[package]
name = "my-project"
version = "1.0.0"
edition = "2024"
authors = ["Your Name <you@example.com>"]
description = "A brief description"
license = "MIT OR Apache-2.0"
repository = "https://github.com/user/repo"
homepage = "https://my-project.dev"
documentation = "https://docs.my-project.dev"
keywords = ["cli", "parser"]
categories = ["command-line-utilities"]
readme = "README.md"

# Build configuration
[build]
target-dir = "target"
profile = "release"
rustc-wrapper = "sccache"  # Optional compiler wrapper

# Feature flags
[features]
default = ["std", "logging"]
std = []
no-std = []
logging = ["log", "tracing"]
async = ["tokio", "futures"]
full = ["logging", "async"]

# Dependencies
[dependencies]
# Registry dependency (semver)
serde = "1.0"
serde = { version = "1.0", features = ["derive"] }
serde = { version = "1.0.200", optional = true }

# Exact version
regex = "=1.10.0"

# Version range
tokio = ">=1.35, <2.0"

# Git dependency
custom-lib = { git = "https://github.com/user/repo", branch = "main" }
custom-lib = { git = "https://github.com/user/repo", tag = "v1.0.0" }
custom-lib = { git = "https://github.com/user/repo", rev = "abc123" }

# Local path dependency
local-utils = { path = "../utils" }

# Platform-specific
[target.'cfg(unix)'.dependencies]
libc = "0.2"

[target.'cfg(windows)'.dependencies]
winapi = "0.3"

# Development dependencies
[dev-dependencies]
mockall = "0.12"
tempfile = "3.0"

# Build dependencies (for build scripts)
[build-dependencies]
cc = "1.0"

# Workspace configuration (for root jet.toml)
[workspace]
members = ["crates/*", "apps/*"]
resolver = "2"

[workspace.dependencies]
serde = "1.0.200"
tokio = "1.35.0"

# Package metadata for registry
[package.metadata]
# Custom metadata for tools
msrv = "1.75"  # Minimum supported Jet version

# Lint configuration
[package.metadata.lints]
unused = "deny"
dead_code = "warn"
```

### 2.2 Lock File (`jet.lock`)

Auto-generated, committed to version control for reproducible builds.

```toml
# This file is automatically generated by Jet.
# It is not intended for manual editing.
version = 3

[[package]]
name = "serde"
version = "1.0.200"
source = "registry+https://crates.jet-lang.org"
checksum = "sha256:abc123..."
dependencies = [
    "serde_derive",
]

[[package]]
name = "serde_derive"
version = "1.0.200"
source = "registry+https://crates.jet-lang.org"
checksum = "sha256:def456..."

[[package]]
name = "custom-lib"
version = "0.5.0"
source = "git+https://github.com/user/repo#abc123..."
```

### 2.3 Semantic Versioning

Jet follows SemVer 2.0 with Jet-specific additions:

```
version = "MAJOR.MINOR.PATCH[-PRE][+BUILD]"
```

- **MAJOR**: Breaking API changes
- **MINOR**: New features, backwards compatible
- **PATCH**: Bug fixes, backwards compatible
- **PRE**: Pre-release (alpha, beta, rc)
- **BUILD**: Build metadata (ignored for version comparison)

**Version requirement syntax:**

| Syntax | Meaning |
|--------|---------|
| `"1.2.3"` | Compatible with 1.2.3 (caret: ^1.2.3) |
| `"^1.2.3"` | Compatible with 1.2.3 (>=1.2.3, <2.0.0) |
| `"~1.2.3"` | Approximately 1.2.3 (>=1.2.3, <1.3.0) |
| `"=1.2.3"` | Exactly 1.2.3 |
| `">=1.2.3"` | At least 1.2.3 |
| `">1.2.3, <2.0.0"` | Range |
| `"*"` | Any version |

### 2.4 Registry Protocol

**Registry URL:** `https://crates.jet-lang.org`

**Index format:** Git repository with sparse index

```
# Index structure
se/
  rd/
    serde  # Contains version metadata
```

**API endpoints:**

```
GET  /api/v1/crates/{name}           # Crate metadata
GET  /api/v1/crates/{name}/{version} # Version metadata
GET  /api/v1/crates/{name}/{version}/download  # Download crate
PUT  /api/v1/crates/new              # Publish crate (auth required)
DELETE /api/v1/crates/{name}/{version}/yank    # Yank version
PUT  /api/v1/crates/{name}/{version}/unyank    # Unyank version
```

**Authentication:** API tokens via `~/.jet/credentials.toml`

```toml
[registry]
token = "jet_xxxxxxxxxxxxxxxx"
```

### 2.5 Workspace Support

Monorepo configuration with shared dependencies:

```toml
# Root jet.toml
[workspace]
name = "my-monorepo"
members = [
    "crates/core",
    "crates/parser",
    "apps/cli",
    "apps/server",
]
exclude = ["experimental", "target"]

[workspace.package]
version = "1.0.0"
edition = "2024"
authors = ["Team <team@example.com>"]
license = "MIT"
repository = "https://github.com/org/repo"

[workspace.dependencies]
# Shared versions
serde = { version = "1.0.200", features = ["derive"] }
tokio = { version = "1.35", features = ["full"] }
tracing = "0.1"

# Member crates inherit with:
# serde = { workspace = true }
```

```toml
# crates/core/jet.toml
[package]
name = "jet-core"
version.workspace = true
edition.workspace = true
authors.workspace = true
license.workspace = true
repository.workspace = true

[dependencies]
serde = { workspace = true }
```

---

## 3. IDE Support: `jet-lsp`

Language Server Protocol implementation for IDE integration.

### 3.1 Server Capabilities

```json
{
  "capabilities": {
    "textDocumentSync": { "openClose": true, "change": 2 },
    "completionProvider": {
      "resolveProvider": true,
      "triggerCharacters": [".", ":", "::"]
    },
    "hoverProvider": true,
    "signatureHelpProvider": {
      "triggerCharacters": ["(", ","]
    },
    "definitionProvider": true,
    "typeDefinitionProvider": true,
    "implementationProvider": true,
    "referencesProvider": true,
    "documentHighlightProvider": true,
    "documentSymbolProvider": true,
    "codeActionProvider": {
      "codeActionKinds": ["quickfix", "refactor", "source"]
    },
    "codeLensProvider": { "resolveProvider": false },
    "documentFormattingProvider": true,
    "documentRangeFormattingProvider": true,
    "documentOnTypeFormattingProvider": {
      "firstTriggerCharacter": ";",
      "moreTriggerCharacter": ["}", ","]
    },
    "renameProvider": { "prepareProvider": true },
    "foldingRangeProvider": true,
    "executeCommandProvider": {
      "commands": ["jet-lsp.runTests", "jet-lsp.showDocumentation"]
    },
    "selectionRangeProvider": true,
    "semanticTokensProvider": {
      "legend": {
        "tokenTypes": ["namespace", "type", "class", "enum", "interface", "struct", "typeParameter", "parameter", "variable", "property", "enumMember", "event", "function", "method", "macro", "keyword", "modifier", "comment", "string", "number", "regexp", "operator"],
        "tokenModifiers": ["declaration", "definition", "readonly", "static", "deprecated", "abstract", "async", "modification", "documentation", "defaultLibrary"]
      },
      "full": { "delta": true },
      "range": true
    },
    "inlayHintProvider": true,
    "workspaceSymbolProvider": true,
    "workspace": {
      "workspaceFolders": { "supported": true, "changeNotifications": true },
      "fileOperations": {
        "didCreate": { "filters": [{ "scheme": "file", "pattern": { "glob": "**/*.jet" } }] }
      }
    }
  }
}
```

### 3.2 Features

#### Autocomplete
- Context-aware suggestions
- Type information in labels
- Snippet completion for common patterns
- Import auto-insertion

#### Go to Definition
- Navigate to symbol definitions
- Support for cross-crate navigation
- Handle generated code

#### Find References
- Find all references to a symbol
- Distinguish reads from writes
- Show in preview panel

#### Hover Information
- Type signatures
- Documentation
- Module info
- Inferred types

#### Rename Refactoring
- Symbol renaming across workspace
- Conflict detection
- Preview changes

#### Code Actions
- Quick fixes for errors/warnings
- Auto-import missing modules
- Remove unused imports
- Generate implementations
- Extract variable/function

#### Inlay Hints
- Inferred types: `let x /*: i32 */ = 42`
- Parameter names: `foo(/*name:*/ "test")`
- Lifetime annotations
- Closure captures

### 3.3 Configuration

```jsonc
// VSCode settings.json
{
  "jet.lsp.enabled": true,
  "jet.lsp.path": "/usr/local/bin/jet-lsp",
  "jet.lsp.extraEnv": {
    "JET_LOG": "info"
  },
  "jet.checkOnSave": true,
  "jet.check.command": "check",
  "jet.check.features": "all",
  "jet.inlayHints.typeHints": true,
  "jet.inlayHints.parameterHints": true,
  "jet.inlayHints.chainingHints": true,
  "jet.inlayHints.lifetimeHints": false,
  "jet.completion.autoimport": true,
  "jet.completion.postfix": true,
  "jet.diagnostics.experimental": true,
  "jet.procMacro.enable": true,
  "jet.workspace.symbol.search.scope": "workspace",
  "jet.workspace.symbol.search.kind": "all"
}
```

### 3.4 Editor Extensions

**VSCode:** `jet-lang.vscode-jet`
- Full LSP support
- Debugging integration
- Snippets
- Tasks integration

**Vim/Neovim:** `jet-vim` / `nvim-jet`
- LSP via nvim-lspconfig
- Tree-sitter grammar
- FZF integration

**Emacs:** `jet-mode`
- LSP via lsp-mode
- Company completion
- Flycheck integration

**IntelliJ:** `intellij-jet`
- Native plugin (non-LSP)
- Advanced refactoring
- Graphical type display

---

## 4. Formatter: `jetfmt`

Opinionated, zero-configuration code formatter.

### 4.1 Philosophy

Like `gofmt`, `jetfmt` is not configurable. Consistency across all Jet codebases is valued over personal preference.

### 4.2 Formatting Rules

**Line Length:**
- Soft limit: 100 characters
- Hard limit: 120 characters
- Prefer breaking at 100, allow up to 120 when necessary

**Indentation:**
- 4 spaces (no tabs)
- Continuation indent: 4 spaces

**Braces:**
- Opening brace on same line (K&R style)
```jet
fn main() {
    if condition {
        statement;
    }
}
```

**Imports:**
- Grouped by: stdlib, external, local
- Sorted alphabetically within groups
- No wildcard imports (enforced by linter)
```jet
use std::collections::HashMap;
use std::io::{self, Read};

use serde::{Deserialize, Serialize};
use tokio::runtime::Runtime;

use crate::utils::helper;
```

**Spacing:**
- One space after keywords (if, while, for, etc.)
- No space before semicolons
- One space around operators
- No space after function name in call

**Line Breaks:**
- Max 1 consecutive blank line
- Trailing newline at EOF
- Unix line endings (LF)

**Comments:**
- Preserve doc comments (`///`)
- Preserve regular comments (`//`)
- Reflow doc comments to fit line length

### 4.3 CLI Interface

```
jet fmt [files...] [options]

Options:
  --check                 Check formatting, exit with error if changes needed
  --emit <files|stdout>   Output destination
  --backup                Create .jet.bak files
  --stdin                 Format stdin, output to stdout
  --config-path <path>    (Ignored - for compatibility only)

Examples:
  jet fmt src/             # Format directory
  jet fmt --check          # CI check
  cat file.jet | jet fmt --stdin
```

### 4.4 Integration

**Pre-commit hook:**
```bash
#!/bin/sh
jet fmt --check || exit 1
```

**CI check:**
```yaml
- name: Check formatting
  run: jet fmt --check
```

---

## 5. Linter: `jetlint`

Comprehensive static analysis for code quality.

### 5.1 Lint Categories

| Level | Description | Default |
|-------|-------------|---------|
| `error` | Code that will fail to compile or is clearly wrong | Always on |
| `warn` | Code that is likely incorrect or problematic | Default |
| `info` | Style suggestions and improvements | Off |
| `allow` | Allowed by default, opt-in | Off |

### 5.2 Built-in Lints

**Correctness (error):**
- `unreachable_code` - Code after return/break
- `unused_must_use` - Ignoring Result/Option return
- `invalid_doc_attributes` - Malformed doc comments
- `duplicate_match_arms` - Unreachable match arms

**Suspicious (warn):**
- `unused_variables` - Variables never read
- `unused_imports` - Unnecessary imports
- `dead_code` - Unreachable functions/modules
- `shadowing` - Variable shadowing
- `unused_mut` - Mutable when not needed
- `single_match` - Could use if-let
- `needless_borrow` - Unnecessary references

**Style (warn):**
- `non_snake_case` - Naming convention violations
- `non_camel_case_types` - Type naming
- `non_upper_case_globals` - Constant naming
- `missing_docs` - Public items without docs
- `trivial_casts` - Unnecessary type casts
- `trivial_numeric_casts` - Redundant numeric casts

**Performance (warn):**
- `inefficient_to_string` - Suboptimal string conversion
- `large_enum_variant` - Size disparity in enum variants
- `needless_allocation` - Heap allocation not needed
- `redundant_clone` - Unnecessary clone calls

**Complexity (allow):**
- `too_many_arguments` - Functions with many parameters
- `type_complexity` - Overly complex types
- `cognitive_complexity` - High cyclomatic complexity

**Pedantic (allow):**
- `must_use_candidate` - Functions returning important types
- `missing_errors_doc` - Panic conditions not documented
- `missing_panics_doc` - Panic conditions not documented

### 5.3 Lint Configuration

**In `jet.toml`:**
```toml
[lints]
unused = "deny"
dead_code = "warn"
shadowing = "allow"

[lints.clippy]  # Named lint groups
correctness = "deny"
suspicious = "warn"
style = "warn"
performance = "warn"
```

**In source code:**
```jet
// Allow specific lint for this file
#![allow(unused_variables)]

// Deny lint for next item
#[deny(missing_docs)]
pub fn important_function() {}

// Warn for a block
#[warn(clippy::shadowing)]
{
    let x = 1;
    let x = x + 1;  // triggers warning
}
```

### 5.4 Plugin Architecture

```rust
// Custom lint plugin
use jetlint::{Lint, LintContext, LintPass};

pub struct CustomLint;

impl Lint for CustomLint {
    fn name(&self) -> &'static str {
        "custom_lint"
    }

    fn level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn description(&self) -> &'static str {
        "Description of what this lint checks"
    }
}

impl LintPass for CustomLint {
    fn check_fn(&mut self, cx: &mut LintContext, fn_decl: &FnDecl) {
        // Lint logic here
    }
}
```

**Loading plugins:**
```toml
[package.metadata.lints]
plugins = ["jetlint-custom"]
```

### 5.5 CLI Interface

```
jet lint [options]

Options:
  --fix                   Auto-fix issues where possible
  --deny <lint>           Treat as error
  --warn <lint>           Treat as warning
  --allow <lint>          Allow
  --forbid <lint>         Forbid (unoverrideable)
  --level <level>         Default level
  --output-format <fmt>   human|json|short
  --target <triple>       Target-specific lints

Examples:
  jet lint
  jet lint --fix
  jet lint --deny warnings  # Treat all warnings as errors
```

---

## 6. REPL: `jet repl`

Interactive evaluation environment.

### 6.1 Features

- Expression evaluation
- Multi-line input
- Command history (persisted)
- Tab completion
- Type inspection
- Module loading

### 6.2 Commands

```
:type <expr>          Show inferred type of expression
:doc <symbol>         Show documentation
:load <file>          Load module from file
:reload               Reload current module
:imports              List imported modules
:vars                 List defined variables
:clear                Clear screen
:history              Show command history
:help                 Show help
:quit / :q / Ctrl+D   Exit REPL
```

### 6.3 Session Example

```
$ jet repl
Jet 1.0.0 (2026-02-15)
Type :help for help, :quit to exit

> let x = 42
x: i32 = 42

> :type x
i32

> let add = |a, b| a + b
add: fn(i32, i32) -> i32

> add(x, 8)
50

> :type add
fn(i32, i32) -> i32

> import std::collections::HashMap

> let mut map = HashMap::new()
map: HashMap<K, V>

> map.insert("key", "value")
None

> :doc HashMap
struct HashMap<K, V>
A hash map implementation with O(1) average-case lookups.
...

> :quit
$
```

### 6.4 Configuration

```toml
# ~/.jet/repl.toml
[repl]
history_file = "~/.jet/repl_history"
history_size = 1000
editor = "emacs"  # or "vi"
colors = true
prompt = "> "
multiline_prompt = "... "

[repl.imports]
auto_import = ["std::prelude::*"]
```

---

## 7. Documentation Generator

Generates beautiful, searchable documentation from source code.

### 7.1 Doc Comment Format

```jet
/// Brief description of the function.
///
/// Longer description with **markdown** support.
///
/// # Examples
///
/// ```
/// let result = add(2, 3);
/// assert_eq!(result, 5);
/// ```
///
/// # Errors
///
/// Returns `Err` if...
///
/// # Panics
///
/// Panics if...
///
/// # Safety
///
/// Unsafe because...
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

//! Module-level documentation (crate root)

/// Documented struct
#[derive(Debug)]
pub struct MyStruct {
    /// Field documentation
    pub field: i32,
}
```

### 7.2 Generated Output

**Features:**
- Static HTML generation
- Full-text search (client-side)
- Cross-linking between types
- Source code links
- Syntax highlighting
- Mobile-responsive design
- Dark/light theme
- Copy code buttons

**Structure:**
```
target/doc/
├── index.html              # Crate index
├── all.html                # All items
├── search-index.js         # Search data
├── src/                    # Source files
├── crate-name/
│   ├── index.html          # Module index
│   ├── struct.MyStruct.html
│   └── trait.MyTrait.html
└── static/
    ├── style.css
    ├── search.js
    └── highlight.js
```

### 7.3 Doc Tests

Code examples in doc comments are tested:

```
jet test --doc
```

**Attributes:**
```jet
/// ```ignore
/// This code is not tested
/// ```

/// ```no_run
/// This code compiles but doesn't run
/// ```

/// ```should_panic
/// This code should panic
/// ```

/// ```edition2024
/// Use specific edition
/// ```
```

### 7.4 CLI Interface

```
jet doc [options]

Options:
  --open                  Open in browser
  --no-deps               Don't document dependencies
  --document-private-items
  --document-hidden-items
  --output <path>
  --serve                 Start local server
  --port <n>
  --watch                 Rebuild on changes

Examples:
  jet doc --open
  jet doc --serve --port 3000
```

---

## 8. Debugger Support

Full debugging support via DWARF debug information.

### 8.1 Debug Information

**Generated by default in debug builds:**
```toml
[profile.dev]
debug = true          # Include debug info
debug-assertions = true
overflow-checks = true
```

**Control levels:**
- `0` - No debug info
- `1` - Line tables only
- `2` - Full debug info
- `3` - Full + macro expansions

### 8.2 GDB Integration

```bash
# Compile with debug info
jet build

# Debug with GDB
gdb target/debug/my-app

# Or use jet wrapper
jet gdb -- run arg1 arg2
```

**GDB pretty printers:**
```python
# ~/.jet/gdb_pretty_printers.py
class JetStringPrinter:
    def __init__(self, val):
        self.val = val
    def to_string(self):
        return self.val["ptr"].string()
```

### 8.3 LLDB Integration

```bash
# Debug with LLDB
lldb target/debug/my-app

# Or use jet wrapper
jet lldb -- run arg1 arg2
```

### 8.4 VSCode Debug Adapter

**launch.json configuration:**
```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "jet",
            "request": "launch",
            "name": "Debug",
            "program": "${workspaceFolder}/target/debug/${workspaceFolderBasename}",
            "args": [],
            "cwd": "${workspaceFolder}",
            "env": {},
            "preLaunchTask": "jet build"
        },
        {
            "type": "jet",
            "request": "attach",
            "name": "Attach to Process",
            "pid": "${command:pickProcess}"
        },
        {
            "type": "jet",
            "request": "test",
            "name": "Debug Tests",
            "testFilter": "${input:testFilter}"
        }
    ]
}
```

### 8.5 Debugging Features

- Breakpoints (line, conditional, function)
- Watchpoints
- Variable inspection
- Call stack navigation
- Expression evaluation
- Memory inspection
- Disassembly view

---

## 9. Build System

Flexible build configuration via `jet.toml`.

### 9.1 Build Profiles

```toml
[profile.dev]
opt-level = 0
debug = true
lto = false
codegen-units = 256
panic = "unwind"
incremental = true

[profile.release]
opt-level = 3
debug = false
lto = true
codegen-units = 1
panic = "abort"
strip = true

[profile.test]
inherits = "dev"
opt-level = 1

[profile.bench]
inherits = "release"
debug = true
```

**Profile options:**
| Option | Description |
|--------|-------------|
| `opt-level` | Optimization level (0-3, s, z) |
| `debug` | Debug info level |
| `lto` | Link-time optimization |
| `codegen-units` | Parallel codegen units |
| `panic` | Panic strategy (unwind/abort) |
| `strip` | Strip symbols |
| `incremental` | Incremental compilation |

### 9.2 Conditional Compilation

```toml
[features]
default = ["std"]
std = []
no-std = []
logging = ["log"]
```

```jet
#[cfg(feature = "std")]
use std::collections::HashMap;

#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap;

#[cfg(all(unix, feature = "std"))]
pub fn unix_specific() {}

#[cfg(any(target_os = "linux", target_os = "macos"))]
pub fn posix() {}

#[cfg(target_arch = "wasm32")]
pub fn wasm_only() {}
```

### 9.3 Build Scripts

For C dependencies or code generation:

```toml
[package]
name = "my-crate"
build = "build.jet"
```

```jet
// build.jet
use std::process::Command;

fn main() {
    // Re-run if these files change
    println!("cargo:rerun-if-changed=src/native.c");
    println!("cargo:rerun-if-changed=include/native.h");

    // Compile C code
    cc::Build::new()
        .file("src/native.c")
        .compile("native");

    // Link library
    println!("cargo:rustc-link-lib=static=native");

    // Set cfg flags
    println!("cargo:rustc-cfg=has_native");

    // Environment variables from dependencies
    let dep_path = env::var("DEP_SOME_DEP_INCLUDE").unwrap();
}
```

### 9.4 Target Configuration

```toml
[target.x86_64-unknown-linux-gnu]
linker = "clang"
rustflags = ["-C", "target-cpu=sandybridge"]

[target.wasm32-unknown-unknown]
runner = "wasm-runner"

[target.'cfg(unix)']
runner = "unix-runner"
```

---

## 10. Testing Framework

Built-in, comprehensive testing support.

### 10.1 Test Types

**Unit tests (in source files):**
```jet
// src/lib.jet

pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(2, 3), 5);
    }

    #[test]
    #[should_panic(expected = "overflow")]
    fn test_add_overflow() {
        add(i32::MAX, 1);
    }

    #[test]
    #[ignore = "not implemented yet"]
    fn test_future_feature() {}
}
```

**Integration tests (`tests/` directory):**
```
tests/
├── test_basic.jet       # Each file is a separate test crate
├── test_advanced.jet
└── common/
    └── mod.jet          # Shared test utilities
```

```jet
// tests/test_basic.jet
use my_crate::add;

#[test]
fn test_external() {
    assert_eq!(add(1, 1), 2);
}
```

**Doc tests (in doc comments):**
```jet
/// ```
/// assert_eq!(my_crate::add(2, 2), 4);
/// ```
pub fn add(a: i32, b: i32) -> i32 { ... }
```

### 10.2 Assertion Macros

```jet
assert!(condition);
assert!(condition, "message with {}", arg);

assert_eq!(left, right);
assert_eq!(left, right, "custom message");

assert_ne!(left, right);

assert_matches!(value, Pattern);

assert_throws!(|| { panic!("error") }, "error");
```

### 10.3 Test Fixtures

```jet
use std::test::TempDir;

#[test]
fn test_with_temp() {
    let temp = TempDir::new().unwrap();
    let path = temp.path().join("test.txt");
    // temp is automatically cleaned up
}
```

### 10.4 Benchmarks

```jet
// benches/my_bench.jet
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use my_crate::fibonacci;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| {
        b.iter(|| fibonacci(black_box(20)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
```

### 10.5 Code Coverage

```bash
# Generate coverage report
jet test --coverage

# Output formats
jet test --coverage --coverage-format html   # HTML report
jet test --coverage --coverage-format lcov   # LCOV for external tools
jet test --coverage --coverage-format json   # JSON for CI
```

**Coverage configuration:**
```toml
[coverage]
output_dir = "target/coverage"
ignore = ["tests/*", "benches/*"]
branch_coverage = true
```

### 10.6 Test Organization

```
my-project/
├── src/
│   └── lib.jet          # Unit tests in #[cfg(test)]
├── tests/
│   ├── integration_test.jet
│   └── common/
│       └── helpers.jet
├── benches/
│   └── performance.jet
└── examples/
    └── basic.jet        # Also tested by `jet test --examples`
```

---

## 11. CI/CD Integration

First-class support for continuous integration.

### 11.1 GitHub Actions

**Official action:** `jet-lang/setup-jet`

```yaml
# .github/workflows/ci.yml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        jet: [stable, nightly]

    steps:
      - uses: actions/checkout@v4

      - name: Setup Jet
        uses: jet-lang/setup-jet@v1
        with:
          jet-version: ${{ matrix.jet }}
          components: jetfmt, jetlint

      - name: Cache dependencies
        uses: actions/cache@v3
        with:
          path: |
            ~/.jet/registry
            target/
          key: ${{ runner.os }}-jet-${{ hashFiles('jet.lock') }}

      - name: Check formatting
        run: jet fmt --check

      - name: Run linter
        run: jet lint --deny warnings

      - name: Type check
        run: jet check

      - name: Build
        run: jet build --release

      - name: Run tests
        run: jet test --coverage

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: target/coverage/lcov.info
```

### 11.2 Docker

**Official images:**
```dockerfile
# Dockerfile
FROM jetlang/jet:1.0 as builder

WORKDIR /app
COPY jet.toml jet.lock ./
COPY src ./src

RUN jet build --release

FROM debian:bookworm-slim
COPY --from=builder /app/target/release/my-app /usr/local/bin/
CMD ["my-app"]
```

**Image variants:**
- `jetlang/jet:latest` - Latest stable
- `jetlang/jet:1.0` - Specific version
- `jetlang/jet:nightly` - Nightly builds
- `jetlang/jet:1.0-slim` - Minimal image
- `jetlang/jet:1.0-alpine` - Alpine-based

### 11.3 Cross-Compilation

```bash
# Install cross-compilation targets
jet target add x86_64-pc-windows-gnu
jet target add aarch64-unknown-linux-gnu
jet target add wasm32-unknown-unknown

# Cross-compile
jet build --target x86_64-pc-windows-gnu

# Using cross (containerized)
cross build --target aarch64-unknown-linux-gnu
```

**Cross configuration (`Cross.toml`):**
```toml
[target.aarch64-unknown-linux-gnu]
image = "jetlang/cross:aarch64-linux-gnu"
pre-build = ["apt-get update && apt-get install -y libssl-dev"]

[target.x86_64-pc-windows-gnu]
linker = "x86_64-w64-mingw32-gcc"
```

### 11.4 Release Automation

```yaml
# .github/workflows/release.yml
name: Release

on:
  push:
    tags:
      - 'v*'

jobs:
  release:
    strategy:
      matrix:
        include:
          - target: x86_64-unknown-linux-gnu
            os: ubuntu-latest
          - target: x86_64-apple-darwin
            os: macos-latest
          - target: x86_64-pc-windows-msvc
            os: windows-latest

    runs-on: ${{ matrix.os }}

    steps:
      - uses: actions/checkout@v4

      - uses: jet-lang/setup-jet@v1

      - name: Build release
        run: jet build --release --target ${{ matrix.target }}

      - name: Package
        shell: bash
        run: |
          cd target/${{ matrix.target }}/release
          if [[ "${{ matrix.os }}" == "windows-latest" ]]; then
            7z a ../../../release.zip my-app.exe
          else
            tar czvf ../../../release.tar.gz my-app
          fi

      - name: Upload to release
        uses: softprops/action-gh-release@v1
        with:
          files: release.*
```

---

## 12. Compiler Integration Points

All tools integrate with the Jet compiler through defined interfaces.

### 12.1 Compiler Interface

```rust
// Compiler API for tool integration
pub struct Compiler {
    session: Session,
    options: CompilerOptions,
}

impl Compiler {
    pub fn new(options: CompilerOptions) -> Self;

    pub fn parse(&self, source: &str) -> Result<AST, ParseError>;

    pub fn type_check(&self, ast: &AST) -> Result<TypedAST, TypeError>;

    pub fn compile(&self, ast: &TypedAST) -> Result<IR, CompileError>;

    pub fn emit(&self, ir: &IR, target: Target) -> Result<Artifact, EmitError>;
}
```

### 12.2 Tool-Specific Integration

**Formatter:**
- Operates on AST before type checking
- Preserves comments and whitespace metadata
- Uses `Compiler::parse()` only

**Linter:**
- Uses `Compiler::type_check()` for type-aware lints
- Additional AST passes for style lints
- Access to symbol resolution

**LSP:**
- Maintains incremental compilation state
- Uses compiler's incremental parsing
- Shares type information across operations

**Documentation:**
- Uses `Compiler::parse()` to extract doc comments
- Cross-references symbols using compiler's name resolution

### 12.3 Message Format

**Structured output for tools:**
```json
{
    "message": "type mismatch",
    "level": "error",
    "code": "E0308",
    "spans": [
        {
            "file_name": "src/main.jet",
            "byte_start": 42,
            "byte_end": 45,
            "line_start": 10,
            "line_end": 10,
            "column_start": 8,
            "column_end": 11,
            "label": "expected `i32`, found `String`"
        }
    ],
    "children": [
        {
            "message": "expected because of this",
            "level": "note",
            "spans": [...]
        }
    ]
}
```

---

## 13. Configuration Summary

| File | Purpose |
|------|---------|
| `jet.toml` | Project manifest, dependencies, build config |
| `jet.lock` | Locked dependency versions |
| `.jet/config.toml` | User-global Jet configuration |
| `.jet/credentials.toml` | Registry authentication |
| `build.jet` | Build script for C deps/codegen |
| `Cross.toml` | Cross-compilation configuration |
| `.jetignore` | Files to ignore in Jet operations |

---

## 14. Tool Comparison Matrix

| Task | Command | Speed | Caching |
|------|---------|-------|---------|
| Type check | `jet check` | Fast | Yes |
| Build (debug) | `jet build` | Medium | Yes |
| Build (release) | `jet build --release` | Slow | Yes |
| Test | `jet test` | Medium | Yes |
| Format | `jet fmt` | Fast | No |
| Lint | `jet lint` | Medium | Yes |
| Doc | `jet doc` | Slow | Yes |

---

## Appendix A: File Extensions

| Extension | Purpose |
|-----------|---------|
| `.jet` | Jet source file |
| `.jeti` | Jet interface file (public API) |
| `.jetm` | Jet module file (compiled metadata) |

## Appendix B: Environment Variables

| Variable | Purpose |
|----------|---------|
| `JET_HOME` | Jet installation directory |
| `JET_TARGET_DIR` | Default build output directory |
| `JET_REGISTRY` | Default registry URL |
| `JET_OFFLINE` | Disable network access |
| `JET_LOG` | Logging level (error/warn/info/debug/trace) |
| `JET_BACKTRACE` | Enable backtraces (1/full) |
