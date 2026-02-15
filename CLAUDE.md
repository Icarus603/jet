# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Jet is a systems-oriented programming language designed for readability, safety, and AI-assisted development. It features:
- Python-like syntax with significant indentation
- Rust-level type safety with Hindley-Milner type inference
- Algebraic effect system for structured error handling
- Compiled via LLVM backend (using inkwell crate)
- Custom runtime with Immix GC and M:N threading

## Essential Commands

### Build
```bash
# Build entire workspace (release mode recommended for performance)
cargo build --workspace --release

# Build specific compiler component
cargo build -p jet-lexer
cargo build -p jet-parser
cargo build -p jet-codegen
```

### Test
```bash
# Run all tests
cargo test --workspace

# Run specific test suite
cargo test -p jet-lexer
cargo test -p jet-typeck
cargo test -p jet-integration-tests --test test_compiler_pipeline

# Run single test
cargo test --test test_compiler_pipeline -- specific_test_name
```

### Quality Checks
```bash
# Format check (required for CI)
cargo fmt --all -- --check

# Apply formatting
cargo fmt --all

# Lint with clippy (deny warnings in CI)
cargo clippy --workspace --all-targets -- -D warnings

# Verify safety documentation
bash scripts/check_safety_docs.sh
```

### Production Readiness Verification
```bash
# Run baseline snapshot commands (see PRODUCTION_READINESS.md)
cargo fmt --all -- --check
cargo clippy --workspace --all-targets -- -D warnings
cargo test --workspace
cargo test -p jet-integration-tests --test test_compiler_pipeline
bash scripts/check_safety_docs.sh
```

## Compiler Pipeline Architecture

The Jet compiler is organized into distinct phases, each in its own crate. Understanding this pipeline is critical for making changes:

```
Source (.jet) → Lexer → Parser → Resolve → TypeChecker → Effect → IR → Opt → Codegen → LLVM
```

### Phase Breakdown

| Phase | Crate | Input | Output | Key Responsibility |
|-------|-------|-------|--------|-------------------|
| **Lexer** | `compiler/lexer` | Source text | `TokenStream` | Indentation-aware tokenization (INDENT/DEDENT tokens) |
| **Parser** | `compiler/parser` | `TokenStream` | `ast::Module` | Recursive descent parser with Pratt parsing for expressions |
| **Resolve** | `compiler/resolve` | `ast::Module` | `ast::Module` + `SymbolTable` | Name resolution, builds symbol tables |
| **TypeCheck** | `compiler/typeck` | AST + symbols | `tast::Module` | Hindley-Milner type inference, unification |
| **Effect** | `compiler/effect` | `tast::Module` | `tast::Module` (with effects) | Effect system checking and propagation |
| **IR** | `compiler/ir` | `tast::Module` | `ir::Module` | Lower to SSA-based intermediate representation |
| **Opt** | `compiler/opt` | `ir::Module` | `ir::Module` | IR-level optimizations (DCE, constant folding, etc.) |
| **Codegen** | `compiler/codegen` | `ir::Module` | LLVM IR | LLVM code generation via inkwell |
| **Linker** | `compiler/linker` | LLVM IR | Binary/object | Final linking and artifact generation |

**Error Recovery**: Each phase returns `Result<T, Vec<Diagnostic>>` and attempts to continue compilation past errors to report multiple issues. The driver collects all diagnostics.

**Diagnostics**: Unified error type in `compiler/diagnostics` with source spans, labels, error codes (e.g., E0308 for type mismatches).

## Workspace Structure

```
jet/
├── compiler/           # Frontend and middle-end (13 crates)
│   ├── lexer/         # Tokenization with indentation handling
│   ├── parser/        # AST construction
│   ├── resolve/       # Name resolution
│   ├── typeck/        # Type inference and checking
│   ├── effect/        # Effect system
│   ├── ir/            # Intermediate representation
│   ├── lower/         # AST → IR lowering
│   ├── opt/           # IR optimizations
│   ├── codegen/       # LLVM code generation
│   ├── linker/        # Linking and artifact generation
│   └── diagnostics/   # Error reporting infrastructure
│
├── runtime/           # Runtime system (4 crates)
│   ├── gc/            # Immix garbage collector
│   ├── sched/         # M:N scheduler with work-stealing
│   ├── sync/          # Synchronization primitives
│   └── ffi/           # Foreign function interface
│
├── stdlib/            # Standard library
│
├── tools/             # Tooling (4 crates)
│   ├── cli/           # Main `jet` command (build/run/test/etc.)
│   ├── lsp/           # Language Server Protocol
│   ├── fmt/           # Code formatter
│   └── jetpkg/        # Package manager
│
├── tests/             # Integration tests
│
└── docs/              # Design docs and specifications
```

## Key Design Principles

1. **Modularity**: Each compiler phase is independently testable with clear interfaces
2. **Error Recovery**: Continue past errors to report multiple diagnostics
3. **Indentation-Aware Lexing**: Lexer emits INDENT/DEDENT tokens (parser doesn't track indentation)
4. **Type Inference**: Hindley-Milner with let-generalization (no explicit type annotations required for most code)
5. **Effect System**: Structured effects with handlers (not exceptions)
6. **RAII + GC**: Deterministic destructors with automatic memory management

## Testing Strategy

- **Unit Tests**: Per-crate in `tests/` subdirectories (e.g., `compiler/lexer/tests/`)
- **Golden File Tests**: Expected diagnostics in `tests/diagnostics/` (`.jet` + `.stderr` pairs)
- **Integration Tests**: End-to-end compiler pipeline in `tests/tests/`
- **Property-Based Testing**: Using `proptest` for invariant checking
- **Examples**: Executable examples in `examples/` (tested via CI)

## Safety and FFI

- **Safety Documentation**: All `unsafe` code must have `# Safety` contract documentation
- Verified by: `bash scripts/check_safety_docs.sh` (CI enforced)
- **FFI Boundary**: Marshaling rules and ownership tracking in `runtime/ffi`
- **Borrow Checking**: Dynamic borrow tracking for FFI references

## LLVM Integration

- **Version**: LLVM 21.x (specified in workspace dependencies)
- **Crate**: `inkwell` v0.8 with `llvm21-1` feature
- **Platform Support**: Linux, macOS, Windows (CI validates all three)
- Windows requires LLVM installation (verified in CI/release workflows)

## Type System Specifics

- **Type Variables**: Represented as `TypeId` with unification via `ena` crate
- **Effect Sets**: Tracked per function, propagated through call graph
- **Trait Objects**: Fat pointers (data + vtable)
- **Inline Caches**: Monomorphic call site optimization for dynamic dispatch

## Common Development Patterns

When adding a new language feature:
1. Extend lexer tokens (if new syntax)
2. Update parser to recognize construct
3. Add AST node type
4. Implement name resolution rules
5. Add type checking logic
6. Implement IR lowering
7. Add codegen support
8. Write tests (unit + integration + golden files)

When debugging type errors:
- Check `compiler/typeck/` for unification logic
- Look at diagnostic generation in `compiler/diagnostics/`
- Integration test with golden `.stderr` file to lock expected behavior

## Documentation

- **User Guide**: `docs/user-guide.md`
- **Language Spec**: `docs/language-spec.md`
- **Compiler Architecture**: `docs/compiler-architecture.md` (detailed phase descriptions)
- **Runtime Design**: `docs/runtime-design.md` (GC, scheduler, FFI details)
- **Security Model**: `docs/security-model.md`
- **Release Process**: `docs/release-playbook.md`
