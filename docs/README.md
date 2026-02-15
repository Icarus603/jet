# Jet Programming Language - Design Documentation

This directory contains the comprehensive design documentation for Jet, a new programming language optimized for the AI era.

## Design Documents

### Core Language
- **[language-spec.md](language-spec.md)** - Complete language specification including grammar, type system, effects system, and semantics

### Implementation
- **[compiler-architecture.md](compiler-architecture.md)** - 8-phase compiler pipeline from source to binary
- **[runtime-design.md](runtime-design.md)** - Memory management (Immix GC), concurrency runtime, and object representation
- **[stdlib-design.md](stdlib-design.md)** - Standard library API design

### Ecosystem
- **[tooling-design.md](tooling-design.md)** - CLI, package manager, LSP, formatter, and testing infrastructure
- **[ai-integration.md](ai-integration.md)** - AI-human collaboration features (Jet's key differentiator)
- **[security-model.md](security-model.md)** - Capability-based security and safe execution
- **[release-playbook.md](release-playbook.md)** - Operational release procedure and rollback steps
- **[soundness-risk-register.md](soundness-risk-register.md)** - Type/effect soundness risk tracking
- **[v1.0.0-tracking.md](v1.0.0-tracking.md)** - Jet 1.0 release tracking checklist

## Quick Reference

### Language Goals
1. **Python-like readability** - Significant indentation, minimal punctuation
2. **Rust-level safety** - No null, no unchecked exceptions, memory safety
3. **AI-optimized** - Designed for code generation and human verification

### Key Features
- Static typing with Hindley-Milner inference
- Effects system for explicit error handling
- Structured concurrency (goroutines + channels)
- Generational garbage collection (Immix)

### Implementation Stack
- Host language: Rust
- Backend: LLVM (via inkwell)
- Runtime: Custom with Immix GC

### Toolchain Support Policy
- Supported LLVM series: **21.1.x**
- LLVM 17 and LLVM 21.2+ compatibility is not guaranteed
- LLVM patch updates are tracked through automated repository reminders

## Design Philosophy

Jet assumes code will increasingly be:
1. **Written by AI** - Types and effects guide correct generation
2. **Verified by humans** - Explicit, readable, locally checkable
3. **Executed safely** - Runtime and type system prevent entire bug classes

## Status

These documents represent the target design for Jet 1.0. Implementation proceeds in milestones defined by the compiler architecture.
