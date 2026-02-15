# Jet Security Model

## Overview

Jet 1.0 targets "safe by default" execution for locally run programs and server workloads.
Security is enforced through three layers:

1. Language-level safety (type/effect constraints)
2. Runtime isolation and memory safety
3. Tooling and supply-chain controls

## 1. Language-Level Security

### 1.1 Memory and Type Safety

- No raw pointer access in user-level Jet code
- No implicit null values
- Checked pattern matching and exhaustive enum handling
- Compile-time type checking with explicit effect tracking

### 1.2 Effect-Gated Unsafe Operations

Operations that cross trust boundaries must be explicit in signatures:

- File/network/process access
- FFI calls
- Privileged runtime operations

This allows static review of side effects before execution.

### 1.3 Deterministic Error Surfaces

Jet avoids unchecked exceptions. Failures are represented as typed outcomes or declared effects.
This improves auditability and reduces hidden control-flow attacks.

## 2. Runtime Security

### 2.1 Memory Safety at Runtime

- Managed allocations through the Jet GC/runtime
- Runtime checks at FFI boundaries
- Explicitly marked unsafe extern interfaces in Rust-side runtime code

### 2.2 Concurrency Safety

- Structured concurrency with scoped task lifetimes
- Coordinated shutdown/cancellation
- Channel-based communication patterns over ad-hoc shared mutation

### 2.3 FFI Boundary Rules

All FFI entry points must:

- Validate pointers and sizes
- Define ownership/lifetime semantics
- Document safety contracts

## 3. Tooling and Supply Chain

### 3.1 Build and Verification Gates

Recommended release gates:

- `cargo fmt --all -- --check`
- `cargo clippy --workspace --all-targets`
- `cargo test --workspace`

### 3.2 Dependency Integrity

- Lockfile-based reproducible builds
- Registry publish/install workflows with explicit manifests
- CI policy to reject unresolved high-severity warnings/errors in release branches

## 4. Threat Model (Jet 1.0)

In scope:

- Memory corruption via language/runtime misuse
- Accidental privilege escalation through implicit effects
- Supply-chain drift in dependencies

Out of scope (Jet 1.0):

- Host sandboxing of untrusted binaries (OS/container responsibility)
- Cryptographic hardening guidance for application protocols

## 5. 1.0 Security Baseline

For Jet 1.0 "production-ready" status:

- No known unsoundness in type/effect checking
- No failing tests in workspace
- No deny-level lint failures in workspace checks
- FFI unsafe surfaces documented with `# Safety` contracts
