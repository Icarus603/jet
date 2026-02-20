# Changelog

All notable changes to Jet are documented in this file.

## [1.0.0] - 2026-02-20

### Added
- **Standard Library**: Complete implementations for JSON, DateTime, HTTP client, URL parsing, and Math functions via Rust FFI bridges
- **Production Release**: Jet 1.0.0 with signed release artifacts (Linux x64, macOS ARM64)
- **Homebrew Tap**: Official tap at `Icarus603/jet` with formula `jet-lang`
- **Production Readiness**: All CI gates passing (formatting, clippy, tests, security, documentation)

### Changed
- Renamed Homebrew formula from `jet` to `jet-lang` to avoid conflict with `go-jet/jet`
- Simplified release workflow: removed problematic SBOM generation and macOS x64 cross-compilation

## [Unreleased]

### Added
- Production readiness checklist in `PRODUCTION_READINESS.md`.
- Safety documentation gate script: `scripts/check_safety_docs.sh`.
- Release playbook: `docs/release-playbook.md`.

### Changed
- `jet build` now compiles all workspace members.
- `jet test` now executes discovered test functions through real compile/run cycles.
- Runtime library is now required for executable linking.
- CI clippy gate is strict (`-D warnings`).
- CI example parsing no longer ignores failures.

### Security
- Expanded `# Safety` docs for unsafe stdlib FFI entry points.

## [0.1.0] - 2026-02-15

- Initial public development release.
