# Changelog

All notable changes to Jet are documented in this file.

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
