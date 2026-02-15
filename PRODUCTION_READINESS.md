# Jet 1.0 Production Readiness Checklist

This checklist tracks readiness gates defined in `docs/security-model.md`.

## Security Baseline Gates

- [x] No known unsoundness in type/effect checking
  - Evidence: `docs/soundness-risk-register.md`
  - Evidence: type/effect regression tests in CI
- [x] No failing tests in workspace
  - Evidence: `cargo test --workspace`
  - Evidence: CI `test-linux`, `test-macos`
- [x] No deny-level lint failures in workspace checks
  - Evidence: `cargo clippy --workspace --all-targets -- -D warnings`
  - Evidence: CI `clippy` job
- [x] FFI unsafe surfaces documented with `# Safety` contracts
  - Evidence: `bash scripts/check_safety_docs.sh`
  - Evidence: CI `safety-docs` job

## Full 1.0 Ship Gates

- [ ] Strict CI gates with no bypass for required checks
  - Evidence: `.github/workflows/ci.yml`
  - Evidence: `.github/workflows/release.yml`
  - Evidence: `scripts/check_release_files.sh`
- [ ] Reproducible cross-platform release artifacts
  - Evidence: `.github/workflows/release.yml`
- [ ] User-facing release docs and policies are accurate
  - Evidence: `README.md`, `CHANGELOG.md`, `SECURITY.md`, `docs/release-playbook.md`
- [ ] Deterministic release process
  - Evidence: tag-based release automation + artifact checksums

## Baseline Snapshot Commands

Run and capture output for each release candidate:

```bash
cargo fmt --all -- --check
cargo clippy --workspace --all-targets -- -D warnings
cargo test --workspace
cargo test -p jet-integration-tests --test test_compiler_pipeline
bash scripts/check_safety_docs.sh
```

## Local Verification Snapshot (2026-02-15, macOS)

- `cargo fmt --all -- --check` passed
- `cargo clippy --workspace --all-targets -- -D warnings` passed
- `bash scripts/check_safety_docs.sh` passed
- `cargo test --workspace` passed
- `cargo test -p jet-integration-tests --test test_compiler_pipeline` passed

Note: Full production sign-off still requires green blocking CI across Linux/macOS and release artifact validation.

## CI/Release Hardening Snapshot (2026-02-15)

- Windows CI/release workflows were removed as part of the Linux/macOS-only support policy.
- Required release metadata files are now enforced by `scripts/check_release_files.sh` in CI and release workflows.
- Permissive bypass patterns (`|| true`) were removed from workflow/script paths that are part of required gates.
