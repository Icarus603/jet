# Jet Release Playbook

## 1. Pre-Release Checklist

1. Run baseline gates:
   - `cargo fmt --all -- --check`
   - `cargo clippy --workspace --all-targets -- -D warnings`
   - `cargo test --workspace`
   - `bash scripts/check_safety_docs.sh`
   - `bash scripts/check_release_files.sh`
2. Verify `PRODUCTION_READINESS.md` checkboxes are up to date.
3. Update `CHANGELOG.md` with release notes.
4. Confirm release files exist: `README.md`, `LICENSE`, `CHANGELOG.md`, `SECURITY.md`.

## Required Branch Protection Checks

Release branches must require the following checks as required status checks:
- `Check Formatting`
- `Clippy Linting`
- `Release Files`
- `Test (Linux)`
- `Test (macOS)`
- `Safety Docs`
- `Documentation`

## 2. RC Process

1. Tag release candidate: `v1.0.0-rc.N`.
2. Let CI + release workflows complete for all target platforms.
3. Validate artifact checksums and install smoke tests.
4. Hold a stabilization window and triage blockers.

## 3. GA Process

1. Confirm no P0/P1 blockers remain.
2. Tag GA: `v1.0.0`.
3. Publish release assets and checksums.
4. Announce support policy and post-release verification status.

## 4. Rollback Procedure

1. Mark release as superseded in release notes.
2. Publish a patched tag (`vX.Y.Z+1`) with rollback rationale.
3. Notify users of affected versions and mitigation.

## 5. Post-Release Verification

1. Validate install paths for Linux/macOS artifacts.
2. Run smoke tests from release archives in clean environments.
3. Open follow-up issues for any deferred 1.0.1 tasks.
