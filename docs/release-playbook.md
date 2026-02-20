# Jet Release Playbook

This document provides step-by-step instructions for releasing Jet versions.

## Table of Contents

1. [Pre-Release Checklist](#1-pre-release-checklist)
2. [Version Bumping Procedure](#2-version-bumping-procedure)
3. [Testing Procedure](#3-testing-procedure)
4. [RC Process](#4-rc-process)
5. [GA Process](#5-ga-process)
6. [Release Notes Template](#6-release-notes-template)
7. [Post-Release Verification](#7-post-release-verification)
8. [Rollback Procedure](#8-rollback-procedure)

---

## 1. Pre-Release Checklist

### 1.1 Quality Gates

Run all baseline gates locally:

```bash
# Format check
cargo fmt --all -- --check

# Lint check
cargo clippy --workspace --all-targets -- -D warnings

# Test suite
cargo test --workspace
cargo test -p jet-integration-tests --test test_compiler_pipeline

# Safety documentation
bash scripts/check_safety_docs.sh

# Release files
bash scripts/check_release_files.sh
```

### 1.2 Documentation Verification

- [ ] `README.md` is up to date with current features
- [ ] `CHANGELOG.md` has entry for this version
- [ ] `SECURITY.md` is current
- [ ] `LICENSE` file is present
- [ ] `docs/user-guide.md` reflects current syntax
- [ ] `docs/language-spec.md` is accurate
- [ ] `docs/user-guide.md` support matrix matches actual shipped 1.0 scope

### 1.3 Version Updates

Update version numbers in:
- [ ] `Cargo.toml` (workspace root)
- [ ] `CHANGELOG.md`
- [ ] Any version references in documentation

---

## 2. Version Bumping Procedure

### 2.1 Determine Version Type

- **Patch** (X.Y.Z+1): Bug fixes only
- **Minor** (X.Y+1.0): New features, backwards compatible
- **Major** (X+1.0.0): Breaking changes

### 2.2 Update Version Strings

```bash
# Update all Cargo.toml files in workspace
find . -name "Cargo.toml" -exec sed -i '' 's/^version = "X.Y.Z"/version = "X.Y.Z+1"/' {} \;
```

### 2.3 Update CHANGELOG.md

Add new section at the top:

```markdown
## [X.Y.Z] - YYYY-MM-DD

### Added
- New features

### Changed
- Changes in existing functionality

### Deprecated
- Soon-to-be removed features

### Removed
- Now removed features

### Fixed
- Bug fixes

### Security
- Security improvements
```

---

## 3. Testing Procedure

### 3.1 Local Testing

```bash
# Full test suite
cargo test --workspace --release

# Integration tests
cargo test -p jet-integration-tests

# Example programs
cargo build -p jet-cli --release
bash scripts/test_examples.sh
```

### 3.2 Dry-Run Release

```bash
# Test release build locally
bash scripts/release-dry-run.sh

# Smoke test installation
bash scripts/smoke-test-install.sh
```

### 3.3 Platform-Specific Testing

Test on both platforms:
- [ ] macOS (ARM64)
- [ ] macOS (x64) - if applicable
- [ ] Linux (x64)

---

## 4. RC Process

### 4.1 Create Release Candidate

```bash
# Ensure you're on main and up to date
git checkout main
git pull origin main

# Create RC tag
git tag v1.0.0-rc.1

# Push tag (triggers release workflow)
git push origin v1.0.0-rc.1
```

### 4.2 Monitor CI/CD

Watch for workflow completion:
- [ ] CI workflow passes
- [ ] Release workflow completes
- [ ] All artifacts are generated

### 4.3 Validate Artifacts

```bash
# Download artifacts from GitHub release
# Verify checksums
shasum -a 256 -c SHA256SUMS.txt

# Test installation
# macOS
tar xzf jet-v1.0.0-rc.1-macos-arm64.tar.gz
./jet-v1.0.0-rc.1-macos-arm64/jet --version

# Linux
tar xzf jet-v1.0.0-rc.1-linux-x64.tar.gz
./jet-v1.0.0-rc.1-linux-x64/jet --version
```

### 4.4 Stabilization Window

- Duration: 1 week for RC.1, shorter for subsequent RCs
- Collect feedback from early adopters
- Triage and fix blockers
- Repeat RC process if needed: `v1.0.0-rc.2`, etc.

---

## 5. GA Process

### 5.1 Final Verification

- [ ] No P0/P1 blockers remain
- [ ] All RC feedback addressed
- [ ] Final QA sign-off

### 5.2 Create GA Release

```bash
# Ensure you're on main and up to date
git checkout main
git pull origin main

# Create GA tag
git tag v1.0.0

# Push tag (triggers release workflow)
git push origin v1.0.0
```

### 5.3 Publish Release

1. Go to GitHub Releases page
2. Find the draft release created by CI
3. Edit release notes
4. Publish release (or mark as pre-release if needed)

### 5.4 Post-Release Announcement

- [ ] Post to GitHub Discussions
- [ ] Update website if applicable
- [ ] Social media announcement

---

## 6. Release Notes Template

```markdown
## Jet vX.Y.Z

### Highlights

Brief summary of the most important changes.

### Installation

**macOS (Homebrew):**
```bash
brew tap Icarus603/jet
brew install jet-lang
```

**Linux:**
```bash
curl -fsSL https://raw.githubusercontent.com/Icarus603/jet/main/scripts/install.sh | bash
```

**Direct Download:**
Download the appropriate binary from the assets below.

### Changes

#### Added
-

#### Changed
-

#### Fixed
-

#### Security
-

### SHA256 Checksums

See SHA256SUMS.txt file in assets.

### Verification

All artifacts are signed with Sigstore cosign. See SIGNING_VERIFICATION.md for instructions.

### Compatibility

- Minimum macOS version: 12.0 (Monterey)
- Minimum Linux version: Ubuntu 20.04 / glibc 2.31

### Known Issues

None.

### Support

- Documentation: https://github.com/Icarus603/jet/tree/main/docs
- Issues: https://github.com/Icarus603/jet/issues
- Discussions: https://github.com/Icarus603/jet/discussions
```

---

## 7. Post-Release Verification

### 7.1 Immediate Verification (within 1 hour)

- [ ] GitHub release page is live
- [ ] All artifacts are downloadable
- [ ] SHA256SUMS.txt matches artifacts
- [ ] Signatures are present

### 7.2 Installation Verification (within 24 hours)

Test on clean systems:

```bash
# macOS Homebrew
brew tap Icarus603/jet
brew install jet-lang
jet --version

# Linux install script
curl -fsSL https://raw.githubusercontent.com/Icarus603/jet/vX.Y.Z/scripts/install.sh | bash
jet --version
```

### 7.3 Documentation Verification

- [ ] User guide is accessible
- [ ] API docs are generated
- [ ] Examples compile and run

### 7.4 Monitoring

- [ ] Watch for new issues labeled `regression`
- [ ] Monitor download counts
- [ ] Collect user feedback

---

## 8. Rollback Procedure

If a critical issue is discovered:

### 8.1 Immediate Actions

1. **Mark release as superseded:**
   - Edit release notes
   - Add "SUPERSEDED - DO NOT USE" at the top

2. **Create advisory:**
   - Document the issue in GitHub Security Advisory if applicable
   - Post to Discussions

### 8.2 Prepare Hotfix

```bash
# Create hotfix branch
git checkout -b hotfix/vX.Y.Z+1 vX.Y.Z

# Apply minimal fix
git commit -m "Fix: [description of fix]"

# Tag new version
git tag vX.Y.Z+1
git push origin vX.Y.Z+1
```

### 8.3 Notify Users

- [ ] Update release notes with migration guide
- [ ] Post to GitHub Discussions
- [ ] Update SECURITY.md if security-related

---

## Required Branch Protection Checks

Configure these as required status checks in GitHub settings:

- `Check Formatting`
- `Clippy Linting`
- `Release Files`
- `Test (Linux)`
- `Test (macOS)`
- `Safety Docs`
- `Documentation`

To configure:
1. Go to Settings > Branches
2. Edit main branch protection rule
3. Enable "Require status checks to pass before merging"
4. Add the checks above to required status checks

---

## Appendix: Quick Reference

### Commands

```bash
# Full local verification
cargo fmt --all -- --check && \
cargo clippy --workspace --all-targets -- -D warnings && \
cargo test --workspace && \
bash scripts/check_safety_docs.sh && \
bash scripts/check_release_files.sh

# Release dry-run
bash scripts/release-dry-run.sh

# Smoke test
bash scripts/smoke-test-install.sh
```

### Contact

- Release issues: File GitHub issue with `release-blocker` label
- Security issues: security@jet-lang.org
