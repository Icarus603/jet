# Jet Operations Runbook

This document provides operational guidance for maintaining and troubleshooting Jet releases.

## Table of Contents

1. [Incident Response](#incident-response)
2. [Troubleshooting](#troubleshooting)
3. [Rollback Procedures](#rollback-procedures)
4. [Release Verification](#release-verification)
5. [Monitoring](#monitoring)

---

## Incident Response

### Severity Levels

- **P0 (Critical)**: Security vulnerability, data loss, or complete service outage
- **P1 (High)**: Major functionality broken, significant performance degradation
- **P2 (Medium)**: Minor functionality issues, workarounds available
- **P3 (Low)**: Cosmetic issues, documentation errors

### Response Procedures

#### P0 Security Incident

1. **Immediate (0-15 minutes)**:
   - Assess scope and impact
   - Identify affected versions
   - Notify security team

2. **Short-term (15-60 minutes)**:
   - Prepare security patch
   - Update `SECURITY.md` with advisory
   - Create private security branch if needed

3. **Medium-term (1-24 hours)**:
   - Release patched version
   - Notify users via GitHub Security Advisory
   - Update documentation

#### P1 Release Blocker

1. Assess if issue blocks release
2. If blocking:
   - Create hotfix branch from release tag
   - Apply minimal fix
   - Fast-track through CI
   - Release as patched version

### Communication Channels

- **Internal**: Team Slack/Discord
- **External**: GitHub Issues, GitHub Security Advisories
- **Users**: Release notes, `@jet_lang` Twitter (if applicable)

---

## Troubleshooting

### Build Issues

#### LLVM Not Found

**Symptoms**:
```
error: could not find native static library `LLVM-21`
```

**Resolution**:
1. Verify LLVM 21 is installed:
   ```bash
   # macOS
   brew install llvm@21
   export LLVM_SYS_211_PREFIX=/opt/homebrew/opt/llvm@21  # ARM64
   export LLVM_SYS_211_PREFIX=/usr/local/opt/llvm@21     # x64

   # Linux (Ubuntu/Debian)
   sudo apt-get install llvm-21-dev libclang-21-dev
   export LLVM_SYS_211_PREFIX=/usr/lib/llvm-21
   ```

2. Verify environment variable is set:
   ```bash
   echo $LLVM_SYS_211_PREFIX
   ```

#### Clippy Warnings Fail CI

**Symptoms**:
CI fails with clippy warnings treated as errors.

**Resolution**:
1. Run clippy locally:
   ```bash
   cargo clippy --workspace --all-targets -- -D warnings
   ```

2. Fix or allow specific warnings:
   ```rust
   #[allow(clippy::specific_lint)]
   fn problematic_function() { }
   ```

### Test Failures

#### Platform-Specific Test Failures

**Symptoms**:
Tests pass on one platform but fail on another.

**Resolution**:
1. Check for platform-specific code paths
2. Use conditional compilation:
   ```rust
   #[cfg(target_os = "macos")]
   fn macos_specific() { }
   ```

3. Skip non-essential tests on specific platforms:
   ```rust
   #[cfg_attr(target_os = "macos", ignore)]
   fn linux_only_test() { }
   ```

#### Flaky Tests

**Symptoms**:
Tests pass intermittently.

**Resolution**:
1. Identify race conditions or timing issues
2. Add retry logic for external dependencies
3. Mark as `#[ignore]` if truly flaky, with issue reference

### Release Issues

#### Release Workflow Fails

**Symptoms**:
GitHub Actions release workflow fails.

**Resolution**:
1. Check workflow logs for specific errors
2. Common issues:
   - Missing release files: Run `bash scripts/check_release_files.sh`
   - LLVM not available: Check LLVM installation step
   - Network issues: Retry workflow

3. If release is partially complete:
   - Delete draft release
   - Delete tag: `git push origin :refs/tags/vX.Y.Z`
   - Retag and push again

#### Checksum Mismatch

**Symptoms**:
Install script reports checksum mismatch.

**Resolution**:
1. Verify artifact integrity:
   ```bash
   shasum -a 256 jet-vX.Y.Z-platform-arch.tar.gz
   ```

2. Check SHA256SUMS.txt in release
3. If mismatch is confirmed, rebuild and re-release

---

## Rollback Procedures

### Release Rollback

If a release has critical issues:

1. **Mark release as superseded**:
   - Edit release notes
   - Add "SUPERSEDED - DO NOT USE" warning at top

2. **Publish patched release**:
   ```bash
   # Determine next patch version
   git tag v1.0.1  # if rolling back from v1.0.0
   git push origin v1.0.1
   ```

3. **Update documentation**:
   - Update `CHANGELOG.md`
   - Add rollback rationale

4. **Notify users**:
   - Post to GitHub Discussions
   - Update release notes with migration guide

### Homebrew Rollback

If Homebrew formula needs rollback:

1. Revert formula in tap repository:
   ```bash
   git revert HEAD
   git push
   ```

2. Or manually update to previous version:
   ```ruby
   # In homebrew-jet/Formula/jet.rb
   url "https://github.com/Icarus603/jet/archive/refs/tags/vPREVIOUS.tar.gz"
   sha256 "PREVIOUS_SHA256"
   ```

---

## Release Verification

### Pre-Release Checklist

Run before tagging any release:

```bash
# 1. Quality gates
cargo fmt --all -- --check
cargo clippy --workspace --all-targets -- -D warnings
cargo test --workspace
bash scripts/check_safety_docs.sh
bash scripts/check_release_files.sh

# 2. Build verification
cargo build --workspace --release

# 3. Dry-run release
bash scripts/release-dry-run.sh

# 4. Smoke test
bash scripts/smoke-test-install.sh
```

### Post-Release Verification

After release is published:

1. **Verify GitHub release**:
   - All artifacts present
   - SHA256SUMS.txt included
   - Release notes accurate

2. **Test install methods**:
   ```bash
   # macOS Homebrew
   brew tap Icarus603/jet
   brew install jet
   jet --version

   # Linux install script
   curl -fsSL https://raw.githubusercontent.com/Icarus603/jet/main/scripts/install.sh | bash
   ```

3. **Verify documentation**:
   - User guide accessible
   - API docs generated
   - Examples work

---

## Monitoring

### CI Health Monitoring

Monitor these metrics:

- **Build success rate**: Should be >95%
- **Test flakiness**: Track tests that fail intermittently
- **Build duration**: Watch for significant increases

### Release Metrics

Track for each release:

- Download counts per platform
- Install success rate
- Issue reports within 48 hours

### Alerting

Set up alerts for:

- CI failure on main branch
- Release workflow failure
- Security advisory published

---

## Emergency Contacts

- **Security issues**: security@jet-lang.org (or GitHub Security Advisory)
- **Release issues**: File GitHub issue with `release-blocker` label
- **General support**: GitHub Discussions

---

## Appendix: Common Commands

### Debug Build Issues

```bash
# Verbose build
cargo build --workspace --verbose

# Check specific crate
cargo check -p jet-parser

# Clean and rebuild
cargo clean && cargo build
```

### Debug Test Issues

```bash
# Run single test
cargo test --test test_name -- specific_test

# Run with output
cargo test --workspace -- --nocapture

# Run ignored tests
cargo test --workspace -- --ignored
```

### Release Debugging

```bash
# Test release build locally
cargo build --workspace --release

# Check binary size
ls -lh target/release/jet

# Check dynamic dependencies
otool -L target/release/jet  # macOS
ldd target/release/jet       # Linux
```
