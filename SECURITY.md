# Security Policy

Jet security posture and baseline criteria are defined in `docs/security-model.md`.

## Supported Versions

Only the latest release line is supported for security fixes during 1.0 stabilization.

## Reporting a Vulnerability

Please report vulnerabilities privately to the Jet maintainers before public disclosure.

Include:

- Affected version/commit
- Reproduction steps
- Impact assessment
- Suggested remediation (if available)

## Release Security Gates

Before release:

- `cargo test --workspace` passes
- `cargo clippy --workspace --all-targets -- -D warnings` passes
- `bash scripts/check_safety_docs.sh` passes
- No known unresolved soundness-critical issues in type/effect checking
