# Jet 1.0 Soundness Risk Register

This register tracks known soundness-relevant risks for type/effect checking and their release status.

## Scope

- Type inference and unification
- Effect checking and effect unification
- Trait resolution/coherence decisions
- Subtyping/variance behavior

## Risk Entries

| ID | Area | Risk | Status | Evidence |
|---|---|---|---|---|
| SR-001 | Effect unification | Function effect sets may unify incompletely in edge cases | Mitigated (tests required per change) | `compiler/typeck/src/unify.rs` + regression tests |
| SR-002 | Trait argument matching | Trait arg matching may miss some substitution paths | Mitigated (tests required per change) | `compiler/typeck/src/traits.rs` + coherence tests |
| SR-003 | Subtyping/variance | Conservative equality-based behavior may reject valid programs but should not accept invalid ones | Accepted for 1.0 | `compiler/typeck/src/constraints.rs` |

## Release Rule

Any risk marked Open or Unknown blocks `v1.0.0` until:

1. Mitigated with tests, or
2. Explicitly accepted with rationale and documented user impact.
