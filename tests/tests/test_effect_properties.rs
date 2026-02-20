//! Property-based tests for the Jet effect system
//!
//! These tests verify algebraic properties of effect sets and effect inference.

#![allow(unused_variables)]

use jet_effect::{BuiltinEffect, EffectId, EffectInstance, EffectSet, EffectVar};

// ============================================================================
// Effect Set Algebraic Laws
// ============================================================================

#[test]
fn test_effect_set_union_associativity() {
    // (A ∪ B) ∪ C = A ∪ (B ∪ C)
    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());

    let mut b = EffectSet::empty();
    b.insert(BuiltinEffect::Async.instance());

    let mut c = EffectSet::empty();
    c.insert(BuiltinEffect::Panic.instance());

    // (A ∪ B) ∪ C
    let mut left = a.clone();
    left.union(&b);
    left.union(&c);

    // A ∪ (B ∪ C)
    let mut bc = b.clone();
    bc.union(&c);
    let mut right = a.clone();
    right.union(&bc);

    assert_eq!(left, right, "Union should be associative");
}

#[test]
fn test_effect_set_union_commutativity() {
    // A ∪ B = B ∪ A
    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());
    a.insert(BuiltinEffect::Async.instance());

    let mut b = EffectSet::empty();
    b.insert(BuiltinEffect::Panic.instance());
    b.insert(BuiltinEffect::Unsafe.instance());

    let mut left = a.clone();
    left.union(&b);

    let mut right = b.clone();
    right.union(&a);

    assert_eq!(left, right, "Union should be commutative");
}

#[test]
fn test_effect_set_union_identity() {
    // A ∪ ∅ = A
    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());
    a.insert(BuiltinEffect::Async.instance());

    let empty = EffectSet::empty();

    let mut result = a.clone();
    result.union(&empty);

    assert_eq!(result, a, "Empty set should be identity for union");
}

#[test]
fn test_effect_set_union_idempotence() {
    // A ∪ A = A
    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());
    a.insert(BuiltinEffect::Async.instance());

    let mut result = a.clone();
    result.union(&a);

    assert_eq!(result, a, "Union should be idempotent");
}

#[test]
fn test_effect_set_union_with_top() {
    // A ∪ ⊤ = ⊤
    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());

    let top = EffectSet::top();

    let mut result = a.clone();
    result.union(&top);

    assert!(result.is_top(), "Union with top should yield top");
}

#[test]
fn test_effect_set_contains_reflexivity() {
    // A ⊆ A
    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());
    a.insert(BuiltinEffect::Async.instance());

    assert!(a.contains_set(&a), "Set should contain itself");
}

#[test]
fn test_effect_set_contains_transitivity() {
    // If A ⊆ B and B ⊆ C, then A ⊆ C
    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());

    let mut b = EffectSet::empty();
    b.insert(BuiltinEffect::Io.instance());
    b.insert(BuiltinEffect::Async.instance());

    let mut c = EffectSet::empty();
    c.insert(BuiltinEffect::Io.instance());
    c.insert(BuiltinEffect::Async.instance());
    c.insert(BuiltinEffect::Panic.instance());

    assert!(a.contains_set(&a), "A ⊆ A");
    assert!(b.contains_set(&a), "A ⊆ B");
    assert!(c.contains_set(&b), "B ⊆ C");
    assert!(c.contains_set(&a), "Therefore A ⊆ C by transitivity");
}

#[test]
fn test_effect_set_empty_is_subset_of_all() {
    // ∅ ⊆ A for any A
    let empty = EffectSet::empty();

    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());

    let top = EffectSet::top();

    assert!(
        a.contains_set(&empty),
        "Empty set should be subset of any set"
    );
    assert!(
        top.contains_set(&empty),
        "Empty set should be subset of top"
    );
}

#[test]
fn test_effect_set_difference_properties() {
    // A - ∅ = A
    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());
    a.insert(BuiltinEffect::Async.instance());

    let empty = EffectSet::empty();
    let diff = a.difference(&empty);

    assert_eq!(diff, a, "A - ∅ = A");

    // A - A = ∅
    let diff2 = a.difference(&a);
    assert!(diff2.is_empty(), "A - A = ∅");

    // ∅ - A = ∅
    let diff3 = empty.difference(&a);
    assert!(diff3.is_empty(), "∅ - A = ∅");
}

// ============================================================================
// Effect Set Intersection Properties
// ============================================================================

#[test]
fn test_effect_set_intersection_commutativity() {
    // A ∩ B = B ∩ A (using intersects for partial check)
    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());
    a.insert(BuiltinEffect::Async.instance());

    let mut b = EffectSet::empty();
    b.insert(BuiltinEffect::Async.instance());
    b.insert(BuiltinEffect::Panic.instance());

    assert_eq!(
        a.intersects(&b),
        b.intersects(&a),
        "Intersection should be commutative"
    );
}

#[test]
fn test_effect_set_intersection_with_empty() {
    // A ∩ ∅ = ∅
    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());

    let empty = EffectSet::empty();

    assert!(
        !a.intersects(&empty),
        "Intersection with empty should be empty"
    );
    assert!(
        !empty.intersects(&a),
        "Intersection with empty should be empty"
    );
}

#[test]
fn test_effect_set_intersection_with_top() {
    // A ∩ ⊤ = A (everything intersects with top)
    let mut a = EffectSet::empty();
    a.insert(BuiltinEffect::Io.instance());

    let top = EffectSet::top();

    assert!(a.intersects(&top), "Everything intersects with top");
    assert!(top.intersects(&a), "Top intersects with everything");
}

// ============================================================================
// Effect Instance Properties
// ============================================================================

#[test]
fn test_effect_instance_equality() {
    let io1 = BuiltinEffect::Io.instance();
    let io2 = BuiltinEffect::Io.instance();
    let async_inst = BuiltinEffect::Async.instance();

    assert_eq!(io1, io2, "Same builtin effect instances should be equal");
    assert_ne!(io1, async_inst, "Different effects should not be equal");
}

#[test]
fn test_effect_instance_ordering() {
    // Effects should have deterministic ordering by name
    let io = BuiltinEffect::Io.instance();
    let async_inst = BuiltinEffect::Async.instance();
    let panic = BuiltinEffect::Panic.instance();

    let mut effects = [panic.clone(), io.clone(), async_inst.clone()];
    effects.sort();

    // Should be sorted alphabetically: async, io, panic
    assert_eq!(effects[0], async_inst);
    assert_eq!(effects[1], io);
    assert_eq!(effects[2], panic);
}

// ============================================================================
// Polymorphic Effect Set Properties
// ============================================================================

#[test]
fn test_polymorphic_effect_set_union() {
    // Polymorphic set with variable should preserve variable in union
    let var = EffectVar::new(1);
    let poly = EffectSet::polymorphic(var);

    let mut concrete = EffectSet::empty();
    concrete.insert(BuiltinEffect::Io.instance());

    let mut result = poly.clone();
    result.union(&concrete);

    assert!(
        result.is_polymorphic(),
        "Result should still be polymorphic"
    );
    assert!(result.contains(&BuiltinEffect::Io.instance()));
}

#[test]
fn test_polymorphic_effect_set_empty_union() {
    // !α ∪ ∅ = !α
    let var = EffectVar::new(1);
    let poly = EffectSet::polymorphic(var);

    let empty = EffectSet::empty();

    let mut result = poly.clone();
    result.union(&empty);

    assert!(result.is_polymorphic());
    assert_eq!(result.effect_var(), Some(var));
}

// ============================================================================
// Effect ID Properties
// ============================================================================

#[test]
fn test_effect_id_ordering() {
    let id1 = EffectId::new(1);
    let id2 = EffectId::new(2);
    let id3 = EffectId::new(3);

    assert!(id1 < id2, "Effect IDs should be ordered");
    assert!(id2 < id3, "Effect IDs should be ordered");
    assert!(id1 < id3, "Effect IDs should be transitive");
}

#[test]
fn test_builtin_effect_ids_are_fixed() {
    // Builtin effects have fixed IDs 0-4
    assert_eq!(BuiltinEffect::Async.effect_id().0, 0);
    assert_eq!(BuiltinEffect::Io.effect_id().0, 1);
    assert_eq!(BuiltinEffect::Unsafe.effect_id().0, 2);
    assert_eq!(BuiltinEffect::Diverges.effect_id().0, 3);
    assert_eq!(BuiltinEffect::Panic.effect_id().0, 4);
}

// ============================================================================
// Effect Set Display Properties
// ============================================================================

#[test]
fn test_effect_set_display_pure() {
    let empty = EffectSet::empty();
    assert_eq!(format!("{}", empty), "(pure)");
}

#[test]
fn test_effect_set_display_single_effect() {
    let mut set = EffectSet::empty();
    set.insert(BuiltinEffect::Io.instance());
    assert_eq!(format!("{}", set), "!io");
}

#[test]
fn test_effect_set_display_top() {
    let top = EffectSet::top();
    assert_eq!(format!("{}", top), "!");
}

#[test]
fn test_effect_set_display_multiple_effects() {
    let mut set = EffectSet::empty();
    set.insert(BuiltinEffect::Async.instance());
    set.insert(BuiltinEffect::Io.instance());

    let display = format!("{}", set);
    assert!(display.starts_with("!"), "Should start with !");
    assert!(display.contains("async"), "Should contain async");
    assert!(display.contains("io"), "Should contain io");
}

// ============================================================================
// Effect Handler Composition Properties
// ============================================================================

#[test]
fn test_effect_handler_creation() {
    use jet_effect::EffectHandler;

    let handler = EffectHandler {
        effect: EffectId::new(1),
        name: "TestHandler".to_string(),
        operations: vec![],
        is_exhaustive: true,
    };

    assert_eq!(handler.effect.0, 1);
    assert!(handler.is_exhaustive);
}

// ============================================================================
// Complex Effect Set Operations
// ============================================================================

#[test]
fn test_complex_effect_set_operations() {
    // Create a complex scenario with multiple operations
    let mut io_async = EffectSet::empty();
    io_async.insert(BuiltinEffect::Io.instance());
    io_async.insert(BuiltinEffect::Async.instance());

    let mut async_panic = EffectSet::empty();
    async_panic.insert(BuiltinEffect::Async.instance());
    async_panic.insert(BuiltinEffect::Panic.instance());

    // Union: should have io, async, panic
    let mut union = io_async.clone();
    union.union(&async_panic);
    assert!(union.contains(&BuiltinEffect::Io.instance()));
    assert!(union.contains(&BuiltinEffect::Async.instance()));
    assert!(union.contains(&BuiltinEffect::Panic.instance()));

    // Difference: io_async - async_panic = io
    let diff = io_async.difference(&async_panic);
    assert!(diff.contains(&BuiltinEffect::Io.instance()));
    assert!(!diff.contains(&BuiltinEffect::Async.instance()));
    assert!(!diff.contains(&BuiltinEffect::Panic.instance()));

    // Intersection check
    assert!(
        io_async.intersects(&async_panic),
        "Should intersect on async"
    );

    let pure = EffectSet::empty();
    assert!(
        !io_async.intersects(&pure),
        "Should not intersect with pure"
    );
}

#[test]
fn test_effect_set_chain_operations() {
    // Test chaining multiple operations
    let mut set = EffectSet::empty();

    // Add effects one by one
    set.insert(BuiltinEffect::Io.instance());
    assert_eq!(set.len(), 1);

    set.insert(BuiltinEffect::Async.instance());
    assert_eq!(set.len(), 2);

    // Adding duplicate should not increase size
    set.insert(BuiltinEffect::Io.instance());
    assert_eq!(set.len(), 2);

    // Remove one effect
    set.remove(&BuiltinEffect::Io.instance());
    assert_eq!(set.len(), 1);
    assert!(!set.contains(&BuiltinEffect::Io.instance()));
    assert!(set.contains(&BuiltinEffect::Async.instance()));

    // Remove all
    set.remove(&BuiltinEffect::Async.instance());
    assert!(set.is_empty());
}

// ============================================================================
// Edge Cases and Boundary Conditions
// ============================================================================

#[test]
fn test_effect_set_with_zero_id() {
    let id = EffectId::new(0);
    let effect = EffectInstance::new(id, "ZeroEffect");

    let mut set = EffectSet::empty();
    set.insert(effect.clone());

    assert!(set.contains(&effect));
}

#[test]
fn test_effect_set_with_large_id() {
    let id = EffectId::new(u32::MAX);
    let effect = EffectInstance::new(id, "MaxEffect");

    let mut set = EffectSet::empty();
    set.insert(effect.clone());

    assert!(set.contains(&effect));
}

#[test]
fn test_effect_var_with_zero() {
    let var = EffectVar::new(0);
    let set = EffectSet::polymorphic(var);

    assert!(set.is_polymorphic());
    assert_eq!(set.effect_var(), Some(var));
}

#[test]
fn test_empty_effect_set_operations() {
    let empty = EffectSet::empty();

    // All these should work without panic
    assert!(empty.is_empty());
    assert!(!empty.is_top());
    assert!(!empty.is_polymorphic());
    assert_eq!(empty.len(), 0);
    assert_eq!(empty.to_vec().len(), 0);
}

#[test]
fn test_top_effect_set_operations() {
    let top = EffectSet::top();

    // Top contains everything
    assert!(!top.is_empty());
    assert!(top.is_top());
    assert!(top.contains(&BuiltinEffect::Io.instance()));
    assert!(top.contains(&BuiltinEffect::Async.instance()));
    assert!(top.contains(&BuiltinEffect::Panic.instance()));

    // Top should absorb everything in union
    let mut top_copy = EffectSet::top();
    let mut other = EffectSet::empty();
    other.insert(BuiltinEffect::Io.instance());
    top_copy.union(&other);
    assert!(top_copy.is_top());
}

#[test]
fn test_effect_instance_with_type_args() {
    use jet_ir::Ty;

    let effect =
        EffectInstance::with_type_args(EffectId::new(1), "GenericEffect", vec![Ty::I32, Ty::Bool]);

    assert_eq!(effect.name, "GenericEffect");
    assert_eq!(effect.type_args.len(), 2);
}
