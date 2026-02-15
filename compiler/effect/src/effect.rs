//! Effect representation for the Jet language.
//!
//! This module defines the core data structures for representing effects
//! in the compiler. Effects track what operations a function can perform,
//! such as I/O, async operations, or custom error types.

use indexmap::IndexSet;
use std::fmt;
use std::hash::Hash;

/// A unique identifier for an effect definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct EffectId(pub u32);

impl EffectId {
    /// Creates a new effect ID.
    pub const fn new(id: u32) -> Self {
        Self(id)
    }
}

/// A unique identifier for an effect variable (for polymorphism).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct EffectVar(pub u32);

impl EffectVar {
    /// Creates a new effect variable.
    pub const fn new(id: u32) -> Self {
        Self(id)
    }
}

/// Represents a set of effects that a function can perform.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EffectSet {
    /// The effects in this set, stored in a deterministic order.
    effects: IndexSet<EffectInstance>,
    /// Whether this set represents "all effects" (top of the lattice).
    is_top: bool,
    /// Whether this set is polymorphic over some effect variable.
    effect_var: Option<EffectVar>,
}

impl EffectSet {
    /// Creates an empty effect set (pure function).
    pub fn empty() -> Self {
        Self {
            effects: IndexSet::new(),
            is_top: false,
            effect_var: None,
        }
    }

    /// Creates an effect set representing "all effects".
    pub fn top() -> Self {
        Self {
            effects: IndexSet::new(),
            is_top: true,
            effect_var: None,
        }
    }

    /// Creates an effect set with a single effect.
    pub fn singleton(effect: EffectInstance) -> Self {
        let mut set = Self::empty();
        set.insert(effect);
        set
    }

    /// Creates a polymorphic effect set with a variable.
    pub fn polymorphic(var: EffectVar) -> Self {
        Self {
            effects: IndexSet::new(),
            is_top: false,
            effect_var: Some(var),
        }
    }

    /// Returns true if this is the empty set (pure function).
    pub fn is_empty(&self) -> bool {
        !self.is_top && self.effects.is_empty() && self.effect_var.is_none()
    }

    /// Returns true if this represents all effects.
    pub fn is_top(&self) -> bool {
        self.is_top
    }

    /// Returns true if this is a polymorphic effect set.
    pub fn is_polymorphic(&self) -> bool {
        self.effect_var.is_some()
    }

    /// Gets the effect variable if polymorphic.
    pub fn effect_var(&self) -> Option<EffectVar> {
        self.effect_var
    }

    /// Returns the number of effects in the set.
    pub fn len(&self) -> usize {
        self.effects.len()
    }

    /// Adds an effect to the set.
    pub fn insert(&mut self, effect: EffectInstance) {
        if !self.is_top {
            self.effects.insert(effect);
        }
    }

    /// Removes an effect from the set.
    pub fn remove(&mut self, effect: &EffectInstance) {
        if !self.is_top {
            self.effects.shift_remove(effect);
        }
    }

    /// Checks if the set contains a specific effect.
    pub fn contains(&self, effect: &EffectInstance) -> bool {
        if self.is_top {
            return true;
        }
        self.effects.contains(effect)
    }

    /// Checks if this set contains another set (subset relationship).
    pub fn contains_set(&self, other: &EffectSet) -> bool {
        if self.is_top {
            return true;
        }
        if other.is_top {
            return false;
        }
        other.effects.iter().all(|e| self.effects.contains(e))
    }

    /// Returns an iterator over the effects.
    pub fn iter(&self) -> impl Iterator<Item = &EffectInstance> {
        self.effects.iter()
    }

    /// Merges another effect set into this one (union).
    pub fn union(&mut self, other: &EffectSet) {
        if other.is_top {
            self.is_top = true;
            self.effects.clear();
            self.effect_var = None;
        } else if !self.is_top {
            for effect in &other.effects {
                self.effects.insert(effect.clone());
            }
            // If either is polymorphic, keep the variable
            if self.effect_var.is_none() {
                self.effect_var = other.effect_var;
            }
        }
    }

    /// Computes the union of two effect sets.
    pub fn merged(&self, other: &EffectSet) -> EffectSet {
        let mut result = self.clone();
        result.union(other);
        result
    }

    /// Subtracts another effect set from this one.
    pub fn subtract(&mut self, other: &EffectSet) {
        if other.is_top {
            self.effects.clear();
            self.is_top = false;
            self.effect_var = None;
        } else if !self.is_top {
            for effect in &other.effects {
                self.effects.shift_remove(effect);
            }
        }
    }

    /// Computes the difference of two effect sets.
    pub fn difference(&self, other: &EffectSet) -> EffectSet {
        let mut result = self.clone();
        result.subtract(other);
        result
    }

    /// Returns true if the two effect sets have any effects in common.
    pub fn intersects(&self, other: &EffectSet) -> bool {
        if self.is_top || other.is_top {
            return true;
        }
        self.effects.iter().any(|e| other.effects.contains(e))
    }

    /// Converts the effect set to a vector.
    pub fn to_vec(&self) -> Vec<EffectInstance> {
        self.effects.iter().cloned().collect()
    }
}

impl IntoIterator for EffectSet {
    type Item = EffectInstance;
    type IntoIter = indexmap::set::IntoIter<EffectInstance>;

    fn into_iter(self) -> Self::IntoIter {
        self.effects.into_iter()
    }
}

impl<'a> IntoIterator for &'a EffectSet {
    type Item = &'a EffectInstance;
    type IntoIter = indexmap::set::Iter<'a, EffectInstance>;

    fn into_iter(self) -> Self::IntoIter {
        self.effects.iter()
    }
}

impl fmt::Display for EffectSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_top {
            return write!(f, "!");
        }

        let effects: Vec<_> = self.effects.iter().map(|e| e.to_string()).collect();

        if effects.is_empty() {
            if let Some(var) = self.effect_var {
                write!(f, "!{}", var.0)
            } else {
                write!(f, "(pure)")
            }
        } else {
            write!(f, "!{}", effects.join(" | "))?;
            if let Some(var) = self.effect_var {
                write!(f, " | {}", var.0)?;
            }
            Ok(())
        }
    }
}

/// An individual effect instance with optional type arguments.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectInstance {
    /// The effect identifier.
    pub id: EffectId,
    /// The name of the effect (for display purposes).
    pub name: String,
    /// Type arguments for generic effects.
    pub type_args: Vec<ir::Ty>,
    /// Where this effect comes from.
    pub source: EffectSource,
}

use jet_ir as ir;

impl EffectInstance {
    /// Creates a new effect instance.
    pub fn new(id: EffectId, name: impl Into<String>) -> Self {
        Self {
            id,
            name: name.into(),
            type_args: Vec::new(),
            source: EffectSource::Explicit,
        }
    }

    /// Creates a new effect instance with type arguments.
    pub fn with_type_args(id: EffectId, name: impl Into<String>, type_args: Vec<ir::Ty>) -> Self {
        Self {
            id,
            name: name.into(),
            type_args,
            source: EffectSource::Explicit,
        }
    }

    /// Sets the source of this effect instance.
    pub fn with_source(mut self, source: EffectSource) -> Self {
        self.source = source;
        self
    }

    /// Returns true if this is a builtin effect (async, io, unsafe).
    pub fn is_builtin(&self) -> bool {
        matches!(
            self.name.as_str(),
            "async" | "io" | "unsafe" | "panic" | " diverges"
        )
    }
}

impl fmt::Display for EffectInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.type_args.is_empty() {
            write!(
                f,
                "[{}]",
                self.type_args
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }
        Ok(())
    }
}

impl PartialOrd for EffectInstance {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for EffectInstance {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Order by name for deterministic ordering
        self.name.cmp(&other.name)
    }
}

/// The source of an effect - where it originates from.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum EffectSource {
    /// Explicitly declared in the function signature.
    Explicit,
    /// Inherited from a function call.
    FromCall(DefId),
    /// Introduced by a handler.
    FromHandler,
    /// Inferred from the function body.
    Inferred,
    /// Part of a trait bound.
    FromTrait,
}

impl fmt::Display for EffectSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EffectSource::Explicit => write!(f, "explicit"),
            EffectSource::FromCall(id) => write!(f, "from call {:?}", id),
            EffectSource::FromHandler => write!(f, "from handler"),
            EffectSource::Inferred => write!(f, "inferred"),
            EffectSource::FromTrait => write!(f, "from trait"),
        }
    }
}

/// A definition ID for tracking where effects come from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct DefId(pub u32);

impl DefId {
    /// Creates a new definition ID.
    pub const fn new(id: u32) -> Self {
        Self(id)
    }
}

/// Builtin effects that are part of the language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinEffect {
    /// Async/await effect.
    Async,
    /// I/O operations (file, network, etc.).
    Io,
    /// Unsafe operations.
    Unsafe,
    /// Divergence (non-termination).
    Diverges,
    /// Panic/abort.
    Panic,
}

impl BuiltinEffect {
    /// Returns the name of the builtin effect.
    pub fn name(&self) -> &'static str {
        match self {
            BuiltinEffect::Async => "async",
            BuiltinEffect::Io => "io",
            BuiltinEffect::Unsafe => "unsafe",
            BuiltinEffect::Diverges => "diverges",
            BuiltinEffect::Panic => "panic",
        }
    }

    /// Returns the effect ID for a builtin effect.
    pub fn effect_id(&self) -> EffectId {
        // Builtin effects have fixed IDs 0-4
        match self {
            BuiltinEffect::Async => EffectId(0),
            BuiltinEffect::Io => EffectId(1),
            BuiltinEffect::Unsafe => EffectId(2),
            BuiltinEffect::Diverges => EffectId(3),
            BuiltinEffect::Panic => EffectId(4),
        }
    }

    /// Creates an effect instance for this builtin.
    pub fn instance(&self) -> EffectInstance {
        EffectInstance::new(self.effect_id(), self.name())
    }
}

impl fmt::Display for BuiltinEffect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// An effect handler that can intercept effect operations.
#[derive(Debug, Clone, PartialEq)]
pub struct EffectHandler {
    /// The effect being handled.
    pub effect: EffectId,
    /// The name of the handler.
    pub name: String,
    /// Operations handled by this handler.
    pub operations: Vec<HandledOperation>,
    /// Whether this handler is exhaustive (handles all operations).
    pub is_exhaustive: bool,
}

/// An operation handled by an effect handler.
#[derive(Debug, Clone, PartialEq)]
pub struct HandledOperation {
    /// The operation name.
    pub name: String,
    /// The handler body for this operation.
    pub handler_def_id: DefId,
    /// Whether the handler resumes.
    pub resumes: bool,
}

/// A scope entry for tracking active handlers.
#[derive(Debug, Clone, PartialEq)]
pub struct HandlerScope {
    /// The handlers active in this scope.
    pub handlers: Vec<EffectHandler>,
    /// The parent scope index (if any).
    pub parent: Option<usize>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_effect_set_empty() {
        let set = EffectSet::empty();
        assert!(set.is_empty());
        assert!(!set.is_top());
        assert_eq!(set.len(), 0);
    }

    #[test]
    fn test_effect_set_top() {
        let set = EffectSet::top();
        assert!(!set.is_empty());
        assert!(set.is_top());
        assert!(set.contains(&BuiltinEffect::Io.instance()));
    }

    #[test]
    fn test_effect_set_insert() {
        let mut set = EffectSet::empty();
        let effect = BuiltinEffect::Io.instance();
        set.insert(effect.clone());
        assert!(!set.is_empty());
        assert!(set.contains(&effect));
        assert_eq!(set.len(), 1);
    }

    #[test]
    fn test_effect_set_union() {
        let mut set1 = EffectSet::empty();
        set1.insert(BuiltinEffect::Io.instance());

        let mut set2 = EffectSet::empty();
        set2.insert(BuiltinEffect::Async.instance());

        set1.union(&set2);
        assert!(set1.contains(&BuiltinEffect::Io.instance()));
        assert!(set1.contains(&BuiltinEffect::Async.instance()));
        assert_eq!(set1.len(), 2);
    }

    #[test]
    fn test_effect_set_union_with_top() {
        let mut set1 = EffectSet::empty();
        set1.insert(BuiltinEffect::Io.instance());

        let set2 = EffectSet::top();
        set1.union(&set2);
        assert!(set1.is_top());
    }

    #[test]
    fn test_effect_set_difference() {
        let mut set1 = EffectSet::empty();
        set1.insert(BuiltinEffect::Io.instance());
        set1.insert(BuiltinEffect::Async.instance());

        let mut set2 = EffectSet::empty();
        set2.insert(BuiltinEffect::Io.instance());

        let diff = set1.difference(&set2);
        assert!(!diff.contains(&BuiltinEffect::Io.instance()));
        assert!(diff.contains(&BuiltinEffect::Async.instance()));
    }

    #[test]
    fn test_effect_set_contains_set() {
        let mut set1 = EffectSet::empty();
        set1.insert(BuiltinEffect::Io.instance());
        set1.insert(BuiltinEffect::Async.instance());

        let mut set2 = EffectSet::empty();
        set2.insert(BuiltinEffect::Io.instance());

        assert!(set1.contains_set(&set2));
        assert!(!set2.contains_set(&set1));
    }

    #[test]
    fn test_builtin_effects() {
        assert_eq!(BuiltinEffect::Async.name(), "async");
        assert_eq!(BuiltinEffect::Io.name(), "io");
        assert_eq!(BuiltinEffect::Unsafe.name(), "unsafe");

        let async_inst = BuiltinEffect::Async.instance();
        assert_eq!(async_inst.name, "async");
        assert!(async_inst.is_builtin());
    }

    #[test]
    fn test_effect_instance_display() {
        let effect = EffectInstance::new(EffectId(10), "MyError");
        assert_eq!(format!("{}", effect), "MyError");
    }

    #[test]
    fn test_effect_set_display() {
        let mut set = EffectSet::empty();
        assert_eq!(format!("{}", set), "(pure)");

        set.insert(BuiltinEffect::Io.instance());
        assert_eq!(format!("{}", set), "!io");

        set.insert(BuiltinEffect::Async.instance());
        let display = format!("{}", set);
        assert!(display.contains("async"));
        assert!(display.contains("io"));

        let top = EffectSet::top();
        assert_eq!(format!("{}", top), "!");
    }

    #[test]
    fn test_effect_source() {
        let source = EffectSource::Explicit;
        assert_eq!(format!("{}", source), "explicit");

        let source = EffectSource::FromCall(DefId(42));
        assert!(format!("{}", source).contains("42"));
    }
}
