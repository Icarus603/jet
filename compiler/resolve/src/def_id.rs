//! Definition IDs for uniquely identifying definitions in the AST
//!
//! DefIds are used throughout the compiler to uniquely identify functions,
//! types, variables, traits, modules, and other named entities.

use std::fmt;
use std::sync::atomic::{AtomicU32, Ordering};

/// A unique identifier for a definition in the program
///
/// DefIds are generated sequentially and are unique across the entire
/// compilation session. They can be used to look up definitions in
/// the resolver's definition tables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DefId(u32);

impl DefId {
    /// Creates a new DefId from a raw u32 value
    ///
    /// # Safety
    /// This should only be used for deserialization or testing.
    /// Normal code should use `DefIdGenerator` to create new DefIds.
    pub const fn from_raw(id: u32) -> Self {
        DefId(id)
    }

    /// Returns the raw u32 value of this DefId
    pub const fn as_raw(&self) -> u32 {
        self.0
    }

    /// Returns a placeholder DefId (useful for testing)
    pub const fn placeholder() -> Self {
        DefId(0)
    }

    /// Checks if this is the placeholder DefId
    pub const fn is_placeholder(&self) -> bool {
        self.0 == 0
    }
}

impl fmt::Display for DefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

/// A generator for creating unique DefIds
///
/// This struct uses an atomic counter to generate unique DefIds
/// in a thread-safe manner.
#[derive(Debug)]
pub struct DefIdGenerator {
    counter: AtomicU32,
}

impl Default for DefIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl DefIdGenerator {
    /// Creates a new DefId generator starting at 1
    ///
    /// DefId 0 is reserved as a placeholder/invalid value.
    pub fn new() -> Self {
        Self {
            counter: AtomicU32::new(1),
        }
    }

    /// Creates a new DefId generator with a specific starting value
    ///
    /// # Safety
    /// The starting value should be greater than 0 to avoid conflicts
    /// with the placeholder DefId.
    pub fn with_start(start: u32) -> Self {
        Self {
            counter: AtomicU32::new(start),
        }
    }

    /// Generates a new unique DefId
    pub fn next(&self) -> DefId {
        DefId(self.counter.fetch_add(1, Ordering::SeqCst))
    }

    /// Returns the number of DefIds generated so far
    pub fn count(&self) -> u32 {
        self.counter.load(Ordering::SeqCst) - 1
    }
}

/// A table mapping DefIds to their definitions
///
/// This is used to store and look up definitions by their DefId.
#[derive(Debug, Clone)]
pub struct DefIdTable<T> {
    entries: Vec<Option<T>>,
}

impl<T> Default for DefIdTable<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> DefIdTable<T> {
    /// Creates a new empty DefId table
    pub fn new() -> Self {
        Self {
            // Index 0 is reserved for placeholder
            entries: vec![None],
        }
    }

    /// Inserts a value at the given DefId
    ///
    /// # Panics
    /// Panics if the DefId is out of bounds.
    pub fn insert(&mut self, id: DefId, value: T) {
        let idx = id.as_raw() as usize;
        if idx >= self.entries.len() {
            self.entries.resize_with(idx + 1, || None);
        }
        self.entries[idx] = Some(value);
    }

    /// Gets a reference to the value at the given DefId
    pub fn get(&self, id: DefId) -> Option<&T> {
        self.entries
            .get(id.as_raw() as usize)
            .and_then(|opt| opt.as_ref())
    }

    /// Gets a mutable reference to the value at the given DefId
    pub fn get_mut(&mut self, id: DefId) -> Option<&mut T> {
        self.entries
            .get_mut(id.as_raw() as usize)
            .and_then(|opt| opt.as_mut())
    }

    /// Returns true if the table contains a value for the given DefId
    pub fn contains(&self, id: DefId) -> bool {
        self.get(id).is_some()
    }

    /// Returns an iterator over all entries
    pub fn iter(&self) -> impl Iterator<Item = (DefId, &T)> {
        self.entries
            .iter()
            .enumerate()
            .filter_map(|(idx, opt)| opt.as_ref().map(|v| (DefId(idx as u32), v)))
    }

    /// Returns the number of entries in the table
    pub fn len(&self) -> usize {
        self.entries.iter().filter(|opt| opt.is_some()).count()
    }

    /// Returns true if the table is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// A unique identifier for a module
///
/// ModuleIds are used to identify modules in the module graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleId(u32);

impl ModuleId {
    /// Creates a new ModuleId from a raw u32 value
    pub const fn from_raw(id: u32) -> Self {
        ModuleId(id)
    }

    /// Returns the raw u32 value of this ModuleId
    pub const fn as_raw(&self) -> u32 {
        self.0
    }

    /// Returns the root module ID
    pub const fn root() -> Self {
        ModuleId(0)
    }

    /// Checks if this is the root module
    pub const fn is_root(&self) -> bool {
        self.0 == 0
    }
}

impl fmt::Display for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "mod:{}", self.0)
    }
}

/// A generator for creating unique ModuleIds
#[derive(Debug)]
pub struct ModuleIdGenerator {
    counter: AtomicU32,
}

impl Default for ModuleIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleIdGenerator {
    /// Creates a new ModuleId generator starting at 0 (root)
    pub fn new() -> Self {
        Self {
            counter: AtomicU32::new(0),
        }
    }

    /// Generates a new unique ModuleId
    pub fn next(&self) -> ModuleId {
        ModuleId(self.counter.fetch_add(1, Ordering::SeqCst))
    }

    /// Returns the number of ModuleIds generated so far
    pub fn count(&self) -> u32 {
        self.counter.load(Ordering::SeqCst)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_def_id_creation() {
        let id = DefId::from_raw(42);
        assert_eq!(id.as_raw(), 42);
        assert_eq!(id.to_string(), "#42");
    }

    #[test]
    fn test_def_id_placeholder() {
        let placeholder = DefId::placeholder();
        assert!(placeholder.is_placeholder());
        assert_eq!(placeholder.as_raw(), 0);

        let real = DefId::from_raw(1);
        assert!(!real.is_placeholder());
    }

    #[test]
    fn test_def_id_generator() {
        let gen = DefIdGenerator::new();
        assert_eq!(gen.count(), 0);

        let id1 = gen.next();
        assert_eq!(id1.as_raw(), 1);
        assert_eq!(gen.count(), 1);

        let id2 = gen.next();
        assert_eq!(id2.as_raw(), 2);
        assert_eq!(gen.count(), 2);
    }

    #[test]
    fn test_def_id_table() {
        let mut table = DefIdTable::new();
        let id = DefId::from_raw(1);

        assert!(table.get(id).is_none());
        assert!(!table.contains(id));

        table.insert(id, "hello");
        assert_eq!(table.get(id), Some(&"hello"));
        assert!(table.contains(id));

        *table.get_mut(id).unwrap() = "world";
        assert_eq!(table.get(id), Some(&"world"));
    }

    #[test]
    fn test_module_id() {
        let root = ModuleId::root();
        assert!(root.is_root());
        assert_eq!(root.as_raw(), 0);

        let gen = ModuleIdGenerator::new();
        let id1 = gen.next();
        assert!(id1.is_root()); // First ID is root

        let id2 = gen.next();
        assert!(!id2.is_root());
        assert_eq!(id2.as_raw(), 1);
    }
}
