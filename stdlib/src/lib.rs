//! Jet Standard Library Runtime
//!
//! This crate provides the Rust runtime implementation for the Jet programming
//! language's standard library. It implements core types (Vec, Map, Set, String,
//! Option, Result) with integration to the Immix garbage collector.
//!
//! The types in this crate are designed to be used from compiled Jet code
//! through the C ABI.

#![allow(clippy::missing_safety_doc)]

pub mod map;
pub mod option;
pub mod result;
pub mod set;
pub mod string;
pub mod traits;
pub mod vec;

// Re-export main types
pub use map::Map;
pub use option::Option;
pub use result::Result;
pub use set::Set;
pub use string::{JetString, StringBuilder};
pub use vec::Vec;

/// Type ID constants for GC
pub mod type_ids {
    use jet_rt_gc::TypeId;

    pub const VEC: TypeId = TypeId::new(1);
    pub const MAP: TypeId = TypeId::new(2);
    pub const SET: TypeId = TypeId::new(3);
    pub const STRING: TypeId = TypeId::new(4);
    pub const OPTION: TypeId = TypeId::new(5);
    pub const RESULT: TypeId = TypeId::new(6);
}

/// Initialize the standard library runtime
pub fn init() {
    // Initialize any global state needed by the stdlib
}

/// Shutdown the standard library runtime
pub fn shutdown() {
    // Cleanup any global state
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_basic() {
        let mut v = Vec::new();
        assert!(v.is_empty());
        assert_eq!(v.len(), 0);

        v.push(42i64);
        assert!(!v.is_empty());
        assert_eq!(v.len(), 1);
        assert_eq!(v.get(0), Some(&42));
    }

    #[test]
    fn test_map_basic() {
        let mut m = Map::new();
        assert!(m.is_empty());

        m.insert("key", 42i64);
        assert!(!m.is_empty());
        assert_eq!(m.get(&"key"), Some(&42));
    }

    #[test]
    fn test_set_basic() {
        let mut s = Set::new();
        assert!(s.is_empty());

        s.insert(42i64);
        assert!(!s.is_empty());
        assert!(s.contains(&42));
    }
}
