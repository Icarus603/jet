//! Jet Standard Library Runtime
//!
//! This crate provides the Rust runtime implementation for the Jet programming
//! language's standard library. It implements core types (Vec, Map, Set, String,
//! Option, Result) with integration to the Immix garbage collector.
//!
//! The types in this crate are designed to be used from compiled Jet code
//! through the C ABI.

#![allow(clippy::missing_safety_doc)]
#![allow(unused_doc_comments)]
#![allow(unused_mut)]
#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(clippy::should_implement_trait)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::suspicious_open_options)]
#![allow(clippy::manual_strip)]
#![allow(mismatched_lifetime_syntaxes)]
#![allow(clippy::wrong_self_convention)]
#![allow(clippy::unnecessary_cast)]
#![allow(clippy::manual_range_contains)]
#![allow(clippy::cast_abs_to_unsigned)]
#![allow(clippy::approx_constant)]
#![allow(clippy::type_complexity)]
#![allow(clippy::needless_borrow)]
#![allow(clippy::if_same_then_else)]
#![allow(clippy::result_unit_err)]

pub mod alloc;
pub mod base64;
pub mod concurrent;
pub mod datetime;
pub mod ffi;
pub mod io;
pub mod json;
pub mod map;
pub mod math;
pub mod net;
pub mod option;
pub mod process;
pub mod result;
pub mod set;
pub mod string;
pub mod time;
pub mod traits;
pub mod url;
pub mod vec;

// Re-export main types
pub use alloc::{gc_alloc, gc_free, gc_realloc, GcBox, GcRoot};
pub use concurrent::{Channel, Mutex, RwLock, Task};
pub use ffi::{CStr, CString, NulError, Utf8Error};
pub use io::{
    BufferedReader, BufferedWriter, File, FileMode, IoError, IoErrorKind, Stderr, Stdin, Stdout,
};
pub use map::Map;
pub use net::{
    HttpClient, HttpRequest, HttpResponse, SocketAddr, TcpListener, TcpStream, UdpSocket,
};
pub use option::Option;
pub use process::{
    abort, current_dir, current_exe, exit, set_current_dir, var, vars, Child, Command, ExitStatus,
    Output, Stdio, VarError,
};
pub use result::Result;
pub use set::Set;
pub use string::{JetString, StringBuilder};
pub use time::{Duration, Instant, SystemTime, SystemTimeError};
pub use vec::Vec;

// JSON module
pub use json::{
    from_slice, from_str, to_string, to_string_pretty, to_vec, DeserializeError, Number,
    ParseError as JsonParseError, SerializeError, Value as JsonValue,
};

// DateTime module
pub use datetime::{
    Date, DateTime, DateTimeError, ParseError as DateTimeParseError, Time, TimeZone, UtcOffset,
    Weekday,
};

// Base64 module
pub use base64::{
    decode, decode_config, encode, encode_config, DecodeError as Base64DecodeError,
    Variant as Base64Variant,
};

// URL module
pub use url::{
    decode as url_decode, decode_component as url_decode_component, encode as url_encode,
    encode_component as url_encode_component, ParseError as UrlParseError, QueryPairs, Url,
};

/// Type ID constants for GC
pub mod type_ids {
    use jet_rt_gc::TypeId;

    pub const VEC: TypeId = TypeId::new(1);
    pub const MAP: TypeId = TypeId::new(2);
    pub const SET: TypeId = TypeId::new(3);
    pub const STRING: TypeId = TypeId::new(4);
    pub const OPTION: TypeId = TypeId::new(5);
    pub const RESULT: TypeId = TypeId::new(6);
    pub const FILE: TypeId = TypeId::new(7);
    pub const DURATION: TypeId = TypeId::new(8);
    pub const INSTANT: TypeId = TypeId::new(9);
    pub const SYSTEM_TIME: TypeId = TypeId::new(10);
    pub const TCP_STREAM: TypeId = TypeId::new(11);
    pub const TCP_LISTENER: TypeId = TypeId::new(12);
    pub const UDP_SOCKET: TypeId = TypeId::new(13);
    pub const HTTP_CLIENT: TypeId = TypeId::new(14);
    pub const CHANNEL: TypeId = TypeId::new(15);
    pub const MUTEX: TypeId = TypeId::new(16);
    pub const RWLOCK: TypeId = TypeId::new(17);
    pub const CSTRING: TypeId = TypeId::new(18);
    pub const CHILD: TypeId = TypeId::new(19);
    pub const COMMAND: TypeId = TypeId::new(20);
    // JSON types
    pub const JSON_VALUE: TypeId = TypeId::new(21);
    // DateTime types
    pub const DATE: TypeId = TypeId::new(22);
    pub const TIME: TypeId = TypeId::new(23);
    pub const DATETIME: TypeId = TypeId::new(24);
    // URL types
    pub const URL: TypeId = TypeId::new(25);
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
