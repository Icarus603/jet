//! JSON - JSON parsing and serialization for Jet
//!
//! This module provides JSON Value types, parsing, and serialization
//! for the Jet programming language.

use std::collections::HashMap;
use std::vec::Vec;

/// A JSON value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Null value
    Null,
    /// Boolean value
    Bool(bool),
    /// Number value (integer or float)
    Number(Number),
    /// String value
    String(String),
    /// Array of values
    Array(Vec<Value>),
    /// Object with string keys
    Object(HashMap<String, Value>),
}

/// A JSON number (either integer or float).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Number {
    /// Integer value
    Int(i64),
    /// Float value
    Float(f64),
}

/// Error type for JSON parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    /// Error message
    pub message: String,
    /// Position in the input where the error occurred
    pub position: usize,
}

/// Error type for JSON serialization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SerializeError {
    /// Error message
    pub message: String,
}

/// Error type for JSON deserialization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeserializeError {
    /// Error message
    pub message: String,
}

impl Value {
    /// Creates a null JSON value.
    pub fn null() -> Self {
        Value::Null
    }

    /// Creates a boolean JSON value.
    pub fn bool(v: bool) -> Self {
        Value::Bool(v)
    }

    /// Creates a number JSON value from an integer.
    pub fn int(v: i64) -> Self {
        Value::Number(Number::Int(v))
    }

    /// Creates a number JSON value from a float.
    pub fn float(v: f64) -> Self {
        Value::Number(Number::Float(v))
    }

    /// Creates a string JSON value.
    pub fn string(s: impl Into<String>) -> Self {
        Value::String(s.into())
    }

    /// Creates an array JSON value.
    pub fn array(arr: Vec<Value>) -> Self {
        Value::Array(arr)
    }

    /// Creates an object JSON value.
    pub fn object(obj: HashMap<String, Value>) -> Self {
        Value::Object(obj)
    }

    /// Returns true if this value is null.
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    /// Returns true if this value is a boolean.
    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    /// Returns true if this value is a number.
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    /// Returns true if this value is a string.
    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    /// Returns true if this value is an array.
    pub fn is_array(&self) -> bool {
        matches!(self, Value::Array(_))
    }

    /// Returns true if this value is an object.
    pub fn is_object(&self) -> bool {
        matches!(self, Value::Object(_))
    }

    /// Gets a value by key from an object.
    pub fn get(&self, key: &str) -> Option<&Value> {
        match self {
            Value::Object(obj) => obj.get(key),
            _ => None,
        }
    }

    /// Gets a value by index from an array.
    pub fn get_index(&self, index: usize) -> Option<&Value> {
        match self {
            Value::Array(arr) => arr.get(index),
            _ => None,
        }
    }

    /// Returns the boolean value if this is a boolean.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Returns the integer value if this is an integer number.
    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Number(Number::Int(n)) => Some(*n),
            Value::Number(Number::Float(n)) => Some(*n as i64),
            _ => None,
        }
    }

    /// Returns the float value if this is a number.
    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Number(Number::Int(n)) => Some(*n as f64),
            Value::Number(Number::Float(n)) => Some(*n),
            _ => None,
        }
    }

    /// Returns the string value if this is a string.
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Returns the array if this is an array.
    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match self {
            Value::Array(arr) => Some(arr),
            _ => None,
        }
    }

    /// Returns the mutable array if this is an array.
    pub fn as_array_mut(&mut self) -> Option<&mut Vec<Value>> {
        match self {
            Value::Array(arr) => Some(arr),
            _ => None,
        }
    }

    /// Returns the object if this is an object.
    pub fn as_object(&self) -> Option<&HashMap<String, Value>> {
        match self {
            Value::Object(obj) => Some(obj),
            _ => None,
        }
    }

    /// Returns the mutable object if this is an object.
    pub fn as_object_mut(&mut self) -> Option<&mut HashMap<String, Value>> {
        match self {
            Value::Object(obj) => Some(obj),
            _ => None,
        }
    }
}

impl Number {
    /// Returns true if this number is an integer.
    pub fn is_int(&self) -> bool {
        matches!(self, Number::Int(_))
    }

    /// Returns true if this number is a float.
    pub fn is_float(&self) -> bool {
        matches!(self, Number::Float(_))
    }

    /// Returns the value as an integer.
    pub fn as_int(&self) -> i64 {
        match self {
            Number::Int(n) => *n,
            Number::Float(n) => *n as i64,
        }
    }

    /// Returns the value as a float.
    pub fn as_float(&self) -> f64 {
        match self {
            Number::Int(n) => *n as f64,
            Number::Float(n) => *n,
        }
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Int(n) => write!(f, "{}", n),
            Number::Float(n) => {
                if n.is_nan() {
                    write!(f, "null")
                } else if n.is_infinite() {
                    if n.is_sign_negative() {
                        write!(f, "null")
                    } else {
                        write!(f, "null")
                    }
                } else {
                    write!(f, "{}", n)
                }
            }
        }
    }
}

/// Parses a JSON string into a Value.
pub fn from_str(s: &str) -> Result<Value, ParseError> {
    let mut parser = Parser::new(s);
    parser.parse()
}

/// Parses a JSON byte slice into a Value.
pub fn from_slice(v: &[u8]) -> Result<Value, ParseError> {
    let s = std::str::from_utf8(v).map_err(|_| ParseError {
        message: "Invalid UTF-8".to_string(),
        position: 0,
    })?;
    from_str(s)
}

/// Serializes a Value to a JSON string.
pub fn to_string(value: &Value) -> Result<String, SerializeError> {
    let mut serializer = Serializer::new(false);
    serializer.serialize(value)?;
    Ok(serializer.output)
}

/// Serializes a Value to a pretty-printed JSON string.
pub fn to_string_pretty(value: &Value) -> Result<String, SerializeError> {
    let mut serializer = Serializer::new(true);
    serializer.serialize(value)?;
    Ok(serializer.output)
}

/// Serializes a Value to a JSON byte vector.
pub fn to_vec(value: &Value) -> Result<Vec<u8>, SerializeError> {
    to_string(value).map(|s| s.into_bytes())
}

// Parser implementation
struct Parser<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Parser { input, position: 0 }
    }

    fn parse(&mut self) -> Result<Value, ParseError> {
        self.skip_whitespace();
        let value = self.parse_value()?;
        self.skip_whitespace();
        if self.position < self.input.len() {
            return Err(self.error("Unexpected trailing characters"));
        }
        Ok(value)
    }

    fn parse_value(&mut self) -> Result<Value, ParseError> {
        self.skip_whitespace();

        if self.position >= self.input.len() {
            return Err(self.error("Unexpected end of input"));
        }

        match self.peek() {
            'n' => self.parse_null(),
            't' | 'f' => self.parse_bool(),
            '"' => self.parse_string(),
            '[' => self.parse_array(),
            '{' => self.parse_object(),
            c if c.is_ascii_digit() || c == '-' => self.parse_number(),
            c => Err(self.error(&format!("Unexpected character: {}", c))),
        }
    }

    fn parse_null(&mut self) -> Result<Value, ParseError> {
        self.expect_literal("null")?;
        Ok(Value::Null)
    }

    fn parse_bool(&mut self) -> Result<Value, ParseError> {
        if self.peek() == 't' {
            self.expect_literal("true")?;
            Ok(Value::Bool(true))
        } else {
            self.expect_literal("false")?;
            Ok(Value::Bool(false))
        }
    }

    fn parse_number(&mut self) -> Result<Value, ParseError> {
        let start = self.position;

        // Optional minus sign
        if self.peek() == '-' {
            self.advance();
        }

        // Integer part
        if self.peek() == '0' {
            self.advance();
        } else if self.peek().is_ascii_digit() {
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        } else {
            return Err(self.error("Expected digit"));
        }

        // Fractional part
        if self.peek() == '.' {
            self.advance();
            if !self.peek().is_ascii_digit() {
                return Err(self.error("Expected digit after decimal point"));
            }
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        // Exponent part
        if self.peek() == 'e' || self.peek() == 'E' {
            self.advance();
            if self.peek() == '+' || self.peek() == '-' {
                self.advance();
            }
            if !self.peek().is_ascii_digit() {
                return Err(self.error("Expected digit in exponent"));
            }
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let num_str = &self.input[start..self.position];

        // Try to parse as integer first
        if let Ok(n) = num_str.parse::<i64>() {
            Ok(Value::Number(Number::Int(n)))
        } else if let Ok(n) = num_str.parse::<f64>() {
            Ok(Value::Number(Number::Float(n)))
        } else {
            Err(self.error("Invalid number format"))
        }
    }

    fn parse_string(&mut self) -> Result<Value, ParseError> {
        self.expect('"')?;
        let mut result = String::new();

        while self.peek() != '"' {
            if self.position >= self.input.len() {
                return Err(self.error("Unterminated string"));
            }

            let c = self.advance();
            if c == '\\' {
                let escaped = self.advance();
                match escaped {
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    '/' => result.push('/'),
                    'b' => result.push('\x08'),
                    'f' => result.push('\x0C'),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    'u' => {
                        let hex = self.take(4);
                        if hex.len() != 4 {
                            return Err(self.error("Invalid unicode escape"));
                        }
                        let code = u32::from_str_radix(hex, 16)
                            .map_err(|_| self.error("Invalid unicode escape"))?;
                        let ch = std::char::from_u32(code)
                            .ok_or_else(|| self.error("Invalid unicode code point"))?;
                        result.push(ch);
                    }
                    _ => return Err(self.error("Invalid escape sequence")),
                }
            } else if c.is_control() {
                return Err(self.error("Control character in string"));
            } else {
                result.push(c);
            }
        }

        self.expect('"')?;
        Ok(Value::String(result))
    }

    fn parse_array(&mut self) -> Result<Value, ParseError> {
        self.expect('[')?;
        self.skip_whitespace();

        let mut arr = Vec::new();

        if self.peek() == ']' {
            self.advance();
            return Ok(Value::Array(arr));
        }

        loop {
            let value = self.parse_value()?;
            arr.push(value);

            self.skip_whitespace();

            match self.peek() {
                ',' => {
                    self.advance();
                    self.skip_whitespace();
                }
                ']' => {
                    self.advance();
                    break;
                }
                _ => return Err(self.error("Expected ',' or ']' in array")),
            }
        }

        Ok(Value::Array(arr))
    }

    fn parse_object(&mut self) -> Result<Value, ParseError> {
        self.expect('{')?;
        self.skip_whitespace();

        let mut obj = HashMap::new();

        if self.peek() == '}' {
            self.advance();
            return Ok(Value::Object(obj));
        }

        loop {
            self.skip_whitespace();

            // Parse key
            if self.peek() != '"' {
                return Err(self.error("Expected string key in object"));
            }

            let key_value = self.parse_string()?;
            let key = match key_value {
                Value::String(s) => s,
                _ => unreachable!(),
            };

            self.skip_whitespace();
            self.expect(':')?;
            self.skip_whitespace();

            let value = self.parse_value()?;
            obj.insert(key, value);

            self.skip_whitespace();

            match self.peek() {
                ',' => {
                    self.advance();
                    self.skip_whitespace();
                }
                '}' => {
                    self.advance();
                    break;
                }
                _ => return Err(self.error("Expected ',' or '}' in object")),
            }
        }

        Ok(Value::Object(obj))
    }

    fn skip_whitespace(&mut self) {
        while self.position < self.input.len() {
            match self.input.chars().nth(self.position) {
                Some(c) if c.is_whitespace() => self.position += 1,
                _ => break,
            }
        }
    }

    fn peek(&self) -> char {
        self.input.chars().nth(self.position).unwrap_or('\0')
    }

    fn advance(&mut self) -> char {
        let c = self.peek();
        self.position += 1;
        c
    }

    fn take(&mut self, n: usize) -> &str {
        let start = self.position;
        let end = (self.position + n).min(self.input.len());
        self.position = end;
        &self.input[start..end]
    }

    fn expect(&mut self, expected: char) -> Result<(), ParseError> {
        if self.peek() == expected {
            self.advance();
            Ok(())
        } else {
            Err(self.error(&format!("Expected '{}'", expected)))
        }
    }

    fn expect_literal(&mut self, literal: &str) -> Result<(), ParseError> {
        if self.input[self.position..].starts_with(literal) {
            self.position += literal.len();
            Ok(())
        } else {
            Err(self.error(&format!("Expected '{}'", literal)))
        }
    }

    fn error(&self, message: &str) -> ParseError {
        ParseError {
            message: message.to_string(),
            position: self.position,
        }
    }
}

// Serializer implementation
struct Serializer {
    output: String,
    pretty: bool,
    indent_level: usize,
}

impl Serializer {
    fn new(pretty: bool) -> Self {
        Serializer {
            output: String::new(),
            pretty,
            indent_level: 0,
        }
    }

    fn serialize(&mut self, value: &Value) -> Result<(), SerializeError> {
        match value {
            Value::Null => self.output.push_str("null"),
            Value::Bool(b) => self.output.push_str(if *b { "true" } else { "false" }),
            Value::Number(n) => self.output.push_str(&n.to_string()),
            Value::String(s) => self.serialize_string(s)?,
            Value::Array(arr) => self.serialize_array(arr)?,
            Value::Object(obj) => self.serialize_object(obj)?,
        }
        Ok(())
    }

    fn serialize_string(&mut self, s: &str) -> Result<(), SerializeError> {
        self.output.push('"');
        for c in s.chars() {
            match c {
                '"' => self.output.push_str("\\\""),
                '\\' => self.output.push_str("\\\\"),
                '\x08' => self.output.push_str("\\b"),
                '\x0C' => self.output.push_str("\\f"),
                '\n' => self.output.push_str("\\n"),
                '\r' => self.output.push_str("\\r"),
                '\t' => self.output.push_str("\\t"),
                c if c.is_control() => {
                    self.output.push_str(&format!("\\u{:04x}", c as u32));
                }
                c => self.output.push(c),
            }
        }
        self.output.push('"');
        Ok(())
    }

    fn serialize_array(&mut self, arr: &[Value]) -> Result<(), SerializeError> {
        self.output.push('[');

        if self.pretty && !arr.is_empty() {
            self.indent_level += 1;
            self.output.push('\n');
            self.write_indent();
        }

        for (i, value) in arr.iter().enumerate() {
            self.serialize(value)?;

            if i < arr.len() - 1 {
                self.output.push(',');
                if self.pretty {
                    self.output.push('\n');
                    self.write_indent();
                }
            }
        }

        if self.pretty && !arr.is_empty() {
            self.indent_level -= 1;
            self.output.push('\n');
            self.write_indent();
        }

        self.output.push(']');
        Ok(())
    }

    fn serialize_object(&mut self, obj: &HashMap<String, Value>) -> Result<(), SerializeError> {
        self.output.push('{');

        let mut entries: Vec<_> = obj.iter().collect();
        entries.sort_by(|a, b| a.0.cmp(b.0)); // Sort for deterministic output

        if self.pretty && !entries.is_empty() {
            self.indent_level += 1;
            self.output.push('\n');
            self.write_indent();
        }

        for (i, (key, value)) in entries.iter().enumerate() {
            self.serialize_string(key)?;

            if self.pretty {
                self.output.push_str(": ");
            } else {
                self.output.push(':');
            }

            self.serialize(value)?;

            if i < entries.len() - 1 {
                self.output.push(',');
                if self.pretty {
                    self.output.push('\n');
                    self.write_indent();
                }
            }
        }

        if self.pretty && !entries.is_empty() {
            self.indent_level -= 1;
            self.output.push('\n');
            self.write_indent();
        }

        self.output.push('}');
        Ok(())
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.output.push_str("  ");
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "JSON parse error at position {}: {}",
            self.position, self.message
        )
    }
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for SerializeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "JSON serialize error: {}", self.message)
    }
}

impl std::error::Error for SerializeError {}

impl std::fmt::Display for DeserializeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "JSON deserialize error: {}", self.message)
    }
}

impl std::error::Error for DeserializeError {}

// C ABI exports for FFI

use crate::string::JetString;

/// Creates a null JSON value.
#[no_mangle]
pub extern "C" fn jet_json_value_null() -> *mut Value {
    Box::into_raw(Box::new(Value::Null))
}

/// Creates a boolean JSON value.
#[no_mangle]
pub extern "C" fn jet_json_value_bool(v: bool) -> *mut Value {
    Box::into_raw(Box::new(Value::Bool(v)))
}

/// Creates an integer JSON value.
#[no_mangle]
pub extern "C" fn jet_json_value_int(v: i64) -> *mut Value {
    Box::into_raw(Box::new(Value::Number(Number::Int(v))))
}

/// Creates a float JSON value.
#[no_mangle]
pub extern "C" fn jet_json_value_float(v: f64) -> *mut Value {
    Box::into_raw(Box::new(Value::Number(Number::Float(v))))
}

/// Creates a string JSON value from a byte slice.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer and length according to `jet_json_value_string`'s FFI contract.
pub unsafe extern "C" fn jet_json_value_string(s: *const u8, len: usize) -> *mut Value {
    if s.is_null() {
        return Box::into_raw(Box::new(Value::String(String::new())));
    }
    let bytes = unsafe { std::slice::from_raw_parts(s, len) };
    let string = String::from_utf8_lossy(bytes);
    Box::into_raw(Box::new(Value::String(string.into_owned())))
}

/// Creates an empty array JSON value.
#[no_mangle]
pub extern "C" fn jet_json_value_array() -> *mut Value {
    Box::into_raw(Box::new(Value::Array(Vec::new())))
}

/// Creates an empty object JSON value.
#[no_mangle]
pub extern "C" fn jet_json_value_object() -> *mut Value {
    Box::into_raw(Box::new(Value::Object(HashMap::new())))
}

/// Parses a JSON string into a Value.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer and length according to `jet_json_from_str`'s FFI contract.
pub unsafe extern "C" fn jet_json_from_str(s: *const u8, len: usize) -> *mut Value {
    if s.is_null() {
        return std::ptr::null_mut();
    }
    let bytes = unsafe { std::slice::from_raw_parts(s, len) };
    match std::str::from_utf8(bytes) {
        Ok(str) => match from_str(str) {
            Ok(value) => Box::into_raw(Box::new(value)),
            Err(_) => std::ptr::null_mut(),
        },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Serializes a Value to a JSON string.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_to_string`'s FFI contract.
pub unsafe extern "C" fn jet_json_to_string(v: *const Value) -> *mut JetString {
    if v.is_null() {
        return std::ptr::null_mut();
    }
    let value = unsafe { &*v };
    match to_string(value) {
        Ok(s) => Box::into_raw(Box::new(JetString::from_str(&s))),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Serializes a Value to a pretty-printed JSON string.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_to_string_pretty`'s FFI contract.
pub unsafe extern "C" fn jet_json_to_string_pretty(v: *const Value) -> *mut JetString {
    if v.is_null() {
        return std::ptr::null_mut();
    }
    let value = unsafe { &*v };
    match to_string_pretty(value) {
        Ok(s) => Box::into_raw(Box::new(JetString::from_str(&s))),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Pushes a value onto an array.
#[no_mangle]
/// # Safety
/// The caller must pass valid pointers according to `jet_json_array_push`'s FFI contract.
pub unsafe extern "C" fn jet_json_array_push(arr: *mut Value, val: *mut Value) {
    if arr.is_null() || val.is_null() {
        return;
    }
    let array = unsafe { &mut *arr };
    let value = unsafe { *Box::from_raw(val) };
    if let Value::Array(ref mut vec) = array {
        vec.push(value);
    }
}

/// Gets a value from an array by index.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_array_get`'s FFI contract.
pub unsafe extern "C" fn jet_json_array_get(arr: *const Value, idx: usize) -> *mut Value {
    if arr.is_null() {
        return std::ptr::null_mut();
    }
    let array = unsafe { &*arr };
    if let Value::Array(ref vec) = array {
        vec.get(idx)
            .map(|v| Box::into_raw(Box::new(v.clone())))
            .unwrap_or(std::ptr::null_mut())
    } else {
        std::ptr::null_mut()
    }
}

/// Returns the length of an array.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_array_len`'s FFI contract.
pub unsafe extern "C" fn jet_json_array_len(arr: *const Value) -> usize {
    if arr.is_null() {
        return 0;
    }
    let array = unsafe { &*arr };
    if let Value::Array(ref vec) = array {
        vec.len()
    } else {
        0
    }
}

/// Inserts a key-value pair into an object.
#[no_mangle]
/// # Safety
/// The caller must pass valid pointers according to `jet_json_object_insert`'s FFI contract.
pub unsafe extern "C" fn jet_json_object_insert(
    obj: *mut Value,
    key: *const u8,
    key_len: usize,
    val: *mut Value,
) {
    if obj.is_null() || key.is_null() || val.is_null() {
        return;
    }
    let object = unsafe { &mut *obj };
    let key_bytes = unsafe { std::slice::from_raw_parts(key, key_len) };
    let key_string = String::from_utf8_lossy(key_bytes).into_owned();
    let value = unsafe { *Box::from_raw(val) };
    if let Value::Object(ref mut map) = object {
        map.insert(key_string, value);
    }
}

/// Gets a value from an object by key.
#[no_mangle]
/// # Safety
/// The caller must pass valid pointers according to `jet_json_object_get`'s FFI contract.
pub unsafe extern "C" fn jet_json_object_get(
    obj: *const Value,
    key: *const u8,
    key_len: usize,
) -> *mut Value {
    if obj.is_null() || key.is_null() {
        return std::ptr::null_mut();
    }
    let object = unsafe { &*obj };
    let key_bytes = unsafe { std::slice::from_raw_parts(key, key_len) };
    let key_string = String::from_utf8_lossy(key_bytes);
    if let Value::Object(ref map) = object {
        map.get(key_string.as_ref())
            .map(|v| Box::into_raw(Box::new(v.clone())))
            .unwrap_or(std::ptr::null_mut())
    } else {
        std::ptr::null_mut()
    }
}

/// Frees a JSON value.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_value_free`'s FFI contract.
pub unsafe extern "C" fn jet_json_value_free(v: *mut Value) {
    if !v.is_null() {
        unsafe { drop(Box::from_raw(v)) };
    }
}

/// Returns true if the value is null.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_is_null`'s FFI contract.
pub unsafe extern "C" fn jet_json_is_null(v: *const Value) -> bool {
    if v.is_null() {
        return false;
    }
    let value = unsafe { &*v };
    value.is_null()
}

/// Returns true if the value is a boolean.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_is_bool`'s FFI contract.
pub unsafe extern "C" fn jet_json_is_bool(v: *const Value) -> bool {
    if v.is_null() {
        return false;
    }
    let value = unsafe { &*v };
    value.is_bool()
}

/// Returns true if the value is a number.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_is_number`'s FFI contract.
pub unsafe extern "C" fn jet_json_is_number(v: *const Value) -> bool {
    if v.is_null() {
        return false;
    }
    let value = unsafe { &*v };
    value.is_number()
}

/// Returns true if the value is a string.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_is_string`'s FFI contract.
pub unsafe extern "C" fn jet_json_is_string(v: *const Value) -> bool {
    if v.is_null() {
        return false;
    }
    let value = unsafe { &*v };
    value.is_string()
}

/// Returns true if the value is an array.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_is_array`'s FFI contract.
pub unsafe extern "C" fn jet_json_is_array(v: *const Value) -> bool {
    if v.is_null() {
        return false;
    }
    let value = unsafe { &*v };
    value.is_array()
}

/// Returns true if the value is an object.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_is_object`'s FFI contract.
pub unsafe extern "C" fn jet_json_is_object(v: *const Value) -> bool {
    if v.is_null() {
        return false;
    }
    let value = unsafe { &*v };
    value.is_object()
}

/// Returns the boolean value if this is a boolean.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_as_bool`'s FFI contract.
pub unsafe extern "C" fn jet_json_as_bool(v: *const Value) -> bool {
    if v.is_null() {
        return false;
    }
    let value = unsafe { &*v };
    value.as_bool().unwrap_or(false)
}

/// Returns the integer value if this is a number.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_as_int`'s FFI contract.
pub unsafe extern "C" fn jet_json_as_int(v: *const Value) -> i64 {
    if v.is_null() {
        return 0;
    }
    let value = unsafe { &*v };
    value.as_int().unwrap_or(0)
}

/// Returns the float value if this is a number.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_as_float`'s FFI contract.
pub unsafe extern "C" fn jet_json_as_float(v: *const Value) -> f64 {
    if v.is_null() {
        return 0.0;
    }
    let value = unsafe { &*v };
    value.as_float().unwrap_or(0.0)
}

/// Returns the string value as a JetString if this is a string.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_json_as_string`'s FFI contract.
pub unsafe extern "C" fn jet_json_as_string(v: *const Value) -> *mut JetString {
    if v.is_null() {
        return std::ptr::null_mut();
    }
    let value = unsafe { &*v };
    match value.as_string() {
        Some(s) => Box::into_raw(Box::new(JetString::from_str(s))),
        None => std::ptr::null_mut(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_null() {
        assert_eq!(from_str("null").unwrap(), Value::Null);
    }

    #[test]
    fn test_parse_bool() {
        assert_eq!(from_str("true").unwrap(), Value::Bool(true));
        assert_eq!(from_str("false").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_parse_number() {
        assert_eq!(from_str("42").unwrap(), Value::Number(Number::Int(42)));
        assert_eq!(from_str("-17").unwrap(), Value::Number(Number::Int(-17)));
        assert_eq!(
            from_str("3.14").unwrap(),
            Value::Number(Number::Float(3.14))
        );
        assert_eq!(
            from_str("-0.5").unwrap(),
            Value::Number(Number::Float(-0.5))
        );
        assert_eq!(
            from_str("1e10").unwrap(),
            Value::Number(Number::Float(1e10))
        );
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(
            from_str("\"hello\"").unwrap(),
            Value::String("hello".to_string())
        );
        assert_eq!(
            from_str("\"hello\\nworld\"").unwrap(),
            Value::String("hello\nworld".to_string())
        );
    }

    #[test]
    fn test_parse_array() {
        let arr = from_str("[1, 2, 3]").unwrap();
        assert!(arr.is_array());
        let arr = arr.as_array().unwrap();
        assert_eq!(arr.len(), 3);
        assert_eq!(arr[0], Value::Number(Number::Int(1)));
    }

    #[test]
    fn test_parse_object() {
        let obj = from_str(r#"{"name": "Alice", "age": 30}"#).unwrap();
        assert!(obj.is_object());
        let obj = obj.as_object().unwrap();
        assert_eq!(obj.get("name"), Some(&Value::String("Alice".to_string())));
        assert_eq!(obj.get("age"), Some(&Value::Number(Number::Int(30))));
    }

    #[test]
    fn test_serialize_null() {
        assert_eq!(to_string(&Value::Null).unwrap(), "null");
    }

    #[test]
    fn test_serialize_bool() {
        assert_eq!(to_string(&Value::Bool(true)).unwrap(), "true");
        assert_eq!(to_string(&Value::Bool(false)).unwrap(), "false");
    }

    #[test]
    fn test_serialize_number() {
        assert_eq!(to_string(&Value::Number(Number::Int(42))).unwrap(), "42");
        assert_eq!(
            to_string(&Value::Number(Number::Float(3.14))).unwrap(),
            "3.14"
        );
    }

    #[test]
    fn test_serialize_string() {
        assert_eq!(
            to_string(&Value::String("hello".to_string())).unwrap(),
            "\"hello\""
        );
    }

    #[test]
    fn test_serialize_array() {
        let arr = Value::Array(Vec::from([
            Value::Number(Number::Int(1)),
            Value::Number(Number::Int(2)),
        ]));
        assert_eq!(to_string(&arr).unwrap(), "[1,2]");
    }

    #[test]
    fn test_serialize_object() {
        let mut obj = HashMap::new();
        obj.insert("key".to_string(), Value::String("value".to_string()));
        let value = Value::Object(obj);
        assert_eq!(to_string(&value).unwrap(), "{\"key\":\"value\"}");
    }

    #[test]
    fn test_roundtrip() {
        let json = r#"{"name":"Alice","age":30,"scores":[85,92,78]}"#;
        let value = from_str(json).unwrap();
        let serialized = to_string(&value).unwrap();
        let reparsed = from_str(&serialized).unwrap();
        assert_eq!(value, reparsed);
    }

    #[test]
    fn test_pretty_print() {
        let mut obj = HashMap::new();
        obj.insert("a".to_string(), Value::Number(Number::Int(1)));
        let value = Value::Object(obj);
        let pretty = to_string_pretty(&value).unwrap();
        assert!(pretty.contains('\n'));
        assert!(pretty.contains("  "));
    }

    #[test]
    fn test_value_accessors() {
        let value = from_str(r#"{"nested": {"key": "value"}}"#).unwrap();
        assert!(value.get("nested").is_some());
        assert!(value.get("missing").is_none());

        let arr = from_str("[10, 20, 30]").unwrap();
        assert_eq!(arr.get_index(0), Some(&Value::Number(Number::Int(10))));
        assert_eq!(arr.get_index(10), None);
    }

    #[test]
    fn test_number_conversions() {
        let int_num = Number::Int(42);
        assert_eq!(int_num.as_int(), 42);
        assert_eq!(int_num.as_float(), 42.0);

        let float_num = Number::Float(3.14);
        assert_eq!(float_num.as_int(), 3);
        assert!((float_num.as_float() - 3.14).abs() < 0.001);
    }

    // FFI tests

    #[test]
    fn test_ffi_value_null() {
        let ptr = jet_json_value_null();
        assert!(!ptr.is_null());
        unsafe {
            assert!((*ptr).is_null());
            jet_json_value_free(ptr);
        }
    }

    #[test]
    fn test_ffi_value_bool() {
        let ptr = jet_json_value_bool(true);
        assert!(!ptr.is_null());
        unsafe {
            assert!((*ptr).as_bool().unwrap());
            assert!(jet_json_is_bool(ptr));
            assert!(jet_json_as_bool(ptr));
            jet_json_value_free(ptr);
        }
    }

    #[test]
    fn test_ffi_value_int() {
        let ptr = jet_json_value_int(42);
        assert!(!ptr.is_null());
        unsafe {
            assert_eq!((*ptr).as_int().unwrap(), 42);
            assert!(jet_json_is_number(ptr));
            assert_eq!(jet_json_as_int(ptr), 42);
            jet_json_value_free(ptr);
        }
    }

    #[test]
    fn test_ffi_value_float() {
        let ptr = jet_json_value_float(3.14);
        assert!(!ptr.is_null());
        unsafe {
            assert!((*ptr).as_float().unwrap() - 3.14 < 0.001);
            assert!(jet_json_is_number(ptr));
            assert!((jet_json_as_float(ptr) - 3.14).abs() < 0.001);
            jet_json_value_free(ptr);
        }
    }

    #[test]
    fn test_ffi_value_string() {
        let s = b"hello";
        let ptr = unsafe { jet_json_value_string(s.as_ptr(), s.len()) };
        assert!(!ptr.is_null());
        unsafe {
            assert!(jet_json_is_string(ptr));
            let jet_str = jet_json_as_string(ptr);
            assert!(!jet_str.is_null());
            assert_eq!((*jet_str).len(), 5);
            crate::string::jet_string_free(jet_str);
            jet_json_value_free(ptr);
        }
    }

    #[test]
    fn test_ffi_value_array() {
        let ptr = jet_json_value_array();
        assert!(!ptr.is_null());
        unsafe {
            assert!(jet_json_is_array(ptr));
            assert_eq!(jet_json_array_len(ptr), 0);
            jet_json_value_free(ptr);
        }
    }

    #[test]
    fn test_ffi_value_object() {
        let ptr = jet_json_value_object();
        assert!(!ptr.is_null());
        unsafe {
            assert!(jet_json_is_object(ptr));
            jet_json_value_free(ptr);
        }
    }

    #[test]
    fn test_ffi_parse() {
        let json = b"{\"name\":\"test\",\"value\":42}";
        let ptr = unsafe { jet_json_from_str(json.as_ptr(), json.len()) };
        assert!(!ptr.is_null());
        unsafe {
            assert!(jet_json_is_object(ptr));
            jet_json_value_free(ptr);
        }
    }

    #[test]
    fn test_ffi_parse_invalid() {
        let json = b"invalid";
        let ptr = unsafe { jet_json_from_str(json.as_ptr(), json.len()) };
        assert!(ptr.is_null());
    }

    #[test]
    fn test_ffi_to_string() {
        let ptr = jet_json_value_int(42);
        unsafe {
            let str_ptr = jet_json_to_string(ptr);
            assert!(!str_ptr.is_null());
            crate::string::jet_string_free(str_ptr);
            jet_json_value_free(ptr);
        }
    }

    #[test]
    fn test_ffi_array_operations() {
        let arr = jet_json_value_array();
        let val1 = jet_json_value_int(1);
        let val2 = jet_json_value_int(2);

        unsafe {
            jet_json_array_push(arr, val1);
            jet_json_array_push(arr, val2);

            assert_eq!(jet_json_array_len(arr), 2);

            let got = jet_json_array_get(arr, 0);
            assert!(!got.is_null());
            assert_eq!(jet_json_as_int(got), 1);
            jet_json_value_free(got);

            let got2 = jet_json_array_get(arr, 1);
            assert!(!got2.is_null());
            assert_eq!(jet_json_as_int(got2), 2);
            jet_json_value_free(got2);

            jet_json_value_free(arr);
        }
    }

    #[test]
    fn test_ffi_object_operations() {
        let obj = jet_json_value_object();
        let key = b"key";
        let val = unsafe { jet_json_value_string(b"value".as_ptr(), 5) };

        unsafe {
            jet_json_object_insert(obj, key.as_ptr(), key.len(), val);

            let got = jet_json_object_get(obj, key.as_ptr(), key.len());
            assert!(!got.is_null());
            assert!(jet_json_is_string(got));
            jet_json_value_free(got);

            jet_json_value_free(obj);
        }
    }

    #[test]
    fn test_ffi_null_safety() {
        unsafe {
            // All functions should handle null gracefully
            assert!(!jet_json_is_null(std::ptr::null()));
            assert!(!jet_json_is_bool(std::ptr::null()));
            assert!(!jet_json_is_number(std::ptr::null()));
            assert!(!jet_json_is_string(std::ptr::null()));
            assert!(!jet_json_is_array(std::ptr::null()));
            assert!(!jet_json_is_object(std::ptr::null()));
            assert!(!jet_json_as_bool(std::ptr::null()));
            assert_eq!(jet_json_as_int(std::ptr::null()), 0);
            assert_eq!(jet_json_as_float(std::ptr::null()), 0.0);
            assert!(jet_json_as_string(std::ptr::null()).is_null());
            assert_eq!(jet_json_array_len(std::ptr::null()), 0);
            assert!(jet_json_array_get(std::ptr::null(), 0).is_null());
            assert!(jet_json_object_get(std::ptr::null(), b"x".as_ptr(), 1).is_null());
            jet_json_value_free(std::ptr::null_mut()); // Should not panic
        }
    }
}
