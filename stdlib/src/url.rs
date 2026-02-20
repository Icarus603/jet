//! URL - URL parsing and encoding for Jet
//!
//! This module provides URL parsing, encoding, and manipulation
//! for the Jet programming language.

#![allow(clippy::result_unit_err)]
#![allow(clippy::type_complexity)]

use std::fmt;

/// A parsed URL.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Url {
    scheme: String,
    username: Option<String>,
    password: Option<String>,
    host: Option<String>,
    port: Option<u16>,
    path: String,
    query: Option<String>,
    fragment: Option<String>,
}

/// Error type for URL parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    message: String,
}

/// Error type for URL decoding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecodeError {
    message: String,
}

/// Iterator over query string key-value pairs.
pub struct QueryPairs<'a> {
    query: &'a str,
    position: usize,
}

impl Url {
    /// Parses a URL string.
    pub fn parse(input: impl AsRef<str>) -> Result<Self, ParseError> {
        let input = input.as_ref();
        Parser::new(input).parse()
    }

    /// Joins a relative URL to this URL.
    pub fn join(&self, input: impl AsRef<str>) -> Result<Self, ParseError> {
        let input = input.as_ref();

        // If input is absolute, just parse it
        if input.contains("://") {
            return Self::parse(input);
        }

        // Handle fragment-only
        if input.starts_with('#') {
            let mut result = self.clone();
            result.fragment = Some(input[1..].to_string());
            return Ok(result);
        }

        // Handle query-only
        if input.starts_with('?') {
            let mut result = self.clone();
            result.query = Some(input[1..].to_string());
            return Ok(result);
        }

        // Handle absolute path
        if input.starts_with('/') {
            let mut result = self.clone();
            result.path = input.to_string();
            result.query = None;
            result.fragment = None;
            return Ok(result);
        }

        // Handle relative path
        let mut result = self.clone();
        let base_path = if self.path.contains('/') {
            let last_slash = self.path.rfind('/').unwrap();
            &self.path[..last_slash + 1]
        } else {
            "/"
        };

        result.path = resolve_relative_path(base_path, input);
        result.query = None;
        result.fragment = None;

        Ok(result)
    }

    /// Returns the scheme.
    pub fn scheme(&self) -> &str {
        &self.scheme
    }

    /// Returns the host, if any.
    pub fn host(&self) -> Option<&str> {
        self.host.as_deref()
    }

    /// Returns the port, if any.
    pub fn port(&self) -> Option<u16> {
        self.port
    }

    /// Returns the path.
    pub fn path(&self) -> &str {
        &self.path
    }

    /// Returns the query string, if any.
    pub fn query(&self) -> Option<&str> {
        self.query.as_deref()
    }

    /// Returns the fragment, if any.
    pub fn fragment(&self) -> Option<&str> {
        self.fragment.as_deref()
    }

    /// Returns the username, if any.
    pub fn username(&self) -> Option<&str> {
        self.username.as_deref()
    }

    /// Returns the password, if any.
    pub fn password(&self) -> Option<&str> {
        self.password.as_deref()
    }

    /// Sets the scheme.
    pub fn set_scheme(&mut self, scheme: &str) -> Result<(), ()> {
        if scheme.is_empty() || scheme.chars().any(|c| !is_scheme_char(c)) {
            return Err(());
        }
        self.scheme = scheme.to_lowercase();
        Ok(())
    }

    /// Sets the host.
    pub fn set_host(&mut self, host: Option<&str>) {
        self.host = host.map(|h| h.to_lowercase());
    }

    /// Sets the port.
    pub fn set_port(&mut self, port: Option<u16>) {
        self.port = port;
    }

    /// Sets the path.
    pub fn set_path(&mut self, path: &str) {
        self.path = if path.starts_with('/') {
            path.to_string()
        } else {
            format!("/{}", path)
        };
    }

    /// Sets the query string.
    pub fn set_query(&mut self, query: Option<&str>) {
        self.query = query.map(|q| q.to_string());
    }

    /// Sets the fragment.
    pub fn set_fragment(&mut self, fragment: Option<&str>) {
        self.fragment = fragment.map(|f| f.to_string());
    }

    /// Returns an iterator over query string pairs.
    pub fn query_pairs(&self) -> QueryPairs<'_> {
        QueryPairs {
            query: self.query.as_deref().unwrap_or(""),
            position: 0,
        }
    }

    /// Returns the URL as a string.
    pub fn as_str(&self) -> String {
        let mut result = String::new();

        // Scheme
        result.push_str(&self.scheme);
        result.push(':');

        // Authority
        if self.host.is_some() || self.username.is_some() {
            result.push_str("//");

            if let Some(ref username) = self.username {
                result.push_str(&encode_component(username));

                if let Some(ref password) = self.password {
                    result.push(':');
                    result.push_str(&encode_component(password));
                }

                result.push('@');
            }

            if let Some(ref host) = self.host {
                result.push_str(host);
            }

            if let Some(port) = self.port {
                result.push(':');
                result.push_str(&port.to_string());
            }
        }

        // Path
        result.push_str(&self.path);

        // Query
        if let Some(ref query) = self.query {
            result.push('?');
            result.push_str(query);
        }

        // Fragment
        if let Some(ref fragment) = self.fragment {
            result.push('#');
            result.push_str(&encode_component(fragment));
        }

        result
    }
}

impl fmt::Display for Url {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl ParseError {
    /// Creates a new parse error.
    pub fn new(message: impl Into<String>) -> Self {
        ParseError {
            message: message.into(),
        }
    }

    /// Returns the error message.
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "URL parse error: {}", self.message)
    }
}

impl std::error::Error for ParseError {}

impl DecodeError {
    /// Creates a new decode error.
    pub fn new(message: impl Into<String>) -> Self {
        DecodeError {
            message: message.into(),
        }
    }

    /// Returns the error message.
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "URL decode error: {}", self.message)
    }
}

impl std::error::Error for DecodeError {}

impl<'a> Iterator for QueryPairs<'a> {
    type Item = (String, String);

    fn next(&mut self) -> Option<Self::Item> {
        if self.position >= self.query.len() {
            return None;
        }

        let remaining = &self.query[self.position..];
        let (pair, rest) = if let Some(amp) = remaining.find('&') {
            (&remaining[..amp], &remaining[amp + 1..])
        } else {
            (remaining, "")
        };

        self.position = self.query.len() - rest.len();

        if let Some(eq) = pair.find('=') {
            let key = decode_component(&pair[..eq]).ok()?;
            let value = decode_component(&pair[eq + 1..]).ok()?;
            Some((key, value))
        } else {
            let key = decode_component(pair).ok()?;
            Some((key, String::new()))
        }
    }
}

/// Encodes a string for use in form data (application/x-www-form-urlencoded).
pub fn encode(input: impl AsRef<[u8]>) -> String {
    let input = input.as_ref();
    let mut result = String::with_capacity(input.len());

    for &byte in input {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                result.push(byte as char);
            }
            b' ' => result.push('+'),
            _ => {
                result.push('%');
                result.push_str(&format!("{:02X}", byte));
            }
        }
    }

    result
}

/// Encodes a string component for use in a URL.
pub fn encode_component(input: impl AsRef<str>) -> String {
    let input = input.as_ref();
    let bytes = input.as_bytes();
    let mut result = String::with_capacity(bytes.len());

    for &byte in bytes {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                result.push(byte as char);
            }
            _ => {
                result.push('%');
                result.push_str(&format!("{:02X}", byte));
            }
        }
    }

    result
}

/// Decodes a form-encoded string.
pub fn decode(input: impl AsRef<str>) -> Result<String, DecodeError> {
    let input = input.as_ref();
    let mut result = String::with_capacity(input.len());
    let mut chars = input.chars();

    while let Some(c) = chars.next() {
        match c {
            '+' => result.push(' '),
            '%' => {
                let hex1 = chars
                    .next()
                    .ok_or_else(|| DecodeError::new("Incomplete escape sequence"))?;
                let hex2 = chars
                    .next()
                    .ok_or_else(|| DecodeError::new("Incomplete escape sequence"))?;
                let hex = format!("{}{}", hex1, hex2);
                let byte = u8::from_str_radix(&hex, 16)
                    .map_err(|_| DecodeError::new("Invalid hex sequence"))?;
                result.push(byte as char);
            }
            c => result.push(c),
        }
    }

    Ok(result)
}

/// Decodes a URL-encoded component.
pub fn decode_component(input: impl AsRef<str>) -> Result<String, DecodeError> {
    let input = input.as_ref();
    let mut result = String::with_capacity(input.len());
    let mut chars = input.chars();

    while let Some(c) = chars.next() {
        match c {
            '%' => {
                let hex1 = chars
                    .next()
                    .ok_or_else(|| DecodeError::new("Incomplete escape sequence"))?;
                let hex2 = chars
                    .next()
                    .ok_or_else(|| DecodeError::new("Incomplete escape sequence"))?;
                let hex = format!("{}{}", hex1, hex2);
                let byte = u8::from_str_radix(&hex, 16)
                    .map_err(|_| DecodeError::new("Invalid hex sequence"))?;
                result.push(byte as char);
            }
            c => result.push(c),
        }
    }

    Ok(result)
}

// URL Parser
struct Parser<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Parser { input, position: 0 }
    }

    fn parse(&mut self) -> Result<Url, ParseError> {
        // Parse scheme
        let scheme = self.parse_scheme()?;

        // Parse authority if present
        let (username, password, host, port) = if self.peek_ahead("//") {
            self.advance(2);
            self.parse_authority()?
        } else {
            (None, None, None, None)
        };

        // Parse path
        let path = self.parse_path();

        // Parse query
        let query = if self.peek() == Some('?') {
            self.advance(1);
            Some(self.parse_until(&['#']))
        } else {
            None
        };

        // Parse fragment
        let fragment = if self.peek() == Some('#') {
            self.advance(1);
            Some(self.parse_remaining())
        } else {
            None
        };

        Ok(Url {
            scheme,
            username,
            password,
            host,
            port,
            path,
            query,
            fragment,
        })
    }

    fn parse_scheme(&mut self) -> Result<String, ParseError> {
        let start = self.position;

        // First character must be a letter
        if let Some(c) = self.peek() {
            if !c.is_ascii_alphabetic() {
                return Err(ParseError::new("Scheme must start with a letter"));
            }
        } else {
            return Err(ParseError::new("Empty scheme"));
        }

        while let Some(c) = self.peek() {
            if c == ':' {
                let scheme = self.input[start..self.position].to_string();
                self.advance(1);
                return Ok(scheme.to_lowercase());
            }
            if !is_scheme_char(c) {
                break;
            }
            self.advance(1);
        }

        Err(ParseError::new("Invalid or missing scheme"))
    }

    fn parse_authority(
        &mut self,
    ) -> Result<(Option<String>, Option<String>, Option<String>, Option<u16>), ParseError> {
        let authority = self.parse_until(&['/', '?', '#']);

        if authority.is_empty() {
            return Ok((None, None, None, None));
        }

        // Parse userinfo
        let (userinfo, hostport): (Option<&str>, &str) = if let Some(at) = authority.find('@') {
            (Some(&authority[..at]), &authority[at + 1..])
        } else {
            (None, &authority)
        };

        let (username, password) = if let Some(userinfo) = userinfo {
            if let Some(colon) = userinfo.find(':') {
                (
                    Some(userinfo[..colon].to_string()),
                    Some(userinfo[colon + 1..].to_string()),
                )
            } else {
                (Some(userinfo.to_string()), None)
            }
        } else {
            (None, None)
        };

        // Parse host and port
        let (host, port) = if hostport.starts_with('[') {
            // IPv6 address
            if let Some(bracket) = hostport.find(']') {
                let host = hostport[..bracket + 1].to_string();
                let port = if hostport.len() > bracket + 2
                    && hostport.chars().nth(bracket + 1) == Some(':')
                {
                    Some(
                        hostport[bracket + 2..]
                            .parse()
                            .map_err(|_| ParseError::new("Invalid port"))?,
                    )
                } else {
                    None
                };
                (Some(host), port)
            } else {
                return Err(ParseError::new("Unclosed IPv6 address"));
            }
        } else if let Some(colon) = hostport.rfind(':') {
            let host = hostport[..colon].to_string();
            let port = hostport[colon + 1..]
                .parse()
                .map_err(|_| ParseError::new("Invalid port"))?;
            (Some(host), Some(port))
        } else {
            (Some(hostport.to_string()), None)
        };

        Ok((username, password, host, port))
    }

    fn parse_path(&mut self) -> String {
        self.parse_until(&['?', '#'])
    }

    fn parse_until(&mut self, delims: &[char]) -> String {
        let start = self.position;
        while let Some(c) = self.peek() {
            if delims.contains(&c) {
                break;
            }
            self.advance(1);
        }
        self.input[start..self.position].to_string()
    }

    fn parse_remaining(&mut self) -> String {
        let result = &self.input[self.position..];
        self.position = self.input.len();
        result.to_string()
    }

    fn peek(&self) -> Option<char> {
        self.input.chars().nth(self.position)
    }

    fn peek_ahead(&self, s: &str) -> bool {
        self.input[self.position..].starts_with(s)
    }

    fn advance(&mut self, n: usize) {
        self.position += n;
    }
}

fn is_scheme_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '+' || c == '-' || c == '.'
}

fn resolve_relative_path(base: &str, relative: &str) -> String {
    // Simple path resolution
    if relative.is_empty() {
        return base.to_string();
    }

    let mut components: Vec<&str> = base.split('/').collect();
    components.pop(); // Remove the last component

    for component in relative.split('/') {
        match component {
            "" | "." => {}
            ".." => {
                if components.len() > 1 {
                    components.pop();
                }
            }
            _ => components.push(component),
        }
    }

    components.join("/")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_parse_simple_url() {
        let url = Url::parse("https://example.com").unwrap();
        assert_eq!(url.scheme(), "https");
        assert_eq!(url.host(), Some("example.com"));
        assert_eq!(url.path(), "");
    }

    #[test]
    fn test_parse_full_url() {
        let url =
            Url::parse("https://user:pass@example.com:8080/path?query=value#fragment").unwrap();
        assert_eq!(url.scheme(), "https");
        assert_eq!(url.username(), Some("user"));
        assert_eq!(url.password(), Some("pass"));
        assert_eq!(url.host(), Some("example.com"));
        assert_eq!(url.port(), Some(8080));
        assert_eq!(url.path(), "/path");
        assert_eq!(url.query(), Some("query=value"));
        assert_eq!(url.fragment(), Some("fragment"));
    }

    #[test]
    fn test_parse_with_query() {
        let url = Url::parse("https://example.com/search?q=hello&page=1").unwrap();
        assert_eq!(url.scheme(), "https");
        assert_eq!(url.host(), Some("example.com"));
        assert_eq!(url.path(), "/search");
        assert_eq!(url.query(), Some("q=hello&page=1"));
    }

    #[test]
    fn test_parse_file_url() {
        let url = Url::parse("file:///path/to/file.txt").unwrap();
        assert_eq!(url.scheme(), "file");
        assert_eq!(url.host(), None);
        assert_eq!(url.path(), "/path/to/file.txt");
    }

    #[test]
    fn test_url_modification() {
        let mut url = Url::parse("https://example.com/path").unwrap();

        url.set_scheme("http").unwrap();
        assert_eq!(url.scheme(), "http");

        url.set_host(Some("newhost.com"));
        assert_eq!(url.host(), Some("newhost.com"));

        url.set_port(Some(8080));
        assert_eq!(url.port(), Some(8080));

        url.set_path("/new/path");
        assert_eq!(url.path(), "/new/path");

        url.set_query(Some("key=value"));
        assert_eq!(url.query(), Some("key=value"));

        url.set_fragment(Some("section"));
        assert_eq!(url.fragment(), Some("section"));
    }

    #[test]
    fn test_join_absolute() {
        let base = Url::parse("https://example.com/path").unwrap();
        let joined = base.join("/absolute").unwrap();
        assert_eq!(joined.path(), "/absolute");
        assert_eq!(joined.host(), Some("example.com"));
    }

    #[test]
    fn test_join_relative() {
        let base = Url::parse("https://example.com/dir/page.html").unwrap();
        let joined = base.join("other.html").unwrap();
        assert_eq!(joined.path(), "/dir/other.html");
    }

    #[test]
    fn test_join_parent() {
        let base = Url::parse("https://example.com/dir/subdir/page.html").unwrap();
        let joined = base.join("../other.html").unwrap();
        assert_eq!(joined.path(), "/dir/other.html");
    }

    #[test]
    fn test_join_fragment() {
        let base = Url::parse("https://example.com/page").unwrap();
        let joined = base.join("#section").unwrap();
        assert_eq!(joined.fragment(), Some("section"));
        assert_eq!(joined.path(), "/page");
    }

    #[test]
    fn test_query_pairs() {
        let url = Url::parse("https://example.com/search?q=hello&page=1&limit=10").unwrap();
        let pairs: HashMap<String, String> = url.query_pairs().collect();

        assert_eq!(pairs.get("q"), Some(&"hello".to_string()));
        assert_eq!(pairs.get("page"), Some(&"1".to_string()));
        assert_eq!(pairs.get("limit"), Some(&"10".to_string()));
    }

    #[test]
    fn test_encode() {
        assert_eq!(encode("hello world"), "hello+world");
        assert_eq!(encode("a=b&c=d"), "a%3Db%26c%3Dd");
        assert_eq!(
            encode("special chars: @#$%"),
            "special+chars%3A+%40%23%24%25"
        );
    }

    #[test]
    fn test_encode_component() {
        assert_eq!(encode_component("hello world"), "hello%20world");
        assert_eq!(encode_component("a=b&c=d"), "a%3Db%26c%3Dd");
    }

    #[test]
    fn test_decode() {
        assert_eq!(decode("hello+world").unwrap(), "hello world");
        assert_eq!(decode("a%3Db%26c%3Dd").unwrap(), "a=b&c=d");
    }

    #[test]
    fn test_decode_component() {
        assert_eq!(decode_component("hello%20world").unwrap(), "hello world");
        assert_eq!(decode_component("a%3Db%26c%3Dd").unwrap(), "a=b&c=d");
    }

    #[test]
    fn test_roundtrip() {
        let original = "https://user:pass@example.com:8080/path?query=value#fragment";
        let url = Url::parse(original).unwrap();
        let string = url.as_str();
        let reparsed = Url::parse(&string).unwrap();
        assert_eq!(url, reparsed);
    }

    #[test]
    fn test_url_display() {
        let url = Url::parse("https://example.com/path").unwrap();
        assert_eq!(format!("{}", url), "https://example.com/path");
    }

    #[test]
    fn test_invalid_scheme() {
        assert!(Url::parse("123://example.com").is_err());
        assert!(Url::parse("/path/only").is_err());
    }

    #[test]
    fn test_empty_query_pairs() {
        let url = Url::parse("https://example.com/path").unwrap();
        let pairs: Vec<(String, String)> = url.query_pairs().collect();
        assert!(pairs.is_empty());
    }
}
