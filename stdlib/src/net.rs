//! Network - Networking primitives for Jet
//!
//! This module provides TCP, UDP, and HTTP networking capabilities
//! for the Jet programming language.

use crate::io::{IoError, IoErrorKind, IoResult};
use crate::string::JetString;
use std::io::{Read, Write};
use std::net::{Shutdown, SocketAddr as StdSocketAddr, ToSocketAddrs};
use std::net::{
    TcpListener as StdTcpListener, TcpStream as StdTcpStream, UdpSocket as StdUdpSocket,
};
use std::time::Duration as StdDuration;

/// A TCP stream between a local and a remote socket.
///
/// After creating a `TcpStream` by either connecting to a remote host or
/// accepting a connection on a `TcpListener`, data can be transmitted
/// by reading and writing to it.
///
/// # Examples
///
/// ```
/// use jet_stdlib::net::TcpStream;
///
/// // Connect to a remote host
/// if let Ok(stream) = TcpStream::connect("127.0.0.1:8080") {
///     // Use the stream
/// }
/// ```
pub struct TcpStream {
    /// The underlying TCP stream
    inner: StdTcpStream,
    /// Remote address
    peer_addr: Option<SocketAddr>,
    /// Local address
    local_addr: Option<SocketAddr>,
}

/// A TCP socket server, listening for connections.
///
/// After creating a `TcpListener` by binding it to a socket address, it listens
/// for incoming TCP connections. These can be accepted by calling `accept`.
///
/// # Examples
///
/// ```
/// use jet_stdlib::net::TcpListener;
///
/// // Bind to a local port
/// if let Ok(listener) = TcpListener::bind("127.0.0.1:8080") {
///     // Accept connections
/// }
/// ```
pub struct TcpListener {
    /// The underlying TCP listener
    inner: StdTcpListener,
    /// Local address
    local_addr: SocketAddr,
}

/// A UDP socket.
///
/// UDP is a connectionless protocol. The `UdpSocket` can send and receive
/// datagrams to/from any address.
///
/// # Examples
///
/// ```
/// use jet_stdlib::net::UdpSocket;
///
/// // Bind to a local port
/// if let Ok(socket) = UdpSocket::bind("127.0.0.1:8080") {
///     // Send and receive datagrams
/// }
/// ```
pub struct UdpSocket {
    /// The underlying UDP socket
    inner: StdUdpSocket,
    /// Local address
    local_addr: SocketAddr,
}

/// An IPv4 or IPv6 socket address.
///
/// A socket address consists of an IP address and a port number.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SocketAddr {
    /// The IP address as a string
    pub ip: JetString,
    /// The port number
    pub port: u16,
}

/// A simple HTTP client for making HTTP requests.
///
/// # Examples
///
/// ```
/// use jet_stdlib::net::HttpClient;
///
/// let client = HttpClient::new();
/// // Make HTTP requests
/// ```
pub struct HttpClient {
    /// Connection timeout
    timeout: Option<StdDuration>,
    /// User agent string
    user_agent: JetString,
}

/// An HTTP request builder.
pub struct HttpRequest {
    /// The HTTP method
    method: HttpMethod,
    /// The URL
    url: JetString,
    /// Request headers
    headers: Vec<(JetString, JetString)>,
    /// Request body
    body: Option<Vec<u8>>,
    /// Timeout
    timeout: Option<StdDuration>,
}

/// An HTTP response.
pub struct HttpResponse {
    /// HTTP status code
    pub status_code: u16,
    /// Response headers
    pub headers: Vec<(JetString, JetString)>,
    /// Response body
    pub body: Vec<u8>,
}

/// HTTP request methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HttpMethod {
    /// GET request
    Get,
    /// POST request
    Post,
    /// PUT request
    Put,
    /// DELETE request
    Delete,
    /// HEAD request
    Head,
    /// OPTIONS request
    Options,
    /// PATCH request
    Patch,
}

impl TcpStream {
    /// Opens a TCP connection to a remote host.
    ///
    /// # Arguments
    /// * `addr` - The address to connect to (e.g., "127.0.0.1:8080")
    ///
    /// # Returns
    /// Returns the connected stream on success, or an error on failure.
    pub fn connect(addr: &str) -> IoResult<Self> {
        match StdTcpStream::connect(addr) {
            Ok(inner) => {
                let peer_addr = inner.peer_addr().ok().map(|a| a.into());
                let local_addr = inner.local_addr().ok().map(|a| a.into());
                Ok(TcpStream {
                    inner,
                    peer_addr,
                    local_addr,
                })
            }
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Opens a TCP connection with a timeout.
    ///
    /// # Arguments
    /// * `addr` - The address to connect to
    /// * `timeout_secs` - Timeout in seconds
    pub fn connect_timeout(addr: &str, timeout_secs: u64) -> IoResult<Self> {
        let addrs: Vec<_> = match addr.to_socket_addrs() {
            Ok(addrs) => addrs.collect(),
            Err(e) => return Err(map_io_error(e)),
        };

        if addrs.is_empty() {
            return Err(IoError::new(
                "Invalid socket address",
                IoErrorKind::InvalidInput,
            ));
        }

        let timeout = StdDuration::from_secs(timeout_secs);

        for addr in addrs {
            match StdTcpStream::connect_timeout(&addr, timeout) {
                Ok(inner) => {
                    let peer_addr = inner.peer_addr().ok().map(|a| a.into());
                    let local_addr = inner.local_addr().ok().map(|a| a.into());
                    return Ok(TcpStream {
                        inner,
                        peer_addr,
                        local_addr,
                    });
                }
                Err(_) => continue,
            }
        }

        Err(IoError::new("Connection timed out", IoErrorKind::Other))
    }

    /// Reads data from the stream into a buffer.
    ///
    /// Returns the number of bytes read, or 0 if the stream has closed.
    pub fn read(&mut self, buf: &mut [u8]) -> IoResult<usize> {
        match self.inner.read(buf) {
            Ok(n) => Ok(n),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Writes data to the stream.
    ///
    /// Returns the number of bytes written.
    pub fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
        match self.inner.write(buf) {
            Ok(n) => Ok(n),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Writes all data to the stream.
    pub fn write_all(&mut self, buf: &[u8]) -> IoResult<()> {
        match self.inner.write_all(buf) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Flushes the output stream.
    pub fn flush(&mut self) -> IoResult<()> {
        match self.inner.flush() {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Sets the read timeout.
    pub fn set_read_timeout(&self, secs: u64) -> IoResult<()> {
        let timeout = if secs == 0 {
            None
        } else {
            Some(StdDuration::from_secs(secs))
        };
        match self.inner.set_read_timeout(timeout) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Sets the write timeout.
    pub fn set_write_timeout(&self, secs: u64) -> IoResult<()> {
        let timeout = if secs == 0 {
            None
        } else {
            Some(StdDuration::from_secs(secs))
        };
        match self.inner.set_write_timeout(timeout) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Returns the peer address.
    pub fn peer_addr(&self) -> Option<&SocketAddr> {
        self.peer_addr.as_ref()
    }

    /// Returns the local address.
    pub fn local_addr(&self) -> Option<&SocketAddr> {
        self.local_addr.as_ref()
    }

    /// Shuts down the read, write, or both halves of this connection.
    pub fn shutdown(&self, how: ShutdownMode) -> IoResult<()> {
        let shutdown = match how {
            ShutdownMode::Read => Shutdown::Read,
            ShutdownMode::Write => Shutdown::Write,
            ShutdownMode::Both => Shutdown::Both,
        };
        match self.inner.shutdown(shutdown) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Sets the nodelay option (disables Nagle's algorithm).
    pub fn set_nodelay(&self, nodelay: bool) -> IoResult<()> {
        match self.inner.set_nodelay(nodelay) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Gets the nodelay option.
    pub fn nodelay(&self) -> IoResult<bool> {
        match self.inner.nodelay() {
            Ok(v) => Ok(v),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Sets the TTL (time-to-live) for IP packets.
    pub fn set_ttl(&self, ttl: u32) -> IoResult<()> {
        match self.inner.set_ttl(ttl) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Gets the TTL.
    pub fn ttl(&self) -> IoResult<u32> {
        match self.inner.ttl() {
            Ok(v) => Ok(v),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Attempts to clone the stream.
    pub fn try_clone(&self) -> IoResult<Self> {
        match self.inner.try_clone() {
            Ok(inner) => {
                let peer_addr = inner.peer_addr().ok().map(|a| a.into());
                let local_addr = inner.local_addr().ok().map(|a| a.into());
                Ok(TcpStream {
                    inner,
                    peer_addr,
                    local_addr,
                })
            }
            Err(e) => Err(map_io_error(e)),
        }
    }
}

impl Read for TcpStream {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.inner.read(buf)
    }
}

impl Write for TcpStream {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.inner.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

impl TcpListener {
    /// Creates a new TCP listener bound to the specified address.
    ///
    /// # Arguments
    /// * `addr` - The address to bind to (e.g., "127.0.0.1:8080")
    pub fn bind(addr: &str) -> IoResult<Self> {
        match StdTcpListener::bind(addr) {
            Ok(inner) => {
                let local_addr = inner.local_addr().unwrap().into();
                Ok(TcpListener { inner, local_addr })
            }
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Accepts a new incoming connection.
    ///
    /// Returns the connected stream and the peer address.
    pub fn accept(&self) -> IoResult<(TcpStream, SocketAddr)> {
        match self.inner.accept() {
            Ok((inner, peer)) => {
                let peer_addr: SocketAddr = peer.into();
                let local_addr = inner.local_addr().ok().map(|a| a.into());
                let stream = TcpStream {
                    inner,
                    peer_addr: Some(peer_addr.clone()),
                    local_addr,
                };
                Ok((stream, peer_addr))
            }
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Returns the local address.
    pub fn local_addr(&self) -> &SocketAddr {
        &self.local_addr
    }

    /// Sets non-blocking mode.
    pub fn set_nonblocking(&self, nonblocking: bool) -> IoResult<()> {
        match self.inner.set_nonblocking(nonblocking) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Sets the TTL.
    pub fn set_ttl(&self, ttl: u32) -> IoResult<()> {
        match self.inner.set_ttl(ttl) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Gets the TTL.
    pub fn ttl(&self) -> IoResult<u32> {
        match self.inner.ttl() {
            Ok(v) => Ok(v),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Returns an iterator over incoming connections.
    pub fn incoming(&self) -> IncomingConnections<'_> {
        IncomingConnections { listener: self }
    }
}

/// Iterator over incoming TCP connections.
pub struct IncomingConnections<'a> {
    listener: &'a TcpListener,
}

impl<'a> Iterator for IncomingConnections<'a> {
    type Item = IoResult<(TcpStream, SocketAddr)>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.listener.accept())
    }
}

impl UdpSocket {
    /// Creates a UDP socket bound to the specified address.
    ///
    /// # Arguments
    /// * `addr` - The address to bind to
    pub fn bind(addr: &str) -> IoResult<Self> {
        match StdUdpSocket::bind(addr) {
            Ok(inner) => {
                let local_addr = inner.local_addr().unwrap().into();
                Ok(UdpSocket { inner, local_addr })
            }
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Sends data to the specified address.
    ///
    /// # Arguments
    /// * `buf` - The data to send
    /// * `addr` - The destination address
    pub fn send_to(&self, buf: &[u8], addr: &str) -> IoResult<usize> {
        match addr.to_socket_addrs() {
            Ok(mut addrs) => {
                if let Some(addr) = addrs.next() {
                    match self.inner.send_to(buf, addr) {
                        Ok(n) => Ok(n),
                        Err(e) => Err(map_io_error(e)),
                    }
                } else {
                    Err(IoError::new("Invalid address", IoErrorKind::InvalidInput))
                }
            }
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Receives data from the socket.
    ///
    /// Returns the number of bytes received and the sender's address.
    pub fn recv_from(&self, buf: &mut [u8]) -> IoResult<(usize, SocketAddr)> {
        match self.inner.recv_from(buf) {
            Ok((n, addr)) => Ok((n, addr.into())),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Sends data on a connected socket.
    pub fn send(&self, buf: &[u8]) -> IoResult<usize> {
        match self.inner.send(buf) {
            Ok(n) => Ok(n),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Receives data on a connected socket.
    pub fn recv(&self, buf: &mut [u8]) -> IoResult<usize> {
        match self.inner.recv(buf) {
            Ok(n) => Ok(n),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Connects the socket to a remote address.
    pub fn connect(&self, addr: &str) -> IoResult<()> {
        match addr.to_socket_addrs() {
            Ok(mut addrs) => {
                if let Some(addr) = addrs.next() {
                    match self.inner.connect(addr) {
                        Ok(_) => Ok(()),
                        Err(e) => Err(map_io_error(e)),
                    }
                } else {
                    Err(IoError::new("Invalid address", IoErrorKind::InvalidInput))
                }
            }
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Returns the local address.
    pub fn local_addr(&self) -> &SocketAddr {
        &self.local_addr
    }

    /// Sets the read timeout.
    pub fn set_read_timeout(&self, secs: u64) -> IoResult<()> {
        let timeout = if secs == 0 {
            None
        } else {
            Some(StdDuration::from_secs(secs))
        };
        match self.inner.set_read_timeout(timeout) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Sets the write timeout.
    pub fn set_write_timeout(&self, secs: u64) -> IoResult<()> {
        let timeout = if secs == 0 {
            None
        } else {
            Some(StdDuration::from_secs(secs))
        };
        match self.inner.set_write_timeout(timeout) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Sets non-blocking mode.
    pub fn set_nonblocking(&self, nonblocking: bool) -> IoResult<()> {
        match self.inner.set_nonblocking(nonblocking) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Sets the broadcast option.
    pub fn set_broadcast(&self, broadcast: bool) -> IoResult<()> {
        match self.inner.set_broadcast(broadcast) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Gets the broadcast option.
    pub fn broadcast(&self) -> IoResult<bool> {
        match self.inner.broadcast() {
            Ok(v) => Ok(v),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Sets the TTL.
    pub fn set_ttl(&self, ttl: u32) -> IoResult<()> {
        match self.inner.set_ttl(ttl) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Gets the TTL.
    pub fn ttl(&self) -> IoResult<u32> {
        match self.inner.ttl() {
            Ok(v) => Ok(v),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Attempts to clone the socket.
    pub fn try_clone(&self) -> IoResult<Self> {
        match self.inner.try_clone() {
            Ok(inner) => {
                let local_addr = inner.local_addr().unwrap().into();
                Ok(UdpSocket { inner, local_addr })
            }
            Err(e) => Err(map_io_error(e)),
        }
    }
}

impl SocketAddr {
    /// Creates a new socket address.
    pub fn new(ip: &str, port: u16) -> Self {
        SocketAddr {
            ip: JetString::from_str(ip),
            port,
        }
    }

    /// Returns the IP address as a string.
    pub fn ip(&self) -> &str {
        self.ip.as_str()
    }

    /// Returns the port number.
    pub fn port(&self) -> u16 {
        self.port
    }

    /// Parses a socket address from a string.
    pub fn parse(s: &str) -> Option<Self> {
        s.to_socket_addrs().ok()?.next().map(|a| a.into())
    }
}

impl From<StdSocketAddr> for SocketAddr {
    fn from(addr: StdSocketAddr) -> Self {
        SocketAddr {
            ip: JetString::from_str(&addr.ip().to_string()),
            port: addr.port(),
        }
    }
}

impl From<SocketAddr> for StdSocketAddr {
    fn from(addr: SocketAddr) -> Self {
        // Try to parse as a standard socket address
        format!("{}:{}", addr.ip(), addr.port())
            .to_socket_addrs()
            .ok()
            .and_then(|mut addrs| addrs.next())
            .unwrap_or_else(|| StdSocketAddr::from(([0, 0, 0, 0], 0)))
    }
}

impl HttpClient {
    /// Creates a new HTTP client with default settings.
    pub fn new() -> Self {
        HttpClient {
            timeout: Some(StdDuration::from_secs(30)),
            user_agent: JetString::from_str("Jet/1.0"),
        }
    }

    /// Creates a new HTTP client with a custom timeout.
    pub fn with_timeout(secs: u64) -> Self {
        HttpClient {
            timeout: if secs == 0 {
                None
            } else {
                Some(StdDuration::from_secs(secs))
            },
            user_agent: JetString::from_str("Jet/1.0"),
        }
    }

    /// Sets the user agent string.
    pub fn set_user_agent(&mut self, agent: &str) {
        self.user_agent = JetString::from_str(agent);
    }

    /// Sets the timeout in seconds.
    pub fn set_timeout(&mut self, secs: u64) {
        self.timeout = if secs == 0 {
            None
        } else {
            Some(StdDuration::from_secs(secs))
        };
    }

    /// Returns the timeout in seconds, or 0 if no timeout.
    pub fn timeout(&self) -> u64 {
        self.timeout.map(|d| d.as_secs()).unwrap_or(0)
    }

    /// Returns the user agent string.
    pub fn user_agent(&self) -> &str {
        self.user_agent.as_str()
    }

    /// Performs a GET request.
    pub fn get(&self, url: &str) -> IoResult<HttpResponse> {
        self.request(HttpRequest::get(url))
    }

    /// Performs a POST request.
    pub fn post(&self, url: &str, body: &[u8]) -> IoResult<HttpResponse> {
        self.request(HttpRequest::post(url).body(body))
    }

    /// Performs a PUT request.
    pub fn put(&self, url: &str, body: &[u8]) -> IoResult<HttpResponse> {
        self.request(HttpRequest::put(url).body(body))
    }

    /// Performs a DELETE request.
    pub fn delete(&self, url: &str) -> IoResult<HttpResponse> {
        self.request(HttpRequest::delete(url))
    }

    /// Performs an HTTP request.
    pub fn request(&self, request: HttpRequest) -> IoResult<HttpResponse> {
        // Parse the URL to extract host and path
        let (host, port, path, is_https) = parse_url(request.url.as_str())?;

        // Connect to the server
        let addr = format!("{}:{}", host, port);
        let mut stream = TcpStream::connect(&addr)?;

        // Set timeout if configured
        if let Some(timeout) = self.timeout {
            stream.set_read_timeout(timeout.as_secs())?;
            stream.set_write_timeout(timeout.as_secs())?;
        }

        // Build the HTTP request
        let method = match request.method {
            HttpMethod::Get => "GET",
            HttpMethod::Post => "POST",
            HttpMethod::Put => "PUT",
            HttpMethod::Delete => "DELETE",
            HttpMethod::Head => "HEAD",
            HttpMethod::Options => "OPTIONS",
            HttpMethod::Patch => "PATCH",
        };

        let mut http_request = format!("{} {} HTTP/1.1\r\n", method, path);
        http_request.push_str(&format!("Host: {}\r\n", host));
        http_request.push_str(&format!("User-Agent: {}\r\n", self.user_agent.as_str()));
        http_request.push_str("Connection: close\r\n");

        // Add custom headers
        for (key, value) in &request.headers {
            http_request.push_str(&format!("{}: {}\r\n", key.as_str(), value.as_str()));
        }

        // Add body if present
        if let Some(ref body) = request.body {
            http_request.push_str(&format!("Content-Length: {}\r\n", body.len()));
            http_request.push_str("\r\n");
        } else {
            http_request.push_str("\r\n");
        }

        // Send the request
        stream.write_all(http_request.as_bytes())?;

        // Send body if present
        if let Some(ref body) = request.body {
            stream.write_all(body)?;
        }

        // Read the response
        let mut response_data = Vec::new();
        let mut buf = [0u8; 4096];

        loop {
            match stream.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => response_data.extend_from_slice(&buf[..n]),
                Err(e) => return Err(e),
            }
        }

        // Parse the HTTP response
        parse_http_response(&response_data)
    }
}

impl Default for HttpClient {
    fn default() -> Self {
        Self::new()
    }
}

impl HttpRequest {
    /// Creates a new GET request.
    pub fn get(url: &str) -> Self {
        HttpRequest {
            method: HttpMethod::Get,
            url: JetString::from_str(url),
            headers: Vec::new(),
            body: None,
            timeout: None,
        }
    }

    /// Creates a new POST request.
    pub fn post(url: &str) -> Self {
        HttpRequest {
            method: HttpMethod::Post,
            url: JetString::from_str(url),
            headers: Vec::new(),
            body: None,
            timeout: None,
        }
    }

    /// Creates a new PUT request.
    pub fn put(url: &str) -> Self {
        HttpRequest {
            method: HttpMethod::Put,
            url: JetString::from_str(url),
            headers: Vec::new(),
            body: None,
            timeout: None,
        }
    }

    /// Creates a new DELETE request.
    pub fn delete(url: &str) -> Self {
        HttpRequest {
            method: HttpMethod::Delete,
            url: JetString::from_str(url),
            headers: Vec::new(),
            body: None,
            timeout: None,
        }
    }

    /// Adds a header to the request.
    pub fn header(mut self, key: &str, value: &str) -> Self {
        self.headers
            .push((JetString::from_str(key), JetString::from_str(value)));
        self
    }

    /// Sets the request body.
    pub fn body(mut self, body: &[u8]) -> Self {
        self.body = Some(body.to_vec());
        self
    }

    /// Sets the request timeout.
    pub fn timeout(mut self, secs: u64) -> Self {
        self.timeout = Some(StdDuration::from_secs(secs));
        self
    }
}

impl HttpResponse {
    /// Creates a new HTTP response.
    pub fn new(status_code: u16) -> Self {
        HttpResponse {
            status_code,
            headers: Vec::new(),
            body: Vec::new(),
        }
    }

    /// Returns true if the response is successful (2xx status code).
    pub fn is_success(&self) -> bool {
        self.status_code >= 200 && self.status_code < 300
    }

    /// Returns true if the response is a redirect (3xx status code).
    pub fn is_redirect(&self) -> bool {
        self.status_code >= 300 && self.status_code < 400
    }

    /// Returns true if the response is a client error (4xx status code).
    pub fn is_client_error(&self) -> bool {
        self.status_code >= 400 && self.status_code < 500
    }

    /// Returns true if the response is a server error (5xx status code).
    pub fn is_server_error(&self) -> bool {
        self.status_code >= 500 && self.status_code < 600
    }

    /// Returns the body as a string.
    pub fn body_as_string(&self) -> JetString {
        JetString::from_str(&String::from_utf8_lossy(&self.body))
    }

    /// Gets a header value by name.
    pub fn header(&self, name: &str) -> Option<&str> {
        let name_lower = name.to_lowercase();
        self.headers
            .iter()
            .find(|(k, _)| k.as_str().to_lowercase() == name_lower)
            .map(|(_, v)| v.as_str())
    }

    /// Returns the Content-Type header value.
    pub fn content_type(&self) -> Option<&str> {
        self.header("Content-Type")
    }

    /// Returns the Content-Length header value.
    pub fn content_length(&self) -> Option<usize> {
        self.header("Content-Length").and_then(|v| v.parse().ok())
    }
}

/// Shutdown mode for TCP streams.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShutdownMode {
    /// Shutdown the read half.
    Read,
    /// Shutdown the write half.
    Write,
    /// Shutdown both halves.
    Both,
}

/// Parses a URL and returns (host, port, path, is_https).
fn parse_url(url: &str) -> IoResult<(String, u16, String, bool)> {
    let url = url.trim();

    // Check for protocol
    let (is_https, rest) = if url.starts_with("https://") {
        (true, &url[8..])
    } else if url.starts_with("http://") {
        (false, &url[7..])
    } else {
        // Assume HTTP if no protocol specified
        (false, url)
    };

    // Find the path
    let (host_port, path) = match rest.find('/') {
        Some(idx) => (&rest[..idx], rest[idx..].to_string()),
        None => (rest, "/".to_string()),
    };

    // Parse host and port
    let (host, port) = match host_port.find(':') {
        Some(idx) => {
            let host = host_port[..idx].to_string();
            let port: u16 = host_port[idx + 1..]
                .parse()
                .map_err(|_| IoError::new("Invalid port", IoErrorKind::InvalidInput))?;
            (host, port)
        }
        None => {
            let port = if is_https { 443 } else { 80 };
            (host_port.to_string(), port)
        }
    };

    Ok((host, port, path, is_https))
}

/// Parses an HTTP response.
fn parse_http_response(data: &[u8]) -> IoResult<HttpResponse> {
    let data_str = String::from_utf8_lossy(data);

    // Split headers and body
    let mut parts = data_str.splitn(2, "\r\n\r\n");
    let header_part = parts.next().unwrap_or("");
    let body_part = parts.next().unwrap_or("");

    // Parse status line
    let mut lines = header_part.lines();
    let status_line = lines
        .next()
        .ok_or_else(|| IoError::new("Invalid HTTP response", IoErrorKind::InvalidInput))?;

    // Parse status code
    let status_parts: Vec<_> = status_line.split_whitespace().collect();
    if status_parts.len() < 2 {
        return Err(IoError::new(
            "Invalid HTTP status line",
            IoErrorKind::InvalidInput,
        ));
    }

    let status_code: u16 = status_parts[1]
        .parse()
        .map_err(|_| IoError::new("Invalid status code", IoErrorKind::InvalidInput))?;

    // Parse headers
    let mut headers = Vec::new();
    for line in lines {
        if let Some(idx) = line.find(':') {
            let key = line[..idx].trim();
            let value = line[idx + 1..].trim();
            headers.push((JetString::from_str(key), JetString::from_str(value)));
        }
    }

    // Body is the rest (handle potential binary data properly)
    let header_len = header_part.len() + 4; // +4 for \r\n\r\n
    let body = if data.len() > header_len {
        data[header_len..].to_vec()
    } else {
        Vec::new()
    };

    Ok(HttpResponse {
        status_code,
        headers,
        body,
    })
}

/// Maps a std::io::Error to our IoError.
fn map_io_error(e: std::io::Error) -> IoError {
    use std::io::ErrorKind;

    let kind = match e.kind() {
        ErrorKind::NotFound => IoErrorKind::NotFound,
        ErrorKind::PermissionDenied => IoErrorKind::PermissionDenied,
        ErrorKind::AlreadyExists => IoErrorKind::AlreadyExists,
        ErrorKind::InvalidInput => IoErrorKind::InvalidInput,
        ErrorKind::UnexpectedEof => IoErrorKind::UnexpectedEof,
        ErrorKind::WriteZero => IoErrorKind::WriteZero,
        ErrorKind::Interrupted => IoErrorKind::Interrupted,
        ErrorKind::NotADirectory => IoErrorKind::NotADirectory,
        ErrorKind::IsADirectory => IoErrorKind::IsADirectory,
        _ => IoErrorKind::Other,
    };

    IoError::new(&e.to_string(), kind)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_socket_addr_creation() {
        let addr = SocketAddr::new("127.0.0.1", 8080);
        assert_eq!(addr.ip(), "127.0.0.1");
        assert_eq!(addr.port(), 8080);
    }

    #[test]
    fn test_socket_addr_parse() {
        let addr = SocketAddr::parse("192.168.1.1:3000").unwrap();
        assert_eq!(addr.ip(), "192.168.1.1");
        assert_eq!(addr.port(), 3000);
    }

    #[test]
    fn test_http_client_creation() {
        let client = HttpClient::new();
        assert_eq!(client.timeout(), 30);
        assert_eq!(client.user_agent(), "Jet/1.0");
    }

    #[test]
    fn test_http_client_with_timeout() {
        let client = HttpClient::with_timeout(60);
        assert_eq!(client.timeout(), 60);
    }

    #[test]
    fn test_http_request_builder() {
        let req = HttpRequest::get("http://example.com")
            .header("Accept", "application/json")
            .timeout(10);

        assert_eq!(req.method, HttpMethod::Get);
        assert_eq!(req.url.as_str(), "http://example.com");
        assert_eq!(req.headers.len(), 1);
        assert_eq!(req.timeout.map(|d| d.as_secs()), Some(10));
    }

    #[test]
    fn test_http_response() {
        let mut response = HttpResponse::new(200);
        response.headers.push((
            JetString::from_str("Content-Type"),
            JetString::from_str("application/json"),
        ));
        response.body = b"{\"key\": \"value\"}".to_vec();

        assert!(response.is_success());
        assert!(!response.is_client_error());
        assert!(!response.is_server_error());
        assert_eq!(response.content_type(), Some("application/json"));
        assert_eq!(response.body_as_string().as_str(), "{\"key\": \"value\"}");
    }

    #[test]
    fn test_http_response_error_codes() {
        let client_error = HttpResponse::new(404);
        assert!(client_error.is_client_error());
        assert!(!client_error.is_success());

        let server_error = HttpResponse::new(500);
        assert!(server_error.is_server_error());
        assert!(!server_error.is_success());

        let redirect = HttpResponse::new(301);
        assert!(redirect.is_redirect());
    }

    #[test]
    fn test_url_parsing() {
        let (host, port, path, is_https) = parse_url("http://example.com/path").unwrap();
        assert_eq!(host, "example.com");
        assert_eq!(port, 80);
        assert_eq!(path, "/path");
        assert!(!is_https);

        let (host, port, path, is_https) = parse_url("https://example.com:8443/api").unwrap();
        assert_eq!(host, "example.com");
        assert_eq!(port, 8443);
        assert_eq!(path, "/api");
        assert!(is_https);

        let (host, port, path, is_https) = parse_url("example.com").unwrap();
        assert_eq!(host, "example.com");
        assert_eq!(port, 80);
        assert_eq!(path, "/");
        assert!(!is_https);
    }

    #[test]
    fn test_http_response_parsing() {
        let response_data = b"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 13\r\n\r\nHello, World!";

        let response = parse_http_response(response_data).unwrap();
        assert_eq!(response.status_code, 200);
        assert_eq!(response.content_type(), Some("text/plain"));
        assert_eq!(response.content_length(), Some(13));
        assert_eq!(response.body_as_string().as_str(), "Hello, World!");
    }

    #[test]
    fn test_udp_socket_bind() {
        // Bind to port 0 to let the OS assign a port
        let socket = UdpSocket::bind("127.0.0.1:0");
        assert!(socket.is_ok());

        let socket = socket.unwrap();
        assert!(socket.local_addr().port() > 0);
    }

    #[test]
    fn test_tcp_listener_bind() {
        // Bind to port 0 to let the OS assign a port
        let listener = TcpListener::bind("127.0.0.1:0");
        assert!(listener.is_ok());

        let listener = listener.unwrap();
        assert!(listener.local_addr().port() > 0);
    }
}
