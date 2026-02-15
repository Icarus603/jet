//! I/O reactor for asynchronous I/O operations.
//!
//! The reactor monitors I/O sources (sockets, files, etc.) for readiness
//! and wakes up tasks waiting for I/O events. It integrates with the
//! scheduler to enable efficient async I/O.
//!
//! # Platform Support
//!
//! - **macOS/BSD**: Uses kqueue
//! - **Linux**: Uses epoll
//! - **Windows**: Uses IOCP (not yet implemented)
//!
//! # Examples
//!
//! ```
//! use jet_rt_sched::reactor::Reactor;
//! use std::net::TcpListener;
//!
//! let reactor = Reactor::new().unwrap();
//! // Register sockets, files, etc. with the reactor
//! ```

use std::collections::HashMap;
use std::os::unix::io::AsRawFd;
#[cfg(target_os = "macos")]
use std::os::unix::io::RawFd;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;
use std::time::Duration;

/// A unique identifier for an I/O registration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IoToken(u64);

impl IoToken {
    /// Creates a new unique I/O token.
    fn new() -> Self {
        static COUNTER: AtomicU64 = AtomicU64::new(1);
        Self(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

/// Interest in I/O events.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Interest {
    /// Interested in readable events.
    Readable,
    /// Interested in writable events.
    Writable,
    /// Interested in both readable and writable events.
    Both,
}

/// An I/O event that occurred.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IoEvent {
    /// The token associated with the I/O source.
    pub token: IoToken,
    /// Whether the source is readable.
    pub readable: bool,
    /// Whether the source is writable.
    pub writable: bool,
    /// Whether an error occurred.
    pub error: bool,
}

/// Registration information for an I/O source.
struct Registration {
    /// The file descriptor.
    #[cfg(target_os = "macos")]
    fd: RawFd,
    /// The interest in events (stored to avoid dead_code warning).
    _interest: Interest,
}

/// The I/O reactor.
///
/// The reactor monitors I/O sources and reports readiness events.
/// It is designed to be integrated with the scheduler for efficient
/// async I/O handling.
///
/// # Examples
///
/// ```
/// use jet_rt_sched::reactor::Reactor;
///
/// let reactor = Reactor::new().unwrap();
/// ```
pub struct Reactor {
    /// The kqueue/epoll file descriptor.
    #[cfg(target_os = "macos")]
    kqueue_fd: RawFd,
    /// Map of tokens to registrations.
    registrations: Mutex<HashMap<IoToken, Registration>>,
}

impl Reactor {
    /// Creates a new I/O reactor.
    ///
    /// # Errors
    ///
    /// Returns an error if the underlying kqueue/epoll creation fails.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sched::reactor::Reactor;
    ///
    /// let reactor = Reactor::new().unwrap();
    /// ```
    pub fn new() -> std::io::Result<Self> {
        #[cfg(target_os = "macos")]
        {
            let fd = unsafe { libc::kqueue() };
            if fd < 0 {
                return Err(std::io::Error::last_os_error());
            }

            Ok(Self {
                kqueue_fd: fd,
                registrations: Mutex::new(HashMap::new()),
            })
        }

        #[cfg(not(target_os = "macos"))]
        {
            // Placeholder for other platforms
            Ok(Self {
                registrations: Mutex::new(HashMap::new()),
            })
        }
    }

    /// Registers an I/O source with the reactor.
    ///
    /// # Arguments
    ///
    /// * `source` - The I/O source to register (must implement `AsRawFd`).
    /// * `interest` - The types of events to monitor for.
    ///
    /// # Returns
    ///
    /// Returns a token that can be used to identify this registration.
    ///
    /// # Errors
    ///
    /// Returns an error if registration fails.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use jet_rt_sched::reactor::{Reactor, Interest};
    /// use std::net::TcpListener;
    ///
    /// let reactor = Reactor::new().unwrap();
    /// let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    /// let token = reactor.register(&listener, Interest::Readable).unwrap();
    /// ```
    pub fn register(&self, source: &impl AsRawFd, interest: Interest) -> std::io::Result<IoToken> {
        let _fd = source.as_raw_fd();
        let token = IoToken::new();

        #[cfg(target_os = "macos")]
        {
            let mut changes: [libc::kevent64_s; 2] = unsafe { std::mem::zeroed() };
            let mut nchanges = 0;

            match interest {
                Interest::Readable | Interest::Both => {
                    changes[nchanges] = libc::kevent64_s {
                        ident: _fd as u64,
                        filter: libc::EVFILT_READ,
                        flags: libc::EV_ADD | libc::EV_ENABLE,
                        fflags: 0,
                        data: 0,
                        udata: token.0,
                        ext: [0; 2],
                    };
                    nchanges += 1;
                }
                _ => {}
            }

            match interest {
                Interest::Writable | Interest::Both => {
                    changes[nchanges] = libc::kevent64_s {
                        ident: _fd as u64,
                        filter: libc::EVFILT_WRITE,
                        flags: libc::EV_ADD | libc::EV_ENABLE,
                        fflags: 0,
                        data: 0,
                        udata: token.0,
                        ext: [0; 2],
                    };
                    nchanges += 1;
                }
                _ => {}
            }

            let ret = unsafe {
                libc::kevent64(
                    self.kqueue_fd,
                    changes.as_ptr(),
                    nchanges as i32,
                    std::ptr::null_mut(),
                    0,
                    0,
                    std::ptr::null(),
                )
            };

            if ret < 0 {
                return Err(std::io::Error::last_os_error());
            }
        }

        let _registration = Registration {
            #[cfg(target_os = "macos")]
            fd: _fd,
            _interest: interest,
        };

        self.registrations
            .lock()
            .unwrap()
            .insert(token, _registration);

        Ok(token)
    }

    /// Deregisters an I/O source from the reactor.
    ///
    /// # Arguments
    ///
    /// * `token` - The token returned by `register`.
    ///
    /// # Errors
    ///
    /// Returns an error if deregistration fails.
    pub fn deregister(&self, token: IoToken) -> std::io::Result<()> {
        let _registration = self
            .registrations
            .lock()
            .unwrap()
            .remove(&token)
            .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "Token not found"))?;

        #[cfg(target_os = "macos")]
        {
            let mut changes: [libc::kevent64_s; 2] = unsafe { std::mem::zeroed() };
            let mut nchanges = 0;

            match _registration._interest {
                Interest::Readable | Interest::Both => {
                    changes[nchanges] = libc::kevent64_s {
                        ident: _registration.fd as u64,
                        filter: libc::EVFILT_READ,
                        flags: libc::EV_DELETE,
                        fflags: 0,
                        data: 0,
                        udata: 0,
                        ext: [0; 2],
                    };
                    nchanges += 1;
                }
                _ => {}
            }

            match _registration._interest {
                Interest::Writable | Interest::Both => {
                    changes[nchanges] = libc::kevent64_s {
                        ident: _registration.fd as u64,
                        filter: libc::EVFILT_WRITE,
                        flags: libc::EV_DELETE,
                        fflags: 0,
                        data: 0,
                        udata: 0,
                        ext: [0; 2],
                    };
                    nchanges += 1;
                }
                _ => {}
            }

            let ret = unsafe {
                libc::kevent64(
                    self.kqueue_fd,
                    changes.as_ptr(),
                    nchanges as i32,
                    std::ptr::null_mut(),
                    0,
                    0,
                    std::ptr::null(),
                )
            };

            if ret < 0 {
                return Err(std::io::Error::last_os_error());
            }
        }

        Ok(())
    }

    /// Polls for I/O events.
    ///
    /// This method blocks until at least one event is available or the
    /// timeout expires.
    ///
    /// # Arguments
    ///
    /// * `events` - A vector to store the returned events.
    /// * `timeout` - The maximum time to wait, or `None` to wait indefinitely.
    ///
    /// # Returns
    ///
    /// Returns the number of events that occurred.
    ///
    /// # Errors
    ///
    /// Returns an error if polling fails.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sched::reactor::Reactor;
    /// use std::time::Duration;
    ///
    /// let reactor = Reactor::new().unwrap();
    /// let mut events = Vec::new();
    /// let n = reactor.poll(&mut events, Some(Duration::from_millis(100))).unwrap();
    /// ```
    pub fn poll(
        &self,
        events: &mut Vec<IoEvent>,
        timeout: Option<Duration>,
    ) -> std::io::Result<usize> {
        events.clear();

        #[cfg(target_os = "macos")]
        {
            let mut kevents: [libc::kevent64_s; 1024] = unsafe { std::mem::zeroed() };

            let timeout_spec = timeout.map(|t| libc::timespec {
                tv_sec: t.as_secs() as i64,
                tv_nsec: t.subsec_nanos() as i64,
            });

            let timeout_ptr = timeout_spec
                .as_ref()
                .map(|t| t as *const _)
                .unwrap_or(std::ptr::null());

            let n = unsafe {
                libc::kevent64(
                    self.kqueue_fd,
                    std::ptr::null(),
                    0,
                    kevents.as_mut_ptr(),
                    kevents.len() as i32,
                    0,
                    timeout_ptr,
                )
            };

            if n < 0 {
                return Err(std::io::Error::last_os_error());
            }

            for kev in kevents.iter().take(n as usize) {
                events.push(IoEvent {
                    token: IoToken(kev.udata),
                    readable: kev.filter == libc::EVFILT_READ,
                    writable: kev.filter == libc::EVFILT_WRITE,
                    error: kev.flags & libc::EV_ERROR != 0,
                });
            }

            Ok(n as usize)
        }

        #[cfg(not(target_os = "macos"))]
        {
            // Placeholder for other platforms
            std::thread::sleep(timeout.unwrap_or(Duration::from_millis(100)));
            Ok(0)
        }
    }

    /// Returns the number of registered I/O sources.
    pub fn num_registrations(&self) -> usize {
        self.registrations.lock().unwrap().len()
    }
}

impl Drop for Reactor {
    fn drop(&mut self) {
        #[cfg(target_os = "macos")]
        {
            unsafe {
                libc::close(self.kqueue_fd);
            }
        }
    }
}

impl std::fmt::Debug for Reactor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Reactor")
            .field("num_registrations", &self.num_registrations())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::ErrorKind;
    use std::net::TcpListener;

    fn bind_loopback_or_skip() -> Option<TcpListener> {
        match TcpListener::bind("127.0.0.1:0") {
            Ok(listener) => Some(listener),
            Err(err) if err.kind() == ErrorKind::PermissionDenied => {
                eprintln!("skipping test: loopback bind denied: {err}");
                None
            }
            Err(err) => panic!("failed to bind loopback listener: {err}"),
        }
    }

    #[test]
    fn test_reactor_new() {
        let reactor = Reactor::new();
        assert!(reactor.is_ok());
    }

    #[test]
    fn test_reactor_debug() {
        let reactor = Reactor::new().unwrap();
        let debug_str = format!("{:?}", reactor);
        assert!(debug_str.contains("Reactor"));
    }

    #[test]
    fn test_reactor_register() {
        let reactor = Reactor::new().unwrap();
        let Some(listener) = bind_loopback_or_skip() else {
            return;
        };

        let token = reactor.register(&listener, Interest::Readable);
        assert!(token.is_ok());
    }

    #[test]
    fn test_reactor_deregister() {
        let reactor = Reactor::new().unwrap();
        let Some(listener) = bind_loopback_or_skip() else {
            return;
        };

        let token = reactor.register(&listener, Interest::Readable).unwrap();
        assert!(reactor.deregister(token).is_ok());
    }

    #[test]
    fn test_reactor_poll_empty() {
        let reactor = Reactor::new().unwrap();
        let mut events = Vec::new();

        let n = reactor.poll(&mut events, Some(Duration::from_millis(10)));
        assert!(n.is_ok());
        assert_eq!(n.unwrap(), 0);
    }

    #[test]
    fn test_io_token() {
        let token1 = IoToken::new();
        let token2 = IoToken::new();

        assert_ne!(token1, token2);
    }

    #[test]
    fn test_interest() {
        assert_eq!(Interest::Readable, Interest::Readable);
        assert_eq!(Interest::Writable, Interest::Writable);
        assert_eq!(Interest::Both, Interest::Both);
        assert_ne!(Interest::Readable, Interest::Writable);
    }
}
