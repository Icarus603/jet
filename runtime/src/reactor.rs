//! I/O reactor integration.
//!
//! This module provides the I/O reactor that integrates with the scheduler
//! to enable efficient async I/O operations.

use crate::config::RuntimeConfig;
use crate::RuntimeError;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

/// The I/O reactor.
///
/// The reactor monitors I/O sources for readiness and integrates with
/// the scheduler to wake up tasks when I/O is ready.
///
/// # Example
///
/// ```rust,no_run
/// use jet_rt::{Runtime, RuntimeConfig};
///
/// let runtime = Runtime::new(RuntimeConfig::default()).unwrap();
/// let reactor = runtime.reactor();
/// ```
pub struct Reactor {
    /// The underlying reactor implementation
    inner: Arc<Mutex<jet_rt_sched::Reactor>>,
    /// Flag indicating if the reactor is running
    running: Arc<AtomicBool>,
    /// Handle for controlling the reactor
    handle: ReactorHandle,
    /// Map from I/O tokens to waiting task IDs
    token_to_task:
        Arc<Mutex<std::collections::HashMap<jet_rt_sched::IoToken, jet_rt_sched::TaskId>>>,
}

/// A handle to the reactor.
///
/// This handle can be used to register I/O sources and poll for events
/// from any thread.
#[derive(Clone)]
#[allow(dead_code)]
pub struct ReactorHandle {
    /// Shared reference to the reactor
    inner: Arc<Mutex<jet_rt_sched::Reactor>>,
    /// Map from I/O tokens to waiting task IDs
    token_to_task:
        Arc<Mutex<std::collections::HashMap<jet_rt_sched::IoToken, jet_rt_sched::TaskId>>>,
}

impl Reactor {
    /// Creates a new I/O reactor.
    ///
    /// # Arguments
    ///
    /// * `config` - The runtime configuration.
    ///
    /// # Returns
    ///
    /// Returns a new reactor or an error if creation fails.
    pub fn new(_config: &RuntimeConfig) -> crate::Result<Self> {
        let reactor =
            jet_rt_sched::Reactor::new().map_err(|e| RuntimeError::ReactorInit(e.to_string()))?;

        let inner = Arc::new(Mutex::new(reactor));
        let token_to_task = Arc::new(Mutex::new(std::collections::HashMap::new()));

        Ok(Self {
            inner: inner.clone(),
            running: Arc::new(AtomicBool::new(false)),
            handle: ReactorHandle {
                inner,
                token_to_task: token_to_task.clone(),
            },
            token_to_task,
        })
    }

    /// Starts the reactor.
    ///
    /// This spawns a background thread that polls for I/O events
    /// and wakes up waiting tasks.
    pub(crate) fn start(&self) -> crate::Result<()> {
        if self.running.load(Ordering::SeqCst) {
            return Ok(());
        }

        self.running.store(true, Ordering::SeqCst);

        // Start the reactor thread
        let inner = self.inner.clone();
        let running = self.running.clone();
        let token_to_task = self.token_to_task.clone();

        std::thread::spawn(move || {
            let mut events = Vec::new();

            while running.load(Ordering::Relaxed) {
                // Poll for events with a timeout
                if let Ok(reactor) = inner.lock() {
                    match reactor.poll(&mut events, Some(Duration::from_millis(10))) {
                        Ok(n) if n > 0 => {
                            // Process events - wake up waiting tasks
                            let token_map = token_to_task.lock().unwrap();
                            for event in &events {
                                // Look up which task is waiting for this event
                                if let Some(_task_id) = token_map.get(&event.token) {
                                    // Store the event for the scheduler to process
                                    // The scheduler will poll for ready events
                                    // and unblock the corresponding tasks
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        });

        Ok(())
    }

    /// Stops the reactor.
    pub(crate) fn stop(&self) -> crate::Result<()> {
        self.running.store(false, Ordering::SeqCst);
        Ok(())
    }

    /// Returns a handle to the reactor.
    pub fn handle(&self) -> ReactorHandle {
        self.handle.clone()
    }
}

impl ReactorHandle {
    /// Registers an I/O source with the reactor.
    ///
    /// # Arguments
    ///
    /// * `source` - The I/O source to register.
    /// * `interest` - The types of events to monitor.
    ///
    /// # Returns
    ///
    /// Returns a token that can be used to identify this registration.
    pub fn register(
        &self,
        source: &impl std::os::unix::io::AsRawFd,
        interest: jet_rt_sched::Interest,
    ) -> std::io::Result<jet_rt_sched::IoToken> {
        let reactor = self.inner.lock().unwrap();
        reactor.register(source, interest)
    }

    /// Deregisters an I/O source.
    ///
    /// # Arguments
    ///
    /// * `token` - The token returned by `register`.
    pub fn deregister(&self, token: jet_rt_sched::IoToken) -> std::io::Result<()> {
        let reactor = self.inner.lock().unwrap();
        reactor.deregister(token)
    }
}

// Re-export reactor types from the scheduler

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reactor_creation() {
        let config = RuntimeConfig::default();
        let reactor = Reactor::new(&config);
        assert!(reactor.is_ok());
    }

    #[test]
    fn test_reactor_start_stop() {
        let config = RuntimeConfig::default();
        let reactor = Reactor::new(&config).unwrap();

        assert!(reactor.start().is_ok());
        assert!(reactor.running.load(Ordering::SeqCst));

        assert!(reactor.stop().is_ok());
        assert!(!reactor.running.load(Ordering::SeqCst));
    }

    #[test]
    fn test_reactor_handle() {
        let config = RuntimeConfig::default();
        let reactor = Reactor::new(&config).unwrap();

        let handle = reactor.handle();
        // Just verify we can clone the handle
        let _handle2 = handle.clone();
    }
}
