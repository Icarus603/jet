//! Jet Channels - Inter-task communication primitives.
//!
//! This crate provides channel-based communication for the Jet runtime,
//! supporting both buffered (asynchronous) and unbuffered (synchronous)
//! channels, as well as select operations for waiting on multiple channels.
//!
//! # Quick Start
//!
//! ```
//! use jet_rt_sync::Channel;
//!
//! // Create a buffered channel
//! let (tx, rx) = Channel::with_capacity(10);
//!
//! // Send a value
//! tx.send(42).unwrap();
//!
//! // Receive the value
//! let value = rx.recv().unwrap();
//! assert_eq!(value, 42);
//! ```
//!
//! # Channel Types
//!
//! - **Buffered channels**: Have a fixed capacity. Send operations complete
//!   immediately if space is available. Useful for decoupling producers and
//!   consumers.
//!
//! - **Unbuffered channels**: Have no buffer. Send operations block until
//!   a receiver is ready (rendezvous semantics). Useful for synchronization.
//!
//! # Select Operations
//!
//! The `Select` struct allows waiting on multiple channel operations
//! simultaneously:
//!
//! ```no_run
//! use jet_rt_sync::{Channel, Select};
//!
//! let (tx1, rx1) = Channel::<i32>::with_capacity(1);
//! let (_tx2, rx2) = Channel::<i32>::with_capacity(1);
//!
//! tx1.send(1).unwrap();
//!
//! let result = Select::new()
//!     .recv(&rx1)
//!     .recv(&rx2)
//!     .wait();
//!
//! assert_eq!(result.index, 0); // rx1 was ready
//! ```

#![warn(missing_docs)]

pub mod channel;
pub mod condvar;
pub mod error;
pub mod mutex;
pub mod rwlock;
pub mod select;

// Re-export main types for convenience
pub use channel::{Channel, Receiver, Sender};
pub use condvar::{Condvar, SharedCondvar};
pub use error::{RecvError, SendError, TryRecvError, TrySendError};
pub use mutex::{Mutex, MutexGuard, SharedMutex};
pub use rwlock::{RwLock, RwLockReadGuard, RwLockWriteGuard, SharedRwLock};
pub use select::{Select, SelectResult, SelectSet};

/// Creates an unbuffered channel.
///
/// This is a convenience function equivalent to `Channel::unbuffered()`.
///
/// # Examples
///
/// ```
/// use jet_rt_sync::chan;
///
/// let (tx, rx) = chan::<i32>();
/// ```
pub fn chan<T>() -> (Sender<T>, Receiver<T>) {
    Channel::unbuffered()
}

/// Creates a buffered channel with the given capacity.
///
/// This is a convenience function equivalent to `Channel::with_capacity(cap)`.
///
/// # Examples
///
/// ```
/// use jet_rt_sync::buffered_chan;
///
/// let (tx, rx) = buffered_chan::<i32>(10);
/// ```
pub fn buffered_chan<T>(capacity: usize) -> (Sender<T>, Receiver<T>) {
    Channel::with_capacity(capacity)
}

/// Sends a value on a channel.
///
/// This is a convenience function equivalent to `sender.send(value)`.
///
/// # Examples
///
/// ```
/// use jet_rt_sync::{buffered_chan, send};
///
/// let (tx, rx) = buffered_chan::<i32>(1);
/// send(&tx, 42).unwrap();
/// ```
pub fn send<T>(sender: &Sender<T>, value: T) -> Result<(), SendError<T>> {
    sender.send(value)
}

/// Receives a value from a channel.
///
/// This is a convenience function equivalent to `receiver.recv()`.
///
/// # Examples
///
/// ```
/// use jet_rt_sync::{buffered_chan, send, recv};
///
/// let (tx, rx) = buffered_chan::<i32>(1);
/// send(&tx, 42).unwrap();
/// let value = recv(&rx).unwrap();
/// assert_eq!(value, 42);
/// ```
pub fn recv<T>(receiver: &Receiver<T>) -> Result<T, RecvError> {
    receiver.recv()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_chan_function() {
        // Unbuffered channels require rendezvous, so we need two threads
        let (tx, rx) = chan::<i32>();

        let sender = thread::spawn(move || {
            tx.send(42).unwrap();
        });

        let receiver = thread::spawn(move || {
            assert_eq!(rx.recv().unwrap(), 42);
        });

        sender.join().unwrap();
        receiver.join().unwrap();
    }

    #[test]
    fn test_buffered_chan_function() {
        let (tx, rx) = buffered_chan::<i32>(5);
        tx.send(1).unwrap();
        tx.send(2).unwrap();
        assert_eq!(rx.recv().unwrap(), 1);
        assert_eq!(rx.recv().unwrap(), 2);
    }

    #[test]
    fn test_send_recv_functions() {
        let (tx, rx) = buffered_chan::<String>(1);
        send(&tx, String::from("hello")).unwrap();
        let value = recv(&rx).unwrap();
        assert_eq!(value, "hello");
    }
}
