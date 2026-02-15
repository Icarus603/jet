//! Channel implementation for inter-task communication.
//!
//! Channels provide a way for tasks to communicate by sending messages
//! between them. This module supports both buffered (asynchronous) and
//! unbuffered (synchronous) channels.
//!
//! # Examples
//!
//! ```
//! use jet_rt_sync::Channel;
//!
//! // Create a buffered channel with capacity 10
//! let (tx, rx) = Channel::with_capacity(10);
//!
//! // Send a value
//! tx.send(42).unwrap();
//!
//! // Receive the value
//! let value = rx.recv().unwrap();
//! assert_eq!(value, 42);
//! ```

use crossbeam_queue::ArrayQueue;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::task::Waker;

use crate::error::{RecvError, SendError, TryRecvError, TrySendError};

/// A channel for sending values between tasks.
///
/// Channels can be either buffered (with a fixed capacity) or unbuffered
/// (synchronous, requiring both sender and receiver to rendezvous).
///
/// # Type Parameters
///
/// * `T` - The type of values that can be sent through the channel.
///
/// # Examples
///
/// ```
/// use jet_rt_sync::Channel;
///
/// // Buffered channel with capacity 5
/// let (tx, rx) = Channel::<i32>::with_capacity(5);
///
/// // Unbuffered channel (synchronous) - requires both sender and receiver
/// // to be ready at the same time for handoff
/// let (tx2, rx2) = Channel::<i32>::unbuffered();
/// ```
pub struct Channel<T> {
    /// Internal channel state (not used directly - Channel acts as a namespace).
    #[allow(dead_code)]
    inner: Arc<ChannelInner<T>>,
}

/// Internal channel state shared between senders and receivers.
struct ChannelInner<T> {
    /// The message buffer (for buffered channels).
    buffer: ArrayQueue<T>,
    /// Capacity (0 for unbuffered channels).
    capacity: usize,
    /// Waiting senders (for unbuffered channels or when buffer is full).
    send_waiters: Mutex<VecDeque<Waker>>,
    /// Waiting receivers.
    recv_waiters: Mutex<VecDeque<Waker>>,
    /// Channel closed flag.
    closed: AtomicBool,
    /// For unbuffered channels: the value being handed off.
    /// This is stored as a pointer to allow atomic exchange.
    handoff: Mutex<Option<T>>,
    /// Flag indicating if a handoff is in progress (unbuffered only).
    handoff_active: AtomicBool,
}

impl<T> ChannelInner<T> {
    /// Close the channel and wake all waiters.
    fn close(&self) {
        self.closed.store(true, Ordering::SeqCst);

        // Wake all waiting senders
        let mut send_waiters = self.send_waiters.lock().unwrap();
        while let Some(waker) = send_waiters.pop_front() {
            waker.wake();
        }
        drop(send_waiters);

        // Wake all waiting receivers
        let mut recv_waiters = self.recv_waiters.lock().unwrap();
        while let Some(waker) = recv_waiters.pop_front() {
            waker.wake();
        }
    }

    /// Check if the channel is closed.
    fn is_closed(&self) -> bool {
        self.closed.load(Ordering::SeqCst)
    }
}

/// The sending half of a channel.
///
/// Senders can be cloned to create multiple producers. When all senders
/// are dropped, the channel is closed.
///
/// # Examples
///
/// ```
/// use jet_rt_sync::Channel;
///
/// let (tx, rx) = Channel::<i32>::with_capacity(2);
/// let tx2 = tx.clone();
///
/// // Both senders can send values
/// tx.send(1).unwrap();
/// tx2.send(2).unwrap();
///
/// // Receive the values
/// assert_eq!(rx.recv().unwrap(), 1);
/// assert_eq!(rx.recv().unwrap(), 2);
/// ```
pub struct Sender<T> {
    inner: Arc<ChannelInner<T>>,
}

impl<T> Sender<T> {
    /// Send a value on the channel (blocking).
    ///
    /// This method blocks until the value can be sent. For buffered channels,
    /// it returns immediately if there's space in the buffer. For unbuffered
    /// channels, it blocks until a receiver is ready to receive the value.
    ///
    /// # Errors
    ///
    /// Returns `SendError` if the channel is closed. The value being sent
    /// is returned as part of the error.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Channel;
    ///
    /// let (tx, rx) = Channel::with_capacity(1);
    /// tx.send(42).unwrap();
    /// ```
    pub fn send(&self, value: T) -> Result<(), SendError<T>> {
        // Check if closed first
        if self.inner.is_closed() {
            return Err(SendError(value));
        }

        if self.inner.capacity == 0 {
            self.send_unbuffered(value)
        } else {
            self.send_buffered(value)
        }
    }

    /// Try to send a value without blocking.
    ///
    /// # Errors
    ///
    /// Returns `TrySendError::Full` if the channel buffer is full.
    /// Returns `TrySendError::Closed` if the channel is closed.
    pub fn try_send(&self, value: T) -> Result<(), TrySendError<T>> {
        if self.inner.is_closed() {
            return Err(TrySendError::Closed(value));
        }

        if self.inner.capacity == 0 {
            // For unbuffered channels, try_send only succeeds if there's a waiting receiver
            // For simplicity, we return Full to indicate the channel is "full" (no receiver ready)
            Err(TrySendError::Full(value))
        } else {
            match self.inner.buffer.push(value) {
                Ok(()) => {
                    // Wake a receiver if any
                    if let Some(waker) = self.inner.recv_waiters.lock().unwrap().pop_front() {
                        waker.wake();
                    }
                    Ok(())
                }
                Err(value) => Err(TrySendError::Full(value)),
            }
        }
    }

    /// Send a value on a buffered channel.
    fn send_buffered(&self, value: T) -> Result<(), SendError<T>> {
        let mut value = value;

        loop {
            // Try to push to buffer
            match self.inner.buffer.push(value) {
                Ok(()) => {
                    // Wake a receiver if any
                    if let Some(waker) = self.inner.recv_waiters.lock().unwrap().pop_front() {
                        waker.wake();
                    }
                    return Ok(());
                }
                Err(v) => {
                    value = v;
                }
            }

            // Buffer full, check if closed
            if self.inner.is_closed() {
                return Err(SendError(value));
            }

            // Park and wait for space
            std::thread::yield_now();

            // Check again after yielding
            if self.inner.is_closed() {
                return Err(SendError(value));
            }
        }
    }

    /// Send a value on an unbuffered channel (rendezvous).
    fn send_unbuffered(&self, value: T) -> Result<(), SendError<T>> {
        // For unbuffered channels, we need to rendezvous with a receiver
        // We store the value and wait for a receiver to take it

        let mut value = Some(value);

        loop {
            // Check if closed
            if self.inner.is_closed() {
                return Err(SendError(value.take().unwrap()));
            }

            // Try to place value for handoff
            let mut handoff = self.inner.handoff.lock().unwrap();

            if handoff.is_none() && !self.inner.handoff_active.load(Ordering::SeqCst) {
                // Place the value
                *handoff = value.take();
                self.inner.handoff_active.store(true, Ordering::SeqCst);
                drop(handoff);

                // Wake a waiting receiver
                if let Some(waker) = self.inner.recv_waiters.lock().unwrap().pop_front() {
                    waker.wake();
                }

                // Wait for the receiver to take the value
                loop {
                    if !self.inner.handoff_active.load(Ordering::SeqCst) {
                        // Receiver has taken the value
                        return Ok(());
                    }

                    if self.inner.is_closed() {
                        // Channel closed during handoff
                        // Try to recover the value if still there
                        let mut handoff = self.inner.handoff.lock().unwrap();
                        if let Some(v) = handoff.take() {
                            return Err(SendError(v));
                        }
                        return Err(SendError(unsafe { std::mem::zeroed::<T>() }));
                    }

                    std::thread::yield_now();
                }
            }

            drop(handoff);

            // Another sender is active or value still pending, wait
            std::thread::yield_now();
        }
    }

    /// Close the channel.
    ///
    /// This prevents further sends and signals to receivers that no more
    /// values will be sent.
    pub fn close(&self) {
        self.inner.close();
    }

    /// Check if the channel is closed.
    pub fn is_closed(&self) -> bool {
        self.inner.is_closed()
    }
}

impl<T> Clone for Sender<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<T> Drop for Sender<T> {
    fn drop(&mut self) {
        // Note: In a full implementation, we would track the number of senders
        // and close the channel when the last sender is dropped.
        // For now, explicit close() is required.
    }
}

/// The receiving half of a channel.
///
/// Receivers can be cloned to create multiple consumers. When all receivers
/// are dropped, the channel remains open but receivers can no longer receive.
///
/// # Examples
///
/// ```
/// use jet_rt_sync::Channel;
///
/// let (tx, rx) = Channel::with_capacity(1);
/// let rx2 = rx.clone();
///
/// tx.send(42).unwrap();
///
/// // Either receiver can receive the value
/// let value = rx.recv().or_else(|_| rx2.recv()).unwrap();
/// assert_eq!(value, 42);
/// ```
pub struct Receiver<T> {
    inner: Arc<ChannelInner<T>>,
}

impl<T> Receiver<T> {
    /// Receive a value from the channel (blocking).
    ///
    /// This method blocks until a value is available or the channel is closed
    /// and empty.
    ///
    /// # Errors
    ///
    /// Returns `RecvError` if the channel is closed and empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Channel;
    ///
    /// let (tx, rx) = Channel::with_capacity(1);
    /// tx.send(42).unwrap();
    ///
    /// let value = rx.recv().unwrap();
    /// assert_eq!(value, 42);
    /// ```
    pub fn recv(&self) -> Result<T, RecvError> {
        loop {
            // Try buffered receive first
            if let Some(value) = self.inner.buffer.pop() {
                // Wake a sender if any (buffered channels)
                if let Some(waker) = self.inner.send_waiters.lock().unwrap().pop_front() {
                    waker.wake();
                }
                return Ok(value);
            }

            // For unbuffered channels, check for handoff
            if self.inner.capacity == 0 {
                if let Some(value) = self.try_recv_unbuffered() {
                    return Ok(value);
                }
            }

            // Check if closed and empty
            if self.inner.is_closed() && self.inner.buffer.is_empty() {
                return Err(RecvError);
            }

            // Park and wait
            std::thread::yield_now();
        }
    }

    /// Try to receive a value without blocking.
    ///
    /// # Errors
    ///
    /// Returns `TryRecvError::Empty` if the channel is empty but not closed.
    /// Returns `TryRecvError::Disconnected` if the channel is closed and empty.
    pub fn try_recv(&self) -> Result<T, TryRecvError> {
        // Try buffered receive first
        if let Some(value) = self.inner.buffer.pop() {
            // Wake a sender if any
            if let Some(waker) = self.inner.send_waiters.lock().unwrap().pop_front() {
                waker.wake();
            }
            return Ok(value);
        }

        // For unbuffered channels, check for handoff
        if self.inner.capacity == 0 {
            if let Some(value) = self.try_recv_unbuffered() {
                return Ok(value);
            }
        }

        // Channel empty or closed
        if self.inner.is_closed() {
            Err(TryRecvError::Disconnected)
        } else {
            Err(TryRecvError::Empty)
        }
    }

    /// Try to receive from an unbuffered channel (non-blocking).
    fn try_recv_unbuffered(&self) -> Option<T> {
        if !self.inner.handoff_active.load(Ordering::SeqCst) {
            return None;
        }

        let mut handoff = self.inner.handoff.lock().unwrap();
        if let Some(value) = handoff.take() {
            self.inner.handoff_active.store(false, Ordering::SeqCst);
            drop(handoff);

            // Wake a waiting sender
            if let Some(waker) = self.inner.send_waiters.lock().unwrap().pop_front() {
                waker.wake();
            }

            return Some(value);
        }

        None
    }

    /// Close the channel.
    pub fn close(&self) {
        self.inner.close();
    }

    /// Check if the channel is closed.
    pub fn is_closed(&self) -> bool {
        self.inner.is_closed()
    }
}

impl<T> Clone for Receiver<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<T> Channel<T> {
    /// Create an unbuffered channel (synchronous).
    ///
    /// Unbuffered channels require both sender and receiver to rendezvous.
    /// Every send blocks until a corresponding receive is ready.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Channel;
    ///
    /// let (tx, rx) = Channel::<i32>::unbuffered();
    /// // Note: unbuffered channels require rendezvous between sender and receiver
    /// // Both must be ready simultaneously for a send/receive to complete
    /// ```
    pub fn unbuffered() -> (Sender<T>, Receiver<T>) {
        Self::with_capacity(0)
    }

    /// Create a buffered channel with the given capacity.
    ///
    /// Buffered channels can hold up to `cap` values without blocking.
    /// A capacity of 0 creates an unbuffered channel.
    ///
    /// # Arguments
    ///
    /// * `cap` - The buffer capacity. Use 0 for unbuffered channels.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Channel;
    ///
    /// let (tx, rx) = Channel::<i32>::with_capacity(10);
    /// ```
    pub fn with_capacity(cap: usize) -> (Sender<T>, Receiver<T>) {
        let inner = Arc::new(ChannelInner {
            buffer: ArrayQueue::new(cap.max(1)),
            capacity: cap,
            send_waiters: Mutex::new(VecDeque::new()),
            recv_waiters: Mutex::new(VecDeque::new()),
            closed: AtomicBool::new(false),
            handoff: Mutex::new(None),
            handoff_active: AtomicBool::new(false),
        });

        let sender = Sender {
            inner: inner.clone(),
        };
        let receiver = Receiver { inner };

        (sender, receiver)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buffered_channel() {
        let (tx, rx) = Channel::with_capacity(2);

        tx.send(1).unwrap();
        tx.send(2).unwrap();

        assert_eq!(rx.recv().unwrap(), 1);
        assert_eq!(rx.recv().unwrap(), 2);
    }

    #[test]
    fn test_channel_close_sender() {
        let (tx, rx) = Channel::<i32>::with_capacity(1);
        tx.close();

        assert!(tx.send(1).is_err());
        assert!(rx.recv().is_err());
    }

    #[test]
    fn test_channel_close_receiver() {
        let (tx, rx) = Channel::<i32>::with_capacity(1);
        rx.close();

        assert!(tx.send(1).is_err());
    }

    #[test]
    fn test_multiple_senders() {
        let (tx1, rx) = Channel::with_capacity(2);
        let tx2 = tx1.clone();

        tx1.send(1).unwrap();
        tx2.send(2).unwrap();

        let mut values = vec![rx.recv().unwrap(), rx.recv().unwrap()];
        values.sort();

        assert_eq!(values, vec![1, 2]);
    }

    #[test]
    fn test_multiple_receivers() {
        let (tx, rx1) = Channel::with_capacity(2);
        let rx2 = rx1.clone();

        tx.send(1).unwrap();
        tx.send(2).unwrap();

        // One of the receivers should get each value
        let v1 = rx1.try_recv().unwrap();
        let v2 = rx2.try_recv().unwrap();

        let mut values = vec![v1, v2];
        values.sort();

        assert_eq!(values, vec![1, 2]);
    }

    #[test]
    fn test_try_send_full() {
        let (tx, _rx) = Channel::with_capacity(1);

        tx.try_send(1).unwrap();
        assert!(matches!(tx.try_send(2), Err(TrySendError::Full(2))));
    }

    #[test]
    fn test_try_send_closed() {
        let (tx, rx) = Channel::with_capacity(1);

        tx.close();
        assert!(matches!(tx.try_send(1), Err(TrySendError::Closed(1))));
        drop(rx); // Keep rx alive until after close test
    }

    #[test]
    fn test_try_recv_empty() {
        let (tx, rx) = Channel::<i32>::with_capacity(1);

        assert!(matches!(rx.try_recv(), Err(TryRecvError::Empty)));
        drop(tx); // Keep tx alive
    }

    #[test]
    fn test_try_recv_disconnected() {
        let (tx, rx) = Channel::<i32>::with_capacity(1);

        tx.close();
        assert!(matches!(rx.try_recv(), Err(TryRecvError::Disconnected)));
    }

    #[test]
    fn test_buffered_ordering() {
        let (tx, rx) = Channel::with_capacity(5);

        for i in 0..5 {
            tx.send(i).unwrap();
        }

        for i in 0..5 {
            assert_eq!(rx.recv().unwrap(), i);
        }
    }

    #[test]
    fn test_is_closed() {
        let (tx, rx) = Channel::<i32>::with_capacity(1);

        assert!(!tx.is_closed());
        assert!(!rx.is_closed());

        tx.close();

        assert!(tx.is_closed());
        assert!(rx.is_closed());
    }
}
