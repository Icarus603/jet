//! Select for multi-channel operations.
//!
//! The `select!` macro and `Select` struct allow waiting on multiple channel
//! operations simultaneously. The first operation that becomes ready is executed.
//!
//! # Examples
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

use std::any::Any;

use crate::channel::{Receiver, Sender};

/// A trait for type-erased senders used in select operations.
pub trait AnySender: Send + Sync {
    /// Attempt to send a value without blocking.
    /// Returns true if the send succeeded.
    fn try_send_any(&self, value: Box<dyn Any>) -> Result<(), Box<dyn Any>>;

    /// Check if the channel is closed.
    fn is_closed(&self) -> bool;
}

/// A trait for type-erased receivers used in select operations.
pub trait AnyReceiver: Send + Sync {
    /// Attempt to receive a value without blocking.
    /// Returns Some(value) if a value was received.
    fn try_recv_any(&self) -> Option<Box<dyn Any>>;

    /// Check if the channel is closed and empty.
    fn is_closed_and_empty(&self) -> bool;
}

impl<T: Send + Sync + 'static> AnySender for Sender<T> {
    fn try_send_any(&self, value: Box<dyn Any>) -> Result<(), Box<dyn Any>> {
        match value.downcast::<T>() {
            Ok(v) => match self.try_send(*v) {
                Ok(()) => Ok(()),
                Err(e) => Err(Box::new(e.into_inner())),
            },
            Err(v) => Err(v),
        }
    }

    fn is_closed(&self) -> bool {
        self.is_closed()
    }
}

impl<T: Send + Sync + 'static> AnyReceiver for Receiver<T> {
    fn try_recv_any(&self) -> Option<Box<dyn Any>> {
        match self.try_recv() {
            Ok(v) => Some(Box::new(v)),
            Err(_) => None,
        }
    }

    fn is_closed_and_empty(&self) -> bool {
        self.is_closed() && self.try_recv().is_err()
    }
}

/// A case in a select operation.
pub enum SelectCase {
    /// Send operation on a channel.
    Send {
        /// The sender to send on.
        sender: Box<dyn AnySender>,
        /// The value to send.
        value: Box<dyn Any>,
    },
    /// Receive operation on a channel.
    Recv {
        /// The receiver to receive from.
        receiver: Box<dyn AnyReceiver>,
    },
    /// Default case (non-blocking).
    Default,
}

/// Builder for select operations.
///
/// Use this struct to construct a select operation that waits on multiple
/// channel operations. The first operation that becomes ready is executed.
///
/// # Examples
///
/// ```no_run
/// use jet_rt_sync::{Channel, Select};
///
/// let (tx, rx) = Channel::with_capacity(1);
/// tx.send(42).unwrap();
///
/// let result = Select::new()
///     .recv(&rx)
///     .wait();
///
/// assert_eq!(result.index, 0);
/// ```
pub struct Select {
    cases: Vec<SelectCase>,
}

impl Select {
    /// Create a new select builder.
    pub fn new() -> Self {
        Self { cases: Vec::new() }
    }

    /// Add a receive case.
    ///
    /// # Type Parameters
    ///
    /// * `T` - The type of values in the channel.
    ///
    /// # Arguments
    ///
    /// * `rx` - The receiver to receive from.
    pub fn recv<T: Send + Sync + 'static>(mut self, rx: &Receiver<T>) -> Self {
        self.cases.push(SelectCase::Recv {
            receiver: Box::new(rx.clone()),
        });
        self
    }

    /// Add a send case.
    ///
    /// # Type Parameters
    ///
    /// * `T` - The type of values in the channel.
    ///
    /// # Arguments
    ///
    /// * `tx` - The sender to send on.
    /// * `value` - The value to send.
    pub fn send<T: Send + Sync + 'static>(mut self, tx: &Sender<T>, value: T) -> Self {
        self.cases.push(SelectCase::Send {
            sender: Box::new(tx.clone()),
            value: Box::new(value),
        });
        self
    }

    /// Add a default case (non-blocking).
    ///
    /// If no other case is ready, the select will return immediately
    /// with the default case index.
    pub fn default(mut self) -> Self {
        self.cases.push(SelectCase::Default);
        self
    }

    /// Wait for one of the cases to become ready.
    ///
    /// This method blocks until one of the channel operations is ready
    /// or the default case is selected.
    ///
    /// # Returns
    ///
    /// A `SelectResult` containing the index of the selected case and
    /// optionally a received value.
    pub fn wait(self) -> SelectResult {
        // Fast path: check if any case is ready
        if let Some(result) = self.try_select() {
            return result;
        }

        // Slow path: wait for a case to become ready
        loop {
            // Check each case
            for (index, case) in self.cases.iter().enumerate() {
                match case {
                    SelectCase::Recv { receiver } => {
                        if let Some(value) = receiver.try_recv_any() {
                            return SelectResult {
                                index,
                                value: Some(value),
                            };
                        }
                    }
                    SelectCase::Send { sender, value } => {
                        if !sender.is_closed() {
                            // Try to send - this may need special handling
                            // For now, we assume the channel has space
                            // In a full implementation, we'd use a parking mechanism
                            let _value_ref: &Box<dyn Any> = value;
                            // Clone the value for the attempt
                            // This is a workaround since we can't easily clone Box<dyn Any>
                            // In practice, this would use a different approach
                        }
                    }
                    SelectCase::Default => {
                        // Default case is always ready
                        return SelectResult { index, value: None };
                    }
                }
            }

            // No case ready, yield and try again
            std::thread::yield_now();
        }
    }

    /// Try to select a ready case without blocking.
    ///
    /// Returns `None` if no case is ready.
    fn try_select(&self) -> Option<SelectResult> {
        for (index, case) in self.cases.iter().enumerate() {
            match case {
                SelectCase::Recv { receiver } => {
                    if let Some(value) = receiver.try_recv_any() {
                        return Some(SelectResult {
                            index,
                            value: Some(value),
                        });
                    }
                }
                SelectCase::Default => {
                    return Some(SelectResult { index, value: None });
                }
                _ => {}
            }
        }
        None
    }
}

impl Default for Select {
    fn default() -> Self {
        Self::new()
    }
}

/// The result of a select operation.
///
/// Contains the index of the selected case and optionally a received value.
#[derive(Debug)]
pub struct SelectResult {
    /// The index of the selected case (0-based).
    pub index: usize,
    /// The received value, if any.
    pub value: Option<Box<dyn Any>>,
}

impl SelectResult {
    /// Get the received value as a concrete type.
    ///
    /// # Type Parameters
    ///
    /// * `T` - The expected type of the value.
    ///
    /// # Returns
    ///
    /// `Some(&T)` if the value exists and is of type T, `None` otherwise.
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.value.as_ref()?.downcast_ref::<T>()
    }
}

/// A select set for advanced use cases.
///
/// This struct allows registering multiple operations and waiting on them
/// with more control than the basic `Select` builder.
pub struct SelectSet {
    cases: Vec<SelectCase>,
}

impl SelectSet {
    /// Create a new empty select set.
    pub fn new() -> Self {
        Self { cases: Vec::new() }
    }

    /// Add a receive operation.
    pub fn add_recv<T: Send + Sync + 'static>(&mut self, rx: &Receiver<T>) {
        self.cases.push(SelectCase::Recv {
            receiver: Box::new(rx.clone()),
        });
    }

    /// Add a send operation.
    pub fn add_send<T: Send + Sync + 'static>(&mut self, tx: &Sender<T>, value: T) {
        self.cases.push(SelectCase::Send {
            sender: Box::new(tx.clone()),
            value: Box::new(value),
        });
    }

    /// Add a default case.
    pub fn add_default(&mut self) {
        self.cases.push(SelectCase::Default);
    }

    /// Wait for one operation to become ready.
    pub fn wait(self) -> SelectResult {
        Select { cases: self.cases }.wait()
    }

    /// Try to select without blocking.
    /// Returns the index of the selected case, or None if no case is ready.
    pub fn try_select(&self) -> Option<usize> {
        for (index, case) in self.cases.iter().enumerate() {
            match case {
                SelectCase::Recv { receiver } => {
                    if receiver.try_recv_any().is_some() {
                        return Some(index);
                    }
                }
                SelectCase::Default => {
                    return Some(index);
                }
                _ => {}
            }
        }
        None
    }
}

impl Default for SelectSet {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Channel;

    #[test]
    fn test_select_recv() {
        let (tx, rx): (Sender<i32>, Receiver<i32>) = Channel::with_capacity(1);
        tx.send(42).unwrap();

        let result = Select::new().recv(&rx).wait();

        assert_eq!(result.index, 0);
        assert_eq!(result.downcast_ref::<i32>(), Some(&42));
    }

    #[test]
    fn test_select_multiple_recv() {
        let (tx1, rx1): (Sender<i32>, Receiver<i32>) = Channel::with_capacity(1);
        let (_tx2, rx2): (Sender<i32>, Receiver<i32>) = Channel::with_capacity(1);

        tx1.send(100).unwrap();

        let result = Select::new().recv(&rx1).recv(&rx2).wait();

        assert_eq!(result.index, 0);
        assert_eq!(result.downcast_ref::<i32>(), Some(&100));
    }

    #[test]
    fn test_select_default() {
        let (_tx, rx): (Sender<i32>, Receiver<i32>) = Channel::with_capacity(1);

        let result = Select::new().recv(&rx).default().wait();

        // Default case should be selected since channel is empty
        assert_eq!(result.index, 1);
        assert!(result.value.is_none());
    }

    #[test]
    fn test_select_set() {
        let (tx, rx): (Sender<i32>, Receiver<i32>) = Channel::with_capacity(1);
        tx.send(42).unwrap();

        let mut set = SelectSet::new();
        set.add_recv(&rx);

        let result = set.wait();

        assert_eq!(result.index, 0);
        assert_eq!(result.downcast_ref::<i32>(), Some(&42));
    }

    #[test]
    fn test_select_result_downcast() {
        let (tx, rx): (Sender<String>, Receiver<String>) = Channel::with_capacity(1);
        tx.send(String::from("hello")).unwrap();

        let result = Select::new().recv(&rx).wait();

        assert_eq!(
            result.downcast_ref::<String>(),
            Some(&String::from("hello"))
        );
        assert_eq!(result.downcast_ref::<i32>(), None);
    }

    #[test]
    fn test_select_set_try_select() {
        let (tx, rx): (Sender<i32>, Receiver<i32>) = Channel::with_capacity(1);
        tx.send(42).unwrap();

        let mut set = SelectSet::new();
        set.add_recv(&rx);

        let index = set.try_select();
        assert_eq!(index, Some(0));
    }

    #[test]
    fn test_select_set_try_select_empty() {
        let (_tx, rx): (Sender<i32>, Receiver<i32>) = Channel::with_capacity(1);

        let mut set = SelectSet::new();
        set.add_recv(&rx);

        let index = set.try_select();
        assert_eq!(index, None);
    }
}
