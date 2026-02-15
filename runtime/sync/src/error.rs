//! Error types for channel operations.
//!
//! This module defines the error types returned by channel send and receive
//! operations when they fail.

use std::fmt;

/// Error returned when sending fails because the channel is closed.
///
/// The sent value is returned so it can be recovered.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SendError<T>(pub T);

impl<T> fmt::Display for SendError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "sending on a closed channel")
    }
}

impl<T: fmt::Debug> std::error::Error for SendError<T> {}

/// Error returned when receiving fails because the channel is closed and empty.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RecvError;

impl fmt::Display for RecvError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "receiving on an empty and closed channel")
    }
}

impl std::error::Error for RecvError {}

/// Error returned when a non-blocking receive fails.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TryRecvError {
    /// The channel is empty but not closed.
    Empty,
    /// The channel is empty and closed.
    Disconnected,
}

impl fmt::Display for TryRecvError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TryRecvError::Empty => write!(f, "channel is empty"),
            TryRecvError::Disconnected => write!(f, "channel is empty and closed"),
        }
    }
}

impl std::error::Error for TryRecvError {}

/// Error returned when a non-blocking send fails.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrySendError<T> {
    /// The channel is full but not closed.
    Full(T),
    /// The channel is closed.
    Closed(T),
}

impl<T> TrySendError<T> {
    /// Returns the value that was being sent.
    pub fn into_inner(self) -> T {
        match self {
            TrySendError::Full(v) | TrySendError::Closed(v) => v,
        }
    }
}

impl<T> fmt::Display for TrySendError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TrySendError::Full(_) => write!(f, "channel is full"),
            TrySendError::Closed(_) => write!(f, "channel is closed"),
        }
    }
}

impl<T: fmt::Debug> std::error::Error for TrySendError<T> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_send_error_display() {
        let err = SendError(42);
        assert_eq!(err.to_string(), "sending on a closed channel");
    }

    #[test]
    fn test_recv_error_display() {
        let err = RecvError;
        assert_eq!(err.to_string(), "receiving on an empty and closed channel");
    }

    #[test]
    fn test_try_recv_error_display() {
        let err = TryRecvError::Empty;
        assert_eq!(err.to_string(), "channel is empty");

        let err = TryRecvError::Disconnected;
        assert_eq!(err.to_string(), "channel is empty and closed");
    }

    #[test]
    fn test_try_send_error_display() {
        let err = TrySendError::Full(42);
        assert_eq!(err.to_string(), "channel is full");

        let err = TrySendError::Closed(42);
        assert_eq!(err.to_string(), "channel is closed");
    }

    #[test]
    fn test_try_send_error_into_inner() {
        let err = TrySendError::Full(42);
        assert_eq!(err.into_inner(), 42);

        let err = TrySendError::Closed(100);
        assert_eq!(err.into_inner(), 100);
    }
}
