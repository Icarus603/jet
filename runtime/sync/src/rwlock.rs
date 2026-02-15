//! Read-write lock implementation for inter-task synchronization.
//!
//! This module provides a reader-writer lock that allows multiple readers
//! or a single writer at a time. It is designed to work with the Jet
//! scheduler's M:N threading model.
//!
//! # Examples
//!
//! ```
//! use jet_rt_sync::RwLock;
//! use std::sync::Arc;
//!
//! let lock = Arc::new(RwLock::new(0));
//!
//! // Multiple readers can hold the lock simultaneously
//! let r1 = lock.read();
//! let r2 = lock.read();
//! assert_eq!(*r1, 0);
//! assert_eq!(*r2, 0);
//! drop(r1);
//! drop(r2);
//!
//! // Only one writer can hold the lock
//! *lock.write() = 42;
//! assert_eq!(*lock.read(), 42);
//! ```

use std::cell::UnsafeCell;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

/// A reader-writer lock.
///
/// This type allows multiple readers or a single writer at any point in time.
/// The lock is write-preferring to prevent writer starvation.
///
/// # Type Parameters
///
/// * `T` - The type of data protected by the lock.
///
/// # Examples
///
/// ```
/// use jet_rt_sync::RwLock;
///
/// let lock = RwLock::new(vec![1, 2, 3]);
///
/// // Read access
/// {
///     let reader = lock.read();
///     assert_eq!(reader.len(), 3);
/// }
///
/// // Write access
/// {
///     let mut writer = lock.write();
///     writer.push(4);
/// }
/// ```
pub struct RwLock<T> {
    /// The inner data protected by the lock.
    data: UnsafeCell<T>,
    /// State of the lock:
    /// - 0: unlocked
    /// - 1..MAX_READERS: number of active readers
    /// - WRITE_LOCKED: write locked (value is usize::MAX)
    state: AtomicUsize,
}

/// Maximum number of concurrent readers.
const MAX_READERS: usize = usize::MAX / 2;

/// State value indicating a write lock.
const WRITE_LOCKED: usize = usize::MAX;

/// A guard that provides read access to the data protected by an RwLock.
///
/// When the guard is dropped, the read lock is released.
pub struct RwLockReadGuard<'a, T> {
    lock: &'a RwLock<T>,
}

/// A guard that provides write access to the data protected by an RwLock.
///
/// When the guard is dropped, the write lock is released.
pub struct RwLockWriteGuard<'a, T> {
    lock: &'a RwLock<T>,
}

// RwLock is Send if T is Send
unsafe impl<T: Send> Send for RwLock<T> {}

// RwLock is Sync if T is Send + Sync
unsafe impl<T: Send + Sync> Sync for RwLock<T> {}

impl<T> RwLock<T> {
    /// Creates a new reader-writer lock protecting the given data.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::RwLock;
    ///
    /// let lock = RwLock::new(42);
    /// ```
    pub fn new(data: T) -> Self {
        Self {
            data: UnsafeCell::new(data),
            state: AtomicUsize::new(0),
        }
    }

    /// Acquires a read lock, blocking until it is available.
    ///
    /// Returns a guard that provides shared access to the protected data.
    /// The lock is released when the guard is dropped.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::RwLock;
    ///
    /// let lock = RwLock::new(42);
    /// let guard = lock.read();
    /// assert_eq!(*guard, 42);
    /// ```
    pub fn read(&self) -> RwLockReadGuard<'_, T> {
        loop {
            // Try to increment the reader count
            let current = self.state.load(Ordering::Relaxed);

            if current >= MAX_READERS {
                // Write lock is held or too many readers
                std::thread::yield_now();
                continue;
            }

            if self
                .state
                .compare_exchange_weak(current, current + 1, Ordering::Acquire, Ordering::Relaxed)
                .is_ok()
            {
                return RwLockReadGuard { lock: self };
            }

            // Failed to acquire, retry
            std::hint::spin_loop();
        }
    }

    /// Attempts to acquire a read lock without blocking.
    ///
    /// Returns `Some(RwLockReadGuard)` if the lock was acquired, `None` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::RwLock;
    ///
    /// let lock = RwLock::new(42);
    ///
    /// if let Some(guard) = lock.try_read() {
    ///     assert_eq!(*guard, 42);
    /// } else {
    ///     // Lock was not available
    /// };
    /// ```
    pub fn try_read(&self) -> Option<RwLockReadGuard<'_, T>> {
        let current = self.state.load(Ordering::Relaxed);

        if current >= MAX_READERS {
            return None;
        }

        if self
            .state
            .compare_exchange(current, current + 1, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            Some(RwLockReadGuard { lock: self })
        } else {
            None
        }
    }

    /// Acquires a write lock, blocking until it is available.
    ///
    /// Returns a guard that provides exclusive access to the protected data.
    /// The lock is released when the guard is dropped.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::RwLock;
    ///
    /// let lock = RwLock::new(42);
    /// *lock.write() = 10;
    /// assert_eq!(*lock.read(), 10);
    /// ```
    pub fn write(&self) -> RwLockWriteGuard<'_, T> {
        loop {
            // Try to acquire write lock
            if self
                .state
                .compare_exchange_weak(0, WRITE_LOCKED, Ordering::Acquire, Ordering::Relaxed)
                .is_ok()
            {
                return RwLockWriteGuard { lock: self };
            }

            // Failed to acquire, yield and retry
            std::thread::yield_now();
        }
    }

    /// Attempts to acquire a write lock without blocking.
    ///
    /// Returns `Some(RwLockWriteGuard)` if the lock was acquired, `None` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::RwLock;
    ///
    /// let lock = RwLock::new(42);
    ///
    /// if let Some(mut guard) = lock.try_write() {
    ///     *guard = 100;
    /// } else {
    ///     // Lock was not available
    /// };
    /// ```
    pub fn try_write(&self) -> Option<RwLockWriteGuard<'_, T>> {
        if self
            .state
            .compare_exchange(0, WRITE_LOCKED, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            Some(RwLockWriteGuard { lock: self })
        } else {
            None
        }
    }

    /// Returns a mutable reference to the underlying data.
    ///
    /// Since this call borrows the lock mutably, no locking is required.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::RwLock;
    ///
    /// let mut lock = RwLock::new(42);
    /// *lock.get_mut() = 10;
    /// assert_eq!(*lock.read(), 10);
    /// ```
    pub fn get_mut(&mut self) -> &mut T {
        // Safe because we have exclusive access via &mut self
        unsafe { &mut *self.data.get() }
    }

    /// Consumes the lock, returning the protected data.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::RwLock;
    ///
    /// let lock = RwLock::new(42);
    /// assert_eq!(lock.into_inner(), 42);
    /// ```
    pub fn into_inner(self) -> T {
        self.data.into_inner()
    }

    /// Returns true if the lock is currently held for writing.
    pub fn is_write_locked(&self) -> bool {
        self.state.load(Ordering::Relaxed) == WRITE_LOCKED
    }

    /// Returns the number of active readers.
    ///
    /// Note: This value may be stale by the time it is returned.
    pub fn reader_count(&self) -> usize {
        let state = self.state.load(Ordering::Relaxed);
        if state >= MAX_READERS {
            0
        } else {
            state
        }
    }

    /// Releases a read lock.
    ///
    /// # Safety
    ///
    /// This should only be called by RwLockReadGuard when it is dropped.
    unsafe fn release_read(&self) {
        self.state.fetch_sub(1, Ordering::Release);
    }

    /// Releases a write lock.
    ///
    /// # Safety
    ///
    /// This should only be called by RwLockWriteGuard when it is dropped.
    unsafe fn release_write(&self) {
        self.state.store(0, Ordering::Release);
    }
}

impl<T: Default> Default for RwLock<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T> From<T> for RwLock<T> {
    fn from(data: T) -> Self {
        Self::new(data)
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for RwLock<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("RwLock");
        match self.try_read() {
            Some(guard) => d.field("data", &*guard),
            None => d.field("data", &"<locked>"),
        };
        d.finish()
    }
}

impl<T> Deref for RwLockReadGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        // Safe because we hold a read lock
        unsafe { &*self.lock.data.get() }
    }
}

impl<T> Deref for RwLockWriteGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        // Safe because we hold a write lock
        unsafe { &*self.lock.data.get() }
    }
}

impl<T> DerefMut for RwLockWriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        // Safe because we hold a write lock
        unsafe { &mut *self.lock.data.get() }
    }
}

impl<T> Drop for RwLockReadGuard<'_, T> {
    fn drop(&mut self) {
        unsafe {
            self.lock.release_read();
        }
    }
}

impl<T> Drop for RwLockWriteGuard<'_, T> {
    fn drop(&mut self) {
        unsafe {
            self.lock.release_write();
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for RwLockReadGuard<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RwLockReadGuard")
            .field("data", &**self)
            .finish()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for RwLockWriteGuard<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RwLockWriteGuard")
            .field("data", &**self)
            .finish()
    }
}

/// A shared read-write lock that can be cloned and shared between tasks.
///
/// This is a convenience wrapper around `Arc<RwLock<T>>`.
pub struct SharedRwLock<T> {
    inner: Arc<RwLock<T>>,
}

impl<T> SharedRwLock<T> {
    /// Creates a new shared read-write lock.
    pub fn new(data: T) -> Self {
        Self {
            inner: Arc::new(RwLock::new(data)),
        }
    }

    /// Acquires a read lock.
    pub fn read(&self) -> RwLockReadGuard<'_, T> {
        self.inner.read()
    }

    /// Attempts to acquire a read lock without blocking.
    pub fn try_read(&self) -> Option<RwLockReadGuard<'_, T>> {
        self.inner.try_read()
    }

    /// Acquires a write lock.
    pub fn write(&self) -> RwLockWriteGuard<'_, T> {
        self.inner.write()
    }

    /// Attempts to acquire a write lock without blocking.
    pub fn try_write(&self) -> Option<RwLockWriteGuard<'_, T>> {
        self.inner.try_write()
    }
}

impl<T> Clone for SharedRwLock<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl<T> Deref for SharedRwLock<T> {
    type Target = RwLock<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::thread;

    #[test]
    fn test_rwlock_read() {
        let lock = RwLock::new(42);
        let guard = lock.read();
        assert_eq!(*guard, 42);
    }

    #[test]
    fn test_rwlock_write() {
        let lock = RwLock::new(0);
        *lock.write() = 42;
        assert_eq!(*lock.read(), 42);
    }

    #[test]
    fn test_rwlock_multiple_readers() {
        let lock = RwLock::new(42);

        let r1 = lock.read();
        let r2 = lock.read();
        let r3 = lock.read();

        assert_eq!(*r1, 42);
        assert_eq!(*r2, 42);
        assert_eq!(*r3, 42);

        assert_eq!(lock.reader_count(), 3);
    }

    #[test]
    fn test_rwlock_try_read() {
        let lock = RwLock::new(42);

        let guard = lock.try_read();
        assert!(guard.is_some());

        // Second try_read should succeed for read locks
        let guard2 = lock.try_read();
        assert!(guard2.is_some());
    }

    #[test]
    fn test_rwlock_try_write() {
        let lock = RwLock::new(42);

        let guard = lock.try_write();
        assert!(guard.is_some());

        // try_write should fail while write lock is held
        let guard2 = lock.try_write();
        assert!(guard2.is_none());

        drop(guard);

        // Now it should succeed
        let guard3 = lock.try_write();
        assert!(guard3.is_some());
    }

    #[test]
    fn test_rwlock_concurrent_readers() {
        let lock = Arc::new(RwLock::new(1));
        let counter = Arc::new(AtomicUsize::new(0));

        let mut handles = vec![];

        for _ in 0..10 {
            let l = lock.clone();
            let c = counter.clone();
            handles.push(thread::spawn(move || {
                let guard = l.read();
                c.fetch_add(*guard, Ordering::SeqCst);
            }));
        }

        for handle in handles {
            handle.join().unwrap();
        }

        // Each thread should have added 1 to the counter
        assert_eq!(counter.load(Ordering::SeqCst), 10);
    }

    #[test]
    fn test_rwlock_concurrent_writers() {
        let lock = Arc::new(RwLock::new(0));

        let mut handles = vec![];

        for _ in 0..10 {
            let l = lock.clone();
            handles.push(thread::spawn(move || {
                let mut guard = l.write();
                *guard += 1;
            }));
        }

        for handle in handles {
            handle.join().unwrap();
        }

        assert_eq!(*lock.read(), 10);
    }

    #[test]
    fn test_rwlock_get_mut() {
        let mut lock = RwLock::new(42);
        *lock.get_mut() = 10;
        assert_eq!(*lock.read(), 10);
    }

    #[test]
    fn test_rwlock_into_inner() {
        let lock = RwLock::new(42);
        assert_eq!(lock.into_inner(), 42);
    }

    #[test]
    fn test_rwlock_default() {
        let lock: RwLock<i32> = Default::default();
        assert_eq!(*lock.read(), 0);
    }

    #[test]
    fn test_rwlock_from() {
        let lock: RwLock<i32> = 42.into();
        assert_eq!(*lock.read(), 42);
    }

    #[test]
    fn test_shared_rwlock() {
        let lock = SharedRwLock::new(0);
        let lock2 = lock.clone();

        thread::spawn(move || {
            *lock2.write() = 42;
        })
        .join()
        .unwrap();

        assert_eq!(*lock.read(), 42);
    }

    #[test]
    fn test_rwlock_debug() {
        let lock = RwLock::new(42);
        let debug_str = format!("{:?}", lock);
        assert!(debug_str.contains("RwLock"));
        assert!(debug_str.contains("42"));
    }

    #[test]
    fn test_rwlock_is_write_locked() {
        let lock = RwLock::new(42);
        assert!(!lock.is_write_locked());

        let guard = lock.write();
        assert!(lock.is_write_locked());

        drop(guard);
        assert!(!lock.is_write_locked());
    }
}
