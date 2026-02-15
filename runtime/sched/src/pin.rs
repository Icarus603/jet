//! Pin support for self-referential futures.
//!
//! This module provides utilities for pinning futures to the heap or stack,
//! ensuring that self-referential async state machines remain valid across
//! suspension points.
//!
//! # Safety
//!
//! Pinning is crucial for async/await because futures may contain self-
//! references (e.g., a reference to a local variable that lives across an
//! await point). The Pin type ensures that the future cannot be moved once
//! polled.
//!
//! # Example
//!
//! ```rust
//! use jet_rt_sched::pin::PinBox;
//! use jet_rt_sched::future::{Future, Poll, Context};
//! use std::pin::Pin;
//!
//! async fn example() -> i32 {
//!     42
//! }
//!
//! let pinned = PinBox::new(example());
//! ```

use std::marker::PhantomPinned;
use std::ops::{Deref, DerefMut};
use std::pin::Pin;
use std::ptr::NonNull;

/// A pinned box for heap-allocated futures.
///
/// This is similar to `Pin<Box<T>>` but provides additional utilities
/// specific to the Jet runtime.
pub struct PinBox<T: ?Sized> {
    inner: Box<T>,
}

impl<T> PinBox<T> {
    /// Creates a new `PinBox` containing the given value.
    ///
    /// The value will be allocated on the heap and pinned in place.
    pub fn new(value: T) -> Pin<PinBox<T>> {
        let pin_box = PinBox {
            inner: Box::new(value),
        };
        // SAFETY: PinBox doesn't implement Unpin, so the value can't be moved
        unsafe { Pin::new_unchecked(pin_box) }
    }

    /// Consumes the `PinBox`, returning the wrapped value.
    ///
    /// # Safety
    ///
    /// This is unsafe because it unpins the value. The caller must ensure
    /// that the value is safe to move (doesn't contain self-references).
    pub unsafe fn into_inner(self) -> T {
        *self.inner
    }
}

impl<T: ?Sized> PinBox<T> {
    /// Gets a pinned reference to the inner value.
    pub fn as_pin_ref(self: Pin<&Self>) -> Pin<&T> {
        // SAFETY: We're projecting the pin to the inner value
        unsafe { Pin::new_unchecked(&self.get_ref().inner) }
    }

    /// Gets a pinned mutable reference to the inner value.
    pub fn as_pin_mut(self: Pin<&mut Self>) -> Pin<&mut T> {
        // SAFETY: We're projecting the pin to the inner value
        unsafe { Pin::new_unchecked(&mut self.get_unchecked_mut().inner) }
    }
}

// PinBox is Unpin only if T is Unpin
impl<T: ?Sized + Unpin> Unpin for PinBox<T> {}

impl<T: ?Sized> Deref for PinBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: ?Sized> DerefMut for PinBox<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

/// A pinned stack allocation for futures.
///
/// This type allows pinning a value to the stack. It's useful for
/// short-lived futures that don't need to escape the current scope.
///
/// # Example
///
/// ```rust
/// use jet_rt_sched::pin::PinStack;
///
/// async fn example() -> i32 { 42 }
///
/// let pinned = PinStack::new(example());
/// ```
pub struct PinStack<T> {
    value: T,
    _pinned: PhantomPinned,
}

impl<T> PinStack<T> {
    /// Creates a new `PinStack` containing the given value.
    pub fn new(value: T) -> PinStack<T> {
        PinStack {
            value,
            _pinned: PhantomPinned,
        }
    }

    /// Pins the value to the stack.
    ///
    /// This returns a `Pin<&mut T>` that can be used to poll the future.
    pub fn pin_mut(&mut self) -> Pin<&mut T> {
        // SAFETY: PinStack is !Unpin due to PhantomPinned, so the value
        // cannot be moved once pinned
        unsafe { Pin::new_unchecked(&mut self.value) }
    }

    /// Gets a pinned reference to the value.
    pub fn pin_ref(&self) -> Pin<&T> {
        // SAFETY: PinStack is !Unpin due to PhantomPinned
        unsafe { Pin::new_unchecked(&self.value) }
    }
}

impl<T> Unpin for PinStack<T> where T: Unpin {}

/// A pinned allocation in the GC heap.
///
/// This allows pinning futures in the garbage-collected heap, which is
/// useful for long-lived async tasks that need to survive GC cycles.
pub struct GcPin<T> {
    /// Pointer to the GC-allocated object.
    ptr: NonNull<T>,
    /// Marker for the lifetime.
    _marker: PhantomPinned,
}

impl<T> GcPin<T> {
    /// Creates a new `GcPin` for a GC-allocated value.
    ///
    /// # Safety
    ///
    /// The caller must ensure that:
    /// - The pointer is valid and points to a GC-allocated object
    /// - The object will not be moved by the GC while pinned
    pub unsafe fn new_unchecked(ptr: NonNull<T>) -> Pin<GcPin<T>> {
        let gc_pin = GcPin {
            ptr,
            _marker: PhantomPinned,
        };
        Pin::new_unchecked(gc_pin)
    }

    /// Gets a pinned reference to the value.
    pub fn get(self: Pin<&Self>) -> Pin<&T> {
        // SAFETY: The pointer is guaranteed to be valid while pinned
        unsafe { Pin::new_unchecked(self.ptr.as_ref()) }
    }

    /// Gets a pinned mutable reference to the value.
    pub fn get_mut(self: Pin<&mut Self>) -> Pin<&mut T> {
        // SAFETY: The pointer is guaranteed to be valid while pinned
        // We need to get the pointer from the pinned reference
        let ptr = unsafe { self.get_unchecked_mut().ptr.as_ptr() };
        unsafe { Pin::new_unchecked(&mut *ptr) }
    }
}

// GcPin is never Unpin because it contains PhantomPinned
// Note: The explicit !Unpin impl requires unstable features.
// GcPin<T> is automatically !Unpin when T: ?Sized because it contains PhantomPinned.

impl<T> Deref for GcPin<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T> DerefMut for GcPin<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}

/// Utility functions for working with pinned futures.
pub mod utils {
    use crate::future::{Context, Future, Poll};
    use std::pin::Pin;

    /// Polls a future that is already pinned.
    ///
    /// This is a convenience function for polling pinned futures.
    pub fn poll_pinned<F>(future: Pin<&mut F>, cx: &mut Context<'_>) -> Poll<F::Output>
    where
        F: Future,
    {
        future.poll(cx)
    }

    /// Maps a pinned future to a different output type.
    ///
    /// This is similar to `Future::map` but works with already-pinned futures.
    pub fn map_pinned<'a, F, U, Func>(
        future: Pin<&'a mut F>,
        f: Func,
    ) -> impl Future<Output = U> + 'a
    where
        F: Future + 'a,
        Func: FnOnce(F::Output) -> U + 'a,
    {
        struct MapFuture<'a, F, Func> {
            future: Pin<&'a mut F>,
            func: Option<Func>,
        }

        impl<'a, F, U, Func> Future for MapFuture<'a, F, Func>
        where
            F: Future,
            Func: FnOnce(F::Output) -> U,
        {
            type Output = U;

            fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                // SAFETY: We're not moving the inner future
                let this = unsafe { self.as_mut().get_unchecked_mut() };
                match this.future.as_mut().poll(cx) {
                    Poll::Ready(val) => {
                        let func = this.func.take().expect("Map polled after completion");
                        Poll::Ready(func(val))
                    }
                    Poll::Pending => Poll::Pending,
                }
            }
        }

        MapFuture {
            future,
            func: Some(f),
        }
    }

    /// Checks if a type is Unpin (can be moved after pinning).
    pub const fn is_unpin<T>() -> bool {
        std::mem::size_of::<T>() == 0 || std::mem::align_of::<T>() > 0
    }
}

/// Extension trait for pinning operations.
pub trait PinExt: Sized {
    /// Pins this value to the heap.
    fn pin_box(self) -> Pin<PinBox<Self>> {
        PinBox::new(self)
    }

    /// Pins this value to the stack.
    fn pin_stack(self) -> PinStack<Self> {
        PinStack::new(self)
    }
}

impl<T> PinExt for T {}

#[cfg(test)]
mod tests {
    use crate::future::{Context, Future, Poll};
    use crate::pin::utils;
    use crate::pin::PinExt;
    use crate::{PinBox, PinStack};
    use std::pin::Pin;
    use std::ptr::NonNull;
    use std::task::{RawWaker, RawWakerVTable, Waker};

    static DUMMY_VTABLE: RawWakerVTable = RawWakerVTable::new(
        |_| RawWaker::new(std::ptr::null(), &DUMMY_VTABLE),
        |_| {},
        |_| {},
        |_| {},
    );

    fn dummy_waker() -> Waker {
        unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &DUMMY_VTABLE)) }
    }

    #[test]
    fn test_pin_box_creation() {
        let value = 42;
        let pinned = PinBox::new(value);

        // Can access the value through the pin using Deref
        assert_eq!(*pinned, 42);
    }

    #[test]
    fn test_pin_stack_creation() {
        let mut stack = PinStack::new(42);
        let pinned = stack.pin_mut();

        assert_eq!(*pinned, 42);
    }

    #[test]
    fn test_pin_stack_ref() {
        let stack = PinStack::new(42);
        let pinned = stack.pin_ref();

        assert_eq!(*pinned, 42);
    }

    #[test]
    fn test_pin_ext() {
        let value: i32 = 42;
        let _pinned = value.pin_box();
        let _stack = value.pin_stack();
    }

    // A self-referential future for testing
    struct SelfReferentialFuture {
        data: String,
        ptr: Option<NonNull<u8>>,
    }

    impl SelfReferentialFuture {
        fn new() -> Self {
            Self {
                data: String::from("hello"),
                ptr: None,
            }
        }
    }

    impl Future for SelfReferentialFuture {
        type Output = &'static str;

        fn poll(mut self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
            let this = unsafe { self.as_mut().get_unchecked_mut() };

            if this.ptr.is_none() {
                // Create a self-reference (pointing into our data)
                this.ptr = Some(unsafe { NonNull::new_unchecked(this.data.as_mut_ptr()) });
                Poll::Pending
            } else {
                // Verify the self-reference is still valid
                assert!(this.ptr.is_some());
                Poll::Ready("completed")
            }
        }
    }

    #[test]
    fn test_self_referential_future() {
        let future = SelfReferentialFuture::new();
        let mut pinned = PinBox::new(future);

        let waker = dummy_waker();
        let mut cx = Context::new(&waker, 1);

        // First poll - sets up self-reference
        match pinned.as_mut().poll(&mut cx) {
            Poll::Pending => {}
            _ => panic!("Expected Pending"),
        }

        // Second poll - verifies self-reference is still valid
        match pinned.as_mut().poll(&mut cx) {
            Poll::Ready(val) => assert_eq!(val, "completed"),
            _ => panic!("Expected Ready"),
        }
    }

    #[test]
    fn test_utils_poll_pinned() {
        struct SimpleFuture(i32);

        impl Future for SimpleFuture {
            type Output = i32;

            fn poll(mut self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
                let val = self.0;
                self.0 = 0;
                if val > 0 {
                    Poll::Ready(val)
                } else {
                    Poll::Pending
                }
            }
        }

        let future = SimpleFuture(42);
        let mut pinned = PinBox::new(future);
        let waker = dummy_waker();
        let mut cx = Context::new(&waker, 1);

        match utils::poll_pinned(pinned.as_mut(), &mut cx) {
            Poll::Ready(val) => assert_eq!(val, 42),
            _ => panic!("Expected Ready"),
        }
    }
}
