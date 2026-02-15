//! Context switching for AArch64 (ARM64) architecture.
//!
//! This module provides low-level context switching primitives for M:N threading.
//! It handles saving and restoring CPU registers during task switches.

use std::fmt;

/// Task context for switching on AArch64.
///
/// Stores the saved register state of a task. When a task is suspended,
/// its CPU registers are saved here. When resumed, the registers are restored.
///
/// AArch64 callee-saved registers: x19-x29, sp, lr (x30)
#[derive(Clone, Copy)]
#[repr(C)]
pub struct Context {
    pub x19: usize, // Callee-saved register
    pub x20: usize, // Callee-saved register
    pub x21: usize, // Callee-saved register
    pub x22: usize, // Callee-saved register
    pub x23: usize, // Callee-saved register
    pub x24: usize, // Callee-saved register
    pub x25: usize, // Callee-saved register
    pub x26: usize, // Callee-saved register
    pub x27: usize, // Callee-saved register
    pub x28: usize, // Callee-saved register
    pub x29: usize, // Frame pointer (fp)
    pub sp: usize,  // Stack pointer
    pub lr: usize,  // Link register (x30)
}

extern "C" {
    fn jet_context_init(ctx: *mut Context, stack_top: *mut u8, entry_point: *const ());
    fn jet_context_switch(current: *mut Context, next: *const Context);
    fn jet_get_stack_pointer() -> usize;
}

impl Context {
    /// Creates a new empty context.
    ///
    /// The context is zero-initialized and must be properly set up
    /// before use with `init`.
    pub const fn new() -> Self {
        Self {
            x19: 0,
            x20: 0,
            x21: 0,
            x22: 0,
            x23: 0,
            x24: 0,
            x25: 0,
            x26: 0,
            x27: 0,
            x28: 0,
            x29: 0,
            sp: 0,
            lr: 0,
        }
    }

    /// Initializes a context for a new task.
    ///
    /// # Safety
    ///
    /// - `stack_top` must point to valid, writable memory
    /// - `entry_point` must be a valid function pointer
    /// - The stack must be properly aligned (16-byte boundary for AArch64)
    pub unsafe fn init(&mut self, stack_top: *mut u8, entry_point: *const ()) {
        jet_context_init(self, stack_top, entry_point);
    }

    /// Switches from the current context to the target context.
    ///
    /// # Safety
    ///
    /// - Both contexts must be properly initialized
    /// - `current` must point to writable memory where current registers will be saved
    /// - `next` must point to a valid context to restore
    /// - This function does not return normally - execution continues at `next`
    /// - Stack must have sufficient space for the switch
    pub unsafe fn switch(current: *mut Context, next: *const Context) {
        jet_context_switch(current, next);
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Context")
            .field("x19", &format_args!("0x{:016x}", self.x19))
            .field("x20", &format_args!("0x{:016x}", self.x20))
            .field("x21", &format_args!("0x{:016x}", self.x21))
            .field("x22", &format_args!("0x{:016x}", self.x22))
            .field("x23", &format_args!("0x{:016x}", self.x23))
            .field("x24", &format_args!("0x{:016x}", self.x24))
            .field("x25", &format_args!("0x{:016x}", self.x25))
            .field("x26", &format_args!("0x{:016x}", self.x26))
            .field("x27", &format_args!("0x{:016x}", self.x27))
            .field("x28", &format_args!("0x{:016x}", self.x28))
            .field("x29", &format_args!("0x{:016x}", self.x29))
            .field("sp", &format_args!("0x{:016x}", self.sp))
            .field("lr", &format_args!("0x{:016x}", self.lr))
            .finish()
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

/// Gets the current stack pointer.
///
/// This is useful for debugging and stack overflow checks.
pub fn current_stack_pointer() -> usize {
    unsafe { jet_get_stack_pointer() }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_new() {
        let ctx = Context::new();
        assert_eq!(ctx.sp, 0);
        assert_eq!(ctx.lr, 0);
    }

    #[test]
    fn test_context_debug() {
        let ctx = Context::new();
        let debug_str = format!("{:?}", ctx);
        assert!(debug_str.contains("Context"));
        assert!(debug_str.contains("sp"));
        assert!(debug_str.contains("lr"));
    }

    #[test]
    fn test_stack_pointer() {
        let sp = current_stack_pointer();
        // Stack pointer should be a valid address (non-zero and reasonably aligned)
        assert_ne!(sp, 0);
        #[cfg(target_arch = "aarch64")]
        assert_eq!(
            sp % 16,
            0,
            "Stack pointer should be 16-byte aligned on AArch64"
        );

        #[cfg(target_arch = "x86_64")]
        assert_eq!(
            sp % 16,
            8,
            "Stack pointer should be 8 mod 16 inside an x86_64 function"
        );
    }
}
