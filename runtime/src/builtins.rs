//! Builtin Functions for Jet
//!
//! This module provides C-exported implementations of Jet's builtin functions
//! like print, println, assert, etc.

use std::ffi::CStr;
use std::os::raw::c_char;

/// Prints a string to stdout without a newline
///
/// This is called from compiled Jet code via FFI.
/// The string must be a null-terminated C string.
///
/// # Safety
///
/// `s` must be either null or a valid NUL-terminated C string pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_print(s: *const c_char) {
    if s.is_null() {
        return;
    }

    unsafe {
        if let Ok(rust_str) = CStr::from_ptr(s).to_str() {
            print!("{}", rust_str);
        }
    }
}

/// Prints a string to stdout with a newline
///
/// This is called from compiled Jet code via FFI.
/// The string must be a null-terminated C string.
///
/// # Safety
///
/// `s` must be either null or a valid NUL-terminated C string pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_println(s: *const c_char) {
    if s.is_null() {
        println!();
        return;
    }

    unsafe {
        if let Ok(rust_str) = CStr::from_ptr(s).to_str() {
            println!("{}", rust_str);
        }
    }
}

/// Prints a string to stderr without a newline
///
/// # Safety
///
/// `s` must be either null or a valid NUL-terminated C string pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_eprint(s: *const c_char) {
    if s.is_null() {
        return;
    }

    unsafe {
        if let Ok(rust_str) = CStr::from_ptr(s).to_str() {
            eprint!("{}", rust_str);
        }
    }
}

/// Prints a string to stderr with a newline
///
/// # Safety
///
/// `s` must be either null or a valid NUL-terminated C string pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_eprintln(s: *const c_char) {
    if s.is_null() {
        eprintln!();
        return;
    }

    unsafe {
        if let Ok(rust_str) = CStr::from_ptr(s).to_str() {
            eprintln!("{}", rust_str);
        }
    }
}

/// Prints an integer to stdout with a newline
#[no_mangle]
pub extern "C" fn jet_print_int(n: i64) {
    println!("{}", n);
}

/// Prints a float to stdout with a newline
#[no_mangle]
pub extern "C" fn jet_print_float(f: f64) {
    println!("{}", f);
}

/// Prints a boolean to stdout with a newline
#[no_mangle]
pub extern "C" fn jet_print_bool(b: bool) {
    println!("{}", b);
}

/// Panics with a message
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_panic(s: *const c_char) -> ! {
    if s.is_null() {
        panic!("panic called with null message");
    }

    unsafe {
        if let Ok(rust_str) = CStr::from_ptr(s).to_str() {
            panic!("{}", rust_str);
        } else {
            panic!("panic called with invalid UTF-8");
        }
    }
}
