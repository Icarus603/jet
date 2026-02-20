//! Jet Standard Library - Math Functions
//!
//! This module provides FFI exports for mathematical functions.
//! All functions operate on f64 values and delegate to std::f64 methods.

// Trigonometry functions

/// Calculate the sine of a number (in radians).
#[no_mangle]
pub extern "C" fn jet_math_sin(x: f64) -> f64 {
    x.sin()
}

/// Calculate the cosine of a number (in radians).
#[no_mangle]
pub extern "C" fn jet_math_cos(x: f64) -> f64 {
    x.cos()
}

/// Calculate the tangent of a number (in radians).
#[no_mangle]
pub extern "C" fn jet_math_tan(x: f64) -> f64 {
    x.tan()
}

/// Calculate the arcsine of a number. Returns value in radians.
#[no_mangle]
pub extern "C" fn jet_math_asin(x: f64) -> f64 {
    x.asin()
}

/// Calculate the arccosine of a number. Returns value in radians.
#[no_mangle]
pub extern "C" fn jet_math_acos(x: f64) -> f64 {
    x.acos()
}

/// Calculate the arctangent of a number. Returns value in radians.
#[no_mangle]
pub extern "C" fn jet_math_atan(x: f64) -> f64 {
    x.atan()
}

/// Calculate the four-quadrant arctangent of y and x.
#[no_mangle]
pub extern "C" fn jet_math_atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

// Logarithm and exponential functions

/// Calculate the natural logarithm of a number.
#[no_mangle]
pub extern "C" fn jet_math_log(x: f64) -> f64 {
    x.ln()
}

/// Calculate the base-10 logarithm of a number.
#[no_mangle]
pub extern "C" fn jet_math_log10(x: f64) -> f64 {
    x.log10()
}

/// Calculate the base-2 logarithm of a number.
#[no_mangle]
pub extern "C" fn jet_math_log2(x: f64) -> f64 {
    x.log2()
}

/// Calculate e raised to the power of x.
#[no_mangle]
pub extern "C" fn jet_math_exp(x: f64) -> f64 {
    x.exp()
}

// Power and root functions

/// Calculate the square root of a number.
#[no_mangle]
pub extern "C" fn jet_math_sqrt(x: f64) -> f64 {
    x.sqrt()
}

/// Raise a number to a power.
#[no_mangle]
pub extern "C" fn jet_math_pow(base: f64, exp: f64) -> f64 {
    base.powf(exp)
}

// Rounding functions

/// Round down to the nearest integer.
#[no_mangle]
pub extern "C" fn jet_math_floor(x: f64) -> f64 {
    x.floor()
}

/// Round up to the nearest integer.
#[no_mangle]
pub extern "C" fn jet_math_ceil(x: f64) -> f64 {
    x.ceil()
}

/// Round to the nearest integer (half away from zero).
#[no_mangle]
pub extern "C" fn jet_math_round(x: f64) -> f64 {
    x.round()
}

// Other functions

/// Calculate the absolute value of a number.
#[no_mangle]
pub extern "C" fn jet_math_abs(x: f64) -> f64 {
    x.abs()
}

/// Returns the sign of a number (-1.0, 0.0, or 1.0).
#[no_mangle]
pub extern "C" fn jet_math_signum(x: f64) -> f64 {
    x.signum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trig() {
        assert!((jet_math_sin(0.0) - 0.0).abs() < 1e-10);
        assert!((jet_math_sin(std::f64::consts::PI / 2.0) - 1.0).abs() < 1e-10);
        assert!((jet_math_cos(0.0) - 1.0).abs() < 1e-10);
        assert!((jet_math_cos(std::f64::consts::PI) + 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_log_exp() {
        assert!((jet_math_log(1.0) - 0.0).abs() < 1e-10);
        assert!((jet_math_log10(10.0) - 1.0).abs() < 1e-10);
        assert!((jet_math_log2(2.0) - 1.0).abs() < 1e-10);
        assert!((jet_math_exp(0.0) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_power() {
        assert!((jet_math_sqrt(4.0) - 2.0).abs() < 1e-10);
        assert!((jet_math_pow(2.0, 3.0) - 8.0).abs() < 1e-10);
    }

    #[test]
    fn test_rounding() {
        assert_eq!(jet_math_floor(2.7), 2.0);
        assert_eq!(jet_math_ceil(2.3), 3.0);
        assert_eq!(jet_math_round(2.5), 3.0);
        assert_eq!(jet_math_round(2.4), 2.0);
    }

    #[test]
    fn test_other() {
        assert_eq!(jet_math_abs(-5.0), 5.0);
        assert_eq!(jet_math_abs(5.0), 5.0);
        assert_eq!(jet_math_signum(-5.0), -1.0);
        assert_eq!(jet_math_signum(5.0), 1.0);
        // Note: Rust's signum(0.0) returns 1.0, not 0.0
        assert_eq!(jet_math_signum(0.0), 1.0);
        assert_eq!(jet_math_signum(-0.0), -1.0);
    }
}
