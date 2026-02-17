//! Time - Time utilities for Jet
//!
//! This module provides duration, instant, and system time types
//! for the Jet programming language.

use std::time::{Duration as StdDuration, Instant as StdInstant, SystemTime as StdSystemTime};

/// A duration representing a span of time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Duration {
    /// Duration in nanoseconds
    nanos: u64,
}

/// A point in time (monotonic clock).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Instant {
    /// The underlying instant
    inner: StdInstant,
}

/// A point in time (system clock).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SystemTime {
    /// The underlying system time
    inner: StdSystemTime,
}

/// Error type for system time operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SystemTimeError {
    /// Error message
    message: String,
}

impl Duration {
    /// Creates a duration from nanoseconds.
    pub fn from_nanos(nanos: u64) -> Self {
        Duration { nanos }
    }

    /// Creates a duration from microseconds.
    pub fn from_micros(micros: u64) -> Self {
        Duration {
            nanos: micros.saturating_mul(1000),
        }
    }

    /// Creates a duration from milliseconds.
    pub fn from_millis(millis: u64) -> Self {
        Duration {
            nanos: millis.saturating_mul(1_000_000),
        }
    }

    /// Creates a duration from seconds.
    pub fn from_secs(secs: u64) -> Self {
        Duration {
            nanos: secs.saturating_mul(1_000_000_000),
        }
    }

    /// Creates a duration from minutes.
    pub fn from_mins(mins: u64) -> Self {
        Duration {
            nanos: mins.saturating_mul(60).saturating_mul(1_000_000_000),
        }
    }

    /// Creates a duration from hours.
    pub fn from_hours(hours: u64) -> Self {
        Duration {
            nanos: hours.saturating_mul(3600).saturating_mul(1_000_000_000),
        }
    }

    /// Creates a duration from days.
    pub fn from_days(days: u64) -> Self {
        Duration {
            nanos: days.saturating_mul(86400).saturating_mul(1_000_000_000),
        }
    }

    /// Returns the duration in nanoseconds.
    pub fn as_nanos(&self) -> u64 {
        self.nanos
    }

    /// Returns the duration in microseconds.
    pub fn as_micros(&self) -> u64 {
        self.nanos / 1000
    }

    /// Returns the duration in milliseconds.
    pub fn as_millis(&self) -> u64 {
        self.nanos / 1_000_000
    }

    /// Returns the duration in seconds.
    pub fn as_secs(&self) -> u64 {
        self.nanos / 1_000_000_000
    }

    /// Returns the seconds component of the duration.
    ///
    /// This is the whole number of seconds represented by the duration.
    pub fn secs(&self) -> u64 {
        self.nanos / 1_000_000_000
    }

    /// Returns the nanoseconds component of the duration.
    ///
    /// This is the fractional part of the duration in nanoseconds,
    /// always less than one billion.
    pub fn nanos(&self) -> u32 {
        (self.nanos % 1_000_000_000) as u32
    }

    /// Returns the duration in minutes.
    pub fn as_mins(&self) -> u64 {
        self.nanos / (60 * 1_000_000_000)
    }

    /// Returns the duration in hours.
    pub fn as_hours(&self) -> u64 {
        self.nanos / (3600 * 1_000_000_000)
    }

    /// Returns the duration in days.
    pub fn as_days(&self) -> u64 {
        self.nanos / (86400 * 1_000_000_000)
    }

    /// Returns the sub-second nanoseconds component.
    pub fn subsec_nanos(&self) -> u32 {
        (self.nanos % 1_000_000_000) as u32
    }

    /// Returns the sub-second milliseconds component.
    pub fn subsec_millis(&self) -> u32 {
        ((self.nanos % 1_000_000_000) / 1_000_000) as u32
    }

    /// Returns the sub-second microseconds component.
    pub fn subsec_micros(&self) -> u32 {
        ((self.nanos % 1_000_000_000) / 1000) as u32
    }

    /// Returns true if the duration is zero.
    pub fn is_zero(&self) -> bool {
        self.nanos == 0
    }

    /// Adds two durations.
    pub fn add(&self, other: &Duration) -> Duration {
        Duration {
            nanos: self.nanos.saturating_add(other.nanos),
        }
    }

    /// Subtracts another duration from this one.
    pub fn sub(&self, other: &Duration) -> Duration {
        Duration {
            nanos: self.nanos.saturating_sub(other.nanos),
        }
    }

    /// Multiplies the duration by a scalar.
    pub fn mul(&self, scalar: u64) -> Duration {
        Duration {
            nanos: self.nanos.saturating_mul(scalar),
        }
    }

    /// Divides the duration by a scalar.
    pub fn div(&self, scalar: u64) -> Duration {
        if scalar == 0 {
            Duration { nanos: u64::MAX }
        } else {
            Duration {
                nanos: self.nanos / scalar,
            }
        }
    }

    /// Returns the duration as a floating-point number of seconds.
    pub fn as_secs_f64(&self) -> f64 {
        self.nanos as f64 / 1_000_000_000.0
    }

    /// Returns the duration as a floating-point number of seconds.
    pub fn as_secs_f32(&self) -> f32 {
        self.nanos as f32 / 1_000_000_000.0
    }

    /// Zero duration.
    pub const ZERO: Duration = Duration { nanos: 0 };

    /// Maximum duration.
    pub const MAX: Duration = Duration { nanos: u64::MAX };
}

impl Default for Duration {
    fn default() -> Self {
        Duration::ZERO
    }
}

impl From<StdDuration> for Duration {
    fn from(d: StdDuration) -> Self {
        Duration {
            nanos: d.as_nanos() as u64,
        }
    }
}

impl From<Duration> for StdDuration {
    fn from(d: Duration) -> Self {
        StdDuration::from_nanos(d.nanos)
    }
}

impl Instant {
    /// Returns the current instant.
    pub fn now() -> Self {
        Instant {
            inner: StdInstant::now(),
        }
    }

    /// Returns the duration since another instant.
    ///
    /// Returns None if `earlier` is later than `self`.
    pub fn duration_since(&self, earlier: &Instant) -> Option<Duration> {
        self.inner
            .checked_duration_since(earlier.inner)
            .map(|d| d.into())
    }

    /// Returns the elapsed time since this instant was created.
    pub fn elapsed(&self) -> Duration {
        self.inner.elapsed().into()
    }

    /// Adds a duration to this instant.
    pub fn add(&self, duration: &Duration) -> Option<Instant> {
        self.inner
            .checked_add(StdDuration::from(*duration))
            .map(|inner| Instant { inner })
    }

    /// Subtracts a duration from this instant.
    pub fn sub(&self, duration: &Duration) -> Option<Instant> {
        self.inner
            .checked_sub(StdDuration::from(*duration))
            .map(|inner| Instant { inner })
    }
}

impl SystemTime {
    /// Returns the current system time.
    pub fn now() -> Self {
        SystemTime {
            inner: StdSystemTime::now(),
        }
    }

    /// Returns the duration since the UNIX epoch.
    pub fn duration_since_epoch(&self) -> Result<Duration, SystemTimeError> {
        match self.inner.duration_since(StdSystemTime::UNIX_EPOCH) {
            Ok(d) => Ok(d.into()),
            Err(_) => Err(SystemTimeError {
                message: "System time is before UNIX epoch".to_string(),
            }),
        }
    }

    /// Returns the duration since another system time.
    pub fn duration_since(&self, earlier: &SystemTime) -> Result<Duration, SystemTimeError> {
        match self.inner.duration_since(earlier.inner) {
            Ok(d) => Ok(d.into()),
            Err(_) => Err(SystemTimeError {
                message: "Earlier time is later than this time".to_string(),
            }),
        }
    }

    /// Adds a duration to this system time.
    pub fn add(&self, duration: &Duration) -> Option<SystemTime> {
        self.inner
            .checked_add(StdDuration::from(*duration))
            .map(|inner| SystemTime { inner })
    }

    /// Subtracts a duration from this system time.
    pub fn sub(&self, duration: &Duration) -> Option<SystemTime> {
        self.inner
            .checked_sub(StdDuration::from(*duration))
            .map(|inner| SystemTime { inner })
    }

    /// Returns the time as seconds since the UNIX epoch.
    pub fn as_secs_since_epoch(&self) -> Result<u64, SystemTimeError> {
        self.duration_since_epoch().map(|d| d.as_secs())
    }

    /// Returns the time as milliseconds since the UNIX epoch.
    pub fn as_millis_since_epoch(&self) -> Result<u64, SystemTimeError> {
        self.duration_since_epoch().map(|d| d.as_millis())
    }
}

impl SystemTimeError {
    /// Returns the error message.
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl std::fmt::Display for SystemTimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SystemTime error: {}", self.message)
    }
}

impl std::error::Error for SystemTimeError {}

/// Sleeps for the specified duration.
///
/// This function blocks the current thread.
pub fn sleep(duration: &Duration) {
    std::thread::sleep(StdDuration::from(*duration));
}

/// Sleeps for the specified number of milliseconds.
pub fn sleep_millis(millis: u64) {
    std::thread::sleep(StdDuration::from_millis(millis));
}

/// Sleeps for the specified number of seconds.
pub fn sleep_secs(secs: u64) {
    std::thread::sleep(StdDuration::from_secs(secs));
}

/// Returns the current time as seconds since the UNIX epoch.
pub fn now_secs() -> u64 {
    SystemTime::now().as_secs_since_epoch().unwrap_or(0)
}

/// Returns the current time as milliseconds since the UNIX epoch.
pub fn now_millis() -> u64 {
    SystemTime::now().as_millis_since_epoch().unwrap_or(0)
}

/// Measures the execution time of a function.
pub fn measure<F, T>(f: F) -> (T, Duration)
where
    F: FnOnce() -> T,
{
    let start = Instant::now();
    let result = f();
    let elapsed = start.elapsed();
    (result, elapsed)
}

/// Waits until a condition is true or a timeout occurs.
pub fn wait_until<F>(condition: F, timeout: &Duration) -> bool
where
    F: Fn() -> bool,
{
    let start = Instant::now();
    while !condition() {
        if start.elapsed().as_nanos() >= timeout.as_nanos() {
            return false;
        }
        sleep(&Duration::from_millis(1));
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_duration_creation() {
        let d = Duration::from_secs(5);
        assert_eq!(d.as_secs(), 5);
        assert_eq!(d.as_millis(), 5000);
    }

    #[test]
    fn test_duration_from_millis() {
        let d = Duration::from_millis(1500);
        assert_eq!(d.as_secs(), 1);
        assert_eq!(d.subsec_millis(), 500);
    }

    #[test]
    fn test_duration_arithmetic() {
        let d1 = Duration::from_secs(10);
        let d2 = Duration::from_secs(3);

        assert_eq!(d1.add(&d2).as_secs(), 13);
        assert_eq!(d1.sub(&d2).as_secs(), 7);
        assert_eq!(d1.mul(2).as_secs(), 20);
        assert_eq!(d1.div(2).as_secs(), 5);
    }

    #[test]
    fn test_duration_zero() {
        let d = Duration::ZERO;
        assert!(d.is_zero());
        assert_eq!(d.as_secs(), 0);
    }

    #[test]
    fn test_instant_now() {
        let start = Instant::now();
        sleep(&Duration::from_millis(10));
        let elapsed = start.elapsed();
        assert!(elapsed.as_millis() >= 10);
    }

    #[test]
    fn test_instant_duration_since() {
        let start = Instant::now();
        sleep(&Duration::from_millis(10));
        let end = Instant::now();

        let duration = end.duration_since(&start).unwrap();
        assert!(duration.as_millis() >= 10);
    }

    #[test]
    fn test_system_time_now() {
        let now = SystemTime::now();
        let secs = now.as_secs_since_epoch().unwrap();
        assert!(secs > 0);
    }

    #[test]
    fn test_measure() {
        let (_, duration) = measure(|| {
            sleep(&Duration::from_millis(10));
            42
        });
        assert!(duration.as_millis() >= 10);
    }

    #[test]
    fn test_duration_conversions() {
        let d = Duration::from_hours(2);
        assert_eq!(d.as_mins(), 120);
        assert_eq!(d.as_secs(), 7200);

        let d = Duration::from_days(1);
        assert_eq!(d.as_hours(), 24);
    }

    #[test]
    fn test_duration_floating_point() {
        let d = Duration::from_millis(1500);
        assert!((d.as_secs_f64() - 1.5).abs() < 0.001);
        assert!((d.as_secs_f32() - 1.5).abs() < 0.001);
    }
}
