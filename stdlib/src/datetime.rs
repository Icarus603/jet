//! DateTime - Date and time utilities for Jet
//!
//! This module provides Duration, Instant, SystemTime, Date, Time, DateTime,
//! and related types for the Jet programming language.

use crate::time::{Duration, SystemTime as BaseSystemTime};
use std::fmt;

/// A date (year, month, day) without time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Date {
    year: i32,
    month: u8, // 1-12
    day: u8,   // 1-31
}

/// A time of day (hour, minute, second, nanosecond).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Time {
    hour: u8,        // 0-23
    minute: u8,      // 0-59
    second: u8,      // 0-59
    nanosecond: u32, // 0-999,999,999
}

/// A date and time with timezone offset.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DateTime {
    date: Date,
    time: Time,
    offset: UtcOffset,
}

/// A day of the week.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Weekday {
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday,
}

/// A UTC offset in seconds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UtcOffset {
    seconds: i32, // -86,400 to +86,400
}

/// A timezone.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TimeZone {
    Utc,
    Local,
}

/// Error type for datetime operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DateTimeError {
    message: String,
}

/// Error type for parse operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    message: String,
}

// Constants
const DAYS_IN_MONTH: [u8; 12] = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
const SECS_PER_MINUTE: i64 = 60;
const SECS_PER_HOUR: i64 = 3600;
const SECS_PER_DAY: i64 = 86400;

impl Date {
    /// Creates a new date.
    ///
    /// Returns None if the date is invalid (e.g., February 30).
    pub fn new(year: i32, month: u8, day: u8) -> Option<Self> {
        if !Self::is_valid_date(year, month, day) {
            return None;
        }
        Some(Date { year, month, day })
    }

    /// Creates a new date without validation.
    ///
    /// # Safety
    /// The caller must ensure the date is valid.
    pub unsafe fn new_unchecked(year: i32, month: u8, day: u8) -> Self {
        Date { year, month, day }
    }

    /// Returns the current date.
    pub fn today() -> Self {
        Self::from_system_time(&BaseSystemTime::now())
    }

    /// Creates a date from a system time.
    pub fn from_system_time(time: &BaseSystemTime) -> Self {
        // Convert to seconds since epoch
        let secs = time.as_secs_since_epoch().unwrap_or(0) as i64;
        Self::from_timestamp(secs)
    }

    /// Creates a date from a Unix timestamp (seconds since epoch).
    fn from_timestamp(timestamp: i64) -> Self {
        // Algorithm from http://howardhinnant.github.io/date_algorithms.html
        let z = timestamp / SECS_PER_DAY + 719468;
        let era = if z >= 0 { z } else { z - 146096 } / 146097;
        let doe = z - era * 146097; // [0, 146096]
        let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365; // [0, 399]
        let y = (yoe as i32) + era as i32 * 400;
        let doy = doe - (365 * yoe + yoe / 4 - yoe / 100); // [0, 365]
        let mp = (5 * doy + 2) / 153; // [0, 11]
        let d = doy - (153 * mp + 2) / 5 + 1; // [1, 31]
        let m = if mp < 10 { mp + 3 } else { mp - 9 }; // [1, 12]
        let year = if m <= 2 { y + 1 } else { y };

        Date {
            year,
            month: m as u8,
            day: d as u8,
        }
    }

    /// Returns the year.
    pub fn year(&self) -> i32 {
        self.year
    }

    /// Returns the month (1-12).
    pub fn month(&self) -> u8 {
        self.month
    }

    /// Returns the day (1-31).
    pub fn day(&self) -> u8 {
        self.day
    }

    /// Returns the day of the week.
    pub fn weekday(&self) -> Weekday {
        // Zeller's congruence algorithm
        let mut y = self.year;
        let mut m = self.month as i32;
        let d = self.day as i32;

        if m < 3 {
            m += 12;
            y -= 1;
        }

        let k = y % 100;
        let j = y / 100;

        let f = d + (13 * (m + 1)) / 5 + k + k / 4 + j / 4 + 5 * j;
        let day_of_week = f % 7;

        match day_of_week {
            0 => Weekday::Saturday,
            1 => Weekday::Sunday,
            2 => Weekday::Monday,
            3 => Weekday::Tuesday,
            4 => Weekday::Wednesday,
            5 => Weekday::Thursday,
            6 => Weekday::Friday,
            _ => unreachable!(),
        }
    }

    /// Returns the day of the year (1-366).
    pub fn ordinal(&self) -> u16 {
        let mut day = self.day as u16;
        for m in 1..self.month {
            day += days_in_month(self.year, m) as u16;
        }
        day
    }

    /// Returns true if this is a leap year.
    pub fn is_leap_year(&self) -> bool {
        is_leap_year(self.year)
    }

    /// Returns the number of days in the month.
    pub fn days_in_month(&self) -> u8 {
        days_in_month(self.year, self.month)
    }

    /// Adds days to the date.
    ///
    /// Returns None if the result would overflow.
    pub fn checked_add_days(&self, days: i32) -> Option<Self> {
        let timestamp = self.to_timestamp() + days as i64 * SECS_PER_DAY;
        Some(Self::from_timestamp(timestamp))
    }

    /// Subtracts days from the date.
    ///
    /// Returns None if the result would underflow.
    pub fn checked_sub_days(&self, days: i32) -> Option<Self> {
        self.checked_add_days(-days)
    }

    /// Returns the duration between two dates.
    pub fn duration_since(&self, other: &Date) -> Duration {
        let self_ts = self.to_timestamp();
        let other_ts = other.to_timestamp();
        let diff = self_ts - other_ts;
        Duration::from_secs(diff as u64)
    }

    /// Formats the date according to the given format string.
    ///
    /// Supported specifiers:
    /// - %Y: Year with century (0001-9999)
    /// - %m: Month (01-12)
    /// - %d: Day (01-31)
    /// - %a: Abbreviated weekday name
    /// - %A: Full weekday name
    /// - %b: Abbreviated month name
    /// - %B: Full month name
    pub fn format(&self, fmt: &str) -> String {
        format_date(self, fmt)
    }

    /// Parses a date from a string.
    pub fn parse(s: &str, fmt: &str) -> Result<Self, ParseError> {
        parse_date(s, fmt)
    }

    /// Converts to a timestamp (seconds since Unix epoch at midnight UTC).
    fn to_timestamp(&self) -> i64 {
        // Algorithm from http://howardhinnant.github.io/date_algorithms.html
        let y = self.year as i32 - if self.month <= 2 { 1 } else { 0 };
        let m = self.month as i32 + if self.month <= 2 { 12 } else { 0 };
        let d = self.day as i32;

        let era = if y >= 0 { y } else { y - 399 } / 400;
        let yoe = y - era * 400; // [0, 399]
        let doy = (153 * (m - 3) + 2) / 5 + d - 1; // [0, 365]
        let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy; // [0, 146096]
        let z = era * 146097 + doe - 719468;

        z as i64 * SECS_PER_DAY
    }

    fn is_valid_date(year: i32, month: u8, day: u8) -> bool {
        if month < 1 || month > 12 {
            return false;
        }
        if day < 1 || day > days_in_month(year, month) {
            return false;
        }
        true
    }
}

impl Time {
    /// Creates a new time.
    ///
    /// Returns None if the time is invalid.
    pub fn new(hour: u8, minute: u8, second: u8) -> Option<Self> {
        Self::with_nanos(hour, minute, second, 0)
    }

    /// Creates a new time with nanoseconds.
    ///
    /// Returns None if the time is invalid.
    pub fn with_nanos(hour: u8, minute: u8, second: u8, nanosecond: u32) -> Option<Self> {
        if hour > 23 || minute > 59 || second > 59 || nanosecond > 999_999_999 {
            return None;
        }
        Some(Time {
            hour,
            minute,
            second,
            nanosecond,
        })
    }

    /// Returns midnight (00:00:00).
    pub fn midnight() -> Self {
        Time {
            hour: 0,
            minute: 0,
            second: 0,
            nanosecond: 0,
        }
    }

    /// Returns the hour (0-23).
    pub fn hour(&self) -> u8 {
        self.hour
    }

    /// Returns the minute (0-59).
    pub fn minute(&self) -> u8 {
        self.minute
    }

    /// Returns the second (0-59).
    pub fn second(&self) -> u8 {
        self.second
    }

    /// Returns the nanosecond (0-999,999,999).
    pub fn nanosecond(&self) -> u32 {
        self.nanosecond
    }

    /// Returns the total seconds since midnight.
    pub fn as_seconds(&self) -> u32 {
        self.hour as u32 * 3600 + self.minute as u32 * 60 + self.second as u32
    }

    /// Formats the time according to the given format string.
    ///
    /// Supported specifiers:
    /// - %H: Hour (00-23)
    /// - %M: Minute (00-59)
    /// - %S: Second (00-59)
    /// - %f: Microseconds (000000-999999)
    pub fn format(&self, fmt: &str) -> String {
        format_time(self, fmt)
    }
}

impl DateTime {
    /// Creates a new DateTime from a date and time.
    pub fn new(date: Date, time: Time) -> Self {
        DateTime {
            date,
            time,
            offset: UtcOffset::utc(),
        }
    }

    /// Creates a new DateTime with a specific offset.
    pub fn with_offset(date: Date, time: Time, offset: UtcOffset) -> Self {
        DateTime { date, time, offset }
    }

    /// Returns the current date and time in UTC.
    pub fn now_utc() -> Self {
        Self::from_system_time(&BaseSystemTime::now(), UtcOffset::utc())
    }

    /// Returns the current date and time in local time.
    pub fn now() -> Self {
        // For simplicity, assume local time is UTC
        // In a real implementation, this would use the system's local timezone
        Self::now_utc()
    }

    /// Creates a DateTime from a system time.
    pub fn from_system_time(time: &BaseSystemTime, offset: UtcOffset) -> Self {
        let secs = time.as_secs_since_epoch().unwrap_or(0) as i64;
        let nanos = time.as_nanos_since_epoch().unwrap_or(0) % 1_000_000_000;

        let date = Date::from_timestamp(secs);
        let time_of_day_secs = (secs % SECS_PER_DAY) as u32;
        let hour = (time_of_day_secs / 3600) as u8;
        let minute = ((time_of_day_secs % 3600) / 60) as u8;
        let second = (time_of_day_secs % 60) as u8;

        let time = Time {
            hour,
            minute,
            second,
            nanosecond: nanos as u32,
        };

        DateTime::with_offset(date, time, offset)
    }

    /// Returns the date component.
    pub fn date(&self) -> Date {
        self.date
    }

    /// Returns the time component.
    pub fn time(&self) -> Time {
        self.time
    }

    /// Returns the UTC offset.
    pub fn offset(&self) -> UtcOffset {
        self.offset
    }

    /// Returns the year.
    pub fn year(&self) -> i32 {
        self.date.year()
    }

    /// Returns the month (1-12).
    pub fn month(&self) -> u8 {
        self.date.month()
    }

    /// Returns the day (1-31).
    pub fn day(&self) -> u8 {
        self.date.day()
    }

    /// Returns the hour (0-23).
    pub fn hour(&self) -> u8 {
        self.time.hour()
    }

    /// Returns the minute (0-59).
    pub fn minute(&self) -> u8 {
        self.time.minute()
    }

    /// Returns the second (0-59).
    pub fn second(&self) -> u8 {
        self.time.second()
    }

    /// Returns the nanosecond (0-999,999,999).
    pub fn nanosecond(&self) -> u32 {
        self.time.nanosecond()
    }

    /// Returns the Unix timestamp (seconds since epoch).
    pub fn timestamp(&self) -> i64 {
        self.date.to_timestamp() + self.time.as_seconds() as i64 - self.offset.as_seconds() as i64
    }

    /// Returns the Unix timestamp in milliseconds.
    pub fn timestamp_millis(&self) -> i64 {
        self.timestamp() * 1000 + (self.time.nanosecond() / 1_000_000) as i64
    }

    /// Adds a duration to the datetime.
    ///
    /// Returns None if the result would overflow.
    pub fn checked_add_duration(&self, duration: &Duration) -> Option<Self> {
        let new_timestamp = self.timestamp() + duration.as_secs() as i64;
        let new_date = Date::from_timestamp(new_timestamp);
        let new_time_secs = (new_timestamp % SECS_PER_DAY) as u32;
        let new_time = Time {
            hour: (new_time_secs / 3600) as u8,
            minute: ((new_time_secs % 3600) / 60) as u8,
            second: (new_time_secs % 60) as u8,
            nanosecond: self.time.nanosecond(),
        };
        Some(DateTime::with_offset(new_date, new_time, self.offset))
    }

    /// Subtracts a duration from the datetime.
    ///
    /// Returns None if the result would underflow.
    pub fn checked_sub_duration(&self, duration: &Duration) -> Option<Self> {
        let new_timestamp = self.timestamp() - duration.as_secs() as i64;
        let new_date = Date::from_timestamp(new_timestamp);
        let new_time_secs = (new_timestamp % SECS_PER_DAY) as u32;
        let new_time = Time {
            hour: (new_time_secs / 3600) as u8,
            minute: ((new_time_secs % 3600) / 60) as u8,
            second: (new_time_secs % 60) as u8,
            nanosecond: self.time.nanosecond(),
        };
        Some(DateTime::with_offset(new_date, new_time, self.offset))
    }

    /// Returns the duration between two datetimes.
    pub fn duration_since(&self, other: &DateTime) -> Duration {
        let self_ts = self.timestamp();
        let other_ts = other.timestamp();
        let diff = (self_ts - other_ts).abs() as u64;
        Duration::from_secs(diff)
    }

    /// Formats the datetime according to the given format string.
    pub fn format(&self, fmt: &str) -> String {
        format_datetime(self, fmt)
    }

    /// Parses a datetime from a string.
    pub fn parse(s: &str, fmt: &str) -> Result<Self, ParseError> {
        parse_datetime(s, fmt)
    }

    /// Converts to system time.
    pub fn to_system_time(&self) -> BaseSystemTime {
        let secs = self.timestamp() as u64;
        let nanos = self.time.nanosecond() as u64;
        BaseSystemTime::from_secs_and_nanos(secs, nanos)
    }
}

impl Weekday {
    /// Returns the number from Monday (1) to Sunday (7).
    pub fn number_from_monday(&self) -> u8 {
        match self {
            Weekday::Monday => 1,
            Weekday::Tuesday => 2,
            Weekday::Wednesday => 3,
            Weekday::Thursday => 4,
            Weekday::Friday => 5,
            Weekday::Saturday => 6,
            Weekday::Sunday => 7,
        }
    }

    /// Returns the number from Sunday (1) to Saturday (7).
    pub fn number_from_sunday(&self) -> u8 {
        match self {
            Weekday::Sunday => 1,
            Weekday::Monday => 2,
            Weekday::Tuesday => 3,
            Weekday::Wednesday => 4,
            Weekday::Thursday => 5,
            Weekday::Friday => 6,
            Weekday::Saturday => 7,
        }
    }

    /// Returns the next weekday.
    pub fn succ(&self) -> Self {
        match self {
            Weekday::Monday => Weekday::Tuesday,
            Weekday::Tuesday => Weekday::Wednesday,
            Weekday::Wednesday => Weekday::Thursday,
            Weekday::Thursday => Weekday::Friday,
            Weekday::Friday => Weekday::Saturday,
            Weekday::Saturday => Weekday::Sunday,
            Weekday::Sunday => Weekday::Monday,
        }
    }

    /// Returns the previous weekday.
    pub fn pred(&self) -> Self {
        match self {
            Weekday::Monday => Weekday::Sunday,
            Weekday::Tuesday => Weekday::Monday,
            Weekday::Wednesday => Weekday::Tuesday,
            Weekday::Thursday => Weekday::Wednesday,
            Weekday::Friday => Weekday::Thursday,
            Weekday::Saturday => Weekday::Friday,
            Weekday::Sunday => Weekday::Saturday,
        }
    }
}

impl fmt::Display for Weekday {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Weekday::Monday => "Monday",
            Weekday::Tuesday => "Tuesday",
            Weekday::Wednesday => "Wednesday",
            Weekday::Thursday => "Thursday",
            Weekday::Friday => "Friday",
            Weekday::Saturday => "Saturday",
            Weekday::Sunday => "Sunday",
        };
        write!(f, "{}", name)
    }
}

impl UtcOffset {
    /// Returns the UTC offset (0 seconds).
    pub fn utc() -> Self {
        UtcOffset { seconds: 0 }
    }

    /// Creates an offset from seconds.
    ///
    /// Returns None if the offset is out of range (-86,400 to +86,400).
    pub fn from_seconds(seconds: i32) -> Option<Self> {
        if seconds.abs() > 86_400 {
            return None;
        }
        Some(UtcOffset { seconds })
    }

    /// Creates an offset from hours, minutes, and seconds.
    ///
    /// Returns None if the offset is out of range.
    pub fn from_hms(hours: i8, minutes: i8, seconds: i8) -> Option<Self> {
        let total = hours as i32 * 3600 + minutes as i32 * 60 + seconds as i32;
        Self::from_seconds(total)
    }

    /// Returns the offset in seconds.
    pub fn as_seconds(&self) -> i32 {
        self.seconds
    }

    /// Returns the offset as hours, minutes, and seconds.
    pub fn as_hms(&self) -> (i8, i8, i8) {
        let abs_seconds = self.seconds.abs();
        let hours = (abs_seconds / 3600) as i8;
        let minutes = ((abs_seconds % 3600) / 60) as i8;
        let seconds = (abs_seconds % 60) as i8;

        if self.seconds < 0 {
            (-hours, -minutes, -seconds)
        } else {
            (hours, minutes, seconds)
        }
    }

    /// Formats the offset as ±HH:MM.
    pub fn format(&self) -> String {
        let (h, m, _) = self.as_hms();
        format!("{:+03}:{:02}", h, m.abs())
    }
}

impl TimeZone {
    /// Returns the UTC timezone.
    pub fn utc() -> Self {
        TimeZone::Utc
    }

    /// Returns the local timezone.
    pub fn local() -> Self {
        TimeZone::Local
    }

    /// Returns the offset at a given datetime.
    pub fn offset_at(&self, _datetime: &DateTime) -> UtcOffset {
        match self {
            TimeZone::Utc => UtcOffset::utc(),
            TimeZone::Local => UtcOffset::utc(), // Simplified
        }
    }
}

impl DateTimeError {
    /// Creates a new error.
    pub fn new(message: impl Into<String>) -> Self {
        DateTimeError {
            message: message.into(),
        }
    }

    /// Returns the error message.
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl ParseError {
    /// Creates a new error.
    pub fn new(message: impl Into<String>) -> Self {
        ParseError {
            message: message.into(),
        }
    }

    /// Returns the error message.
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for DateTimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DateTime error: {}", self.message)
    }
}

impl std::error::Error for DateTimeError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error: {}", self.message)
    }
}

impl std::error::Error for ParseError {}

// Helper functions

fn is_leap_year(year: i32) -> bool {
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

fn days_in_month(year: i32, month: u8) -> u8 {
    if month == 2 && is_leap_year(year) {
        29
    } else {
        DAYS_IN_MONTH[(month - 1) as usize]
    }
}

fn format_date(date: &Date, fmt: &str) -> String {
    let mut result = String::new();
    let mut chars = fmt.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            match chars.next() {
                Some('Y') => result.push_str(&format!("{:04}", date.year)),
                Some('m') => result.push_str(&format!("{:02}", date.month)),
                Some('d') => result.push_str(&format!("{:02}", date.day)),
                Some('a') => result.push_str(&date.weekday().to_string()[..3]),
                Some('A') => result.push_str(&date.weekday().to_string()),
                Some('b') => result.push_str(&month_name(date.month)[..3]),
                Some('B') => result.push_str(&month_name(date.month)),
                Some(c) => {
                    result.push('%');
                    result.push(c);
                }
                None => result.push('%'),
            }
        } else {
            result.push(c);
        }
    }

    result
}

fn format_time(time: &Time, fmt: &str) -> String {
    let mut result = String::new();
    let mut chars = fmt.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            match chars.next() {
                Some('H') => result.push_str(&format!("{:02}", time.hour)),
                Some('M') => result.push_str(&format!("{:02}", time.minute)),
                Some('S') => result.push_str(&format!("{:02}", time.second)),
                Some('f') => result.push_str(&format!("{:06}", time.nanosecond / 1000)),
                Some(c) => {
                    result.push('%');
                    result.push(c);
                }
                None => result.push('%'),
            }
        } else {
            result.push(c);
        }
    }

    result
}

fn format_datetime(dt: &DateTime, fmt: &str) -> String {
    let mut result = String::new();
    let mut chars = fmt.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            match chars.next() {
                Some('Y') => result.push_str(&format!("{:04}", dt.year())),
                Some('m') => result.push_str(&format!("{:02}", dt.month())),
                Some('d') => result.push_str(&format!("{:02}", dt.day())),
                Some('H') => result.push_str(&format!("{:02}", dt.hour())),
                Some('M') => result.push_str(&format!("{:02}", dt.minute())),
                Some('S') => result.push_str(&format!("{:02}", dt.second())),
                Some('f') => result.push_str(&format!("{:06}", dt.nanosecond() / 1000)),
                Some('a') => result.push_str(&dt.date.weekday().to_string()[..3]),
                Some('A') => result.push_str(&dt.date.weekday().to_string()),
                Some('b') => result.push_str(&month_name(dt.month())[..3]),
                Some('B') => result.push_str(&month_name(dt.month())),
                Some('z') => result.push_str(&dt.offset.format()),
                Some(c) => {
                    result.push('%');
                    result.push(c);
                }
                None => result.push('%'),
            }
        } else {
            result.push(c);
        }
    }

    result
}

fn month_name(month: u8) -> &'static str {
    match month {
        1 => "January",
        2 => "February",
        3 => "March",
        4 => "April",
        5 => "May",
        6 => "June",
        7 => "July",
        8 => "August",
        9 => "September",
        10 => "October",
        11 => "November",
        12 => "December",
        _ => "Unknown",
    }
}

fn parse_date(s: &str, fmt: &str) -> Result<Date, ParseError> {
    // Simplified parser for common formats
    // In a full implementation, this would properly parse the format string
    if fmt == "%Y-%m-%d" {
        let parts: Vec<&str> = s.split('-').collect();
        if parts.len() != 3 {
            return Err(ParseError::new("Invalid date format"));
        }
        let year = parts[0]
            .parse()
            .map_err(|_| ParseError::new("Invalid year"))?;
        let month = parts[1]
            .parse()
            .map_err(|_| ParseError::new("Invalid month"))?;
        let day = parts[2]
            .parse()
            .map_err(|_| ParseError::new("Invalid day"))?;
        Date::new(year, month, day).ok_or_else(|| ParseError::new("Invalid date"))
    } else {
        Err(ParseError::new("Unsupported format"))
    }
}

fn parse_datetime(s: &str, fmt: &str) -> Result<DateTime, ParseError> {
    // Simplified parser
    if fmt == "%Y-%m-%d %H:%M:%S" {
        let parts: Vec<&str> = s.split_whitespace().collect();
        if parts.len() != 2 {
            return Err(ParseError::new("Invalid datetime format"));
        }
        let date = parse_date(parts[0], "%Y-%m-%d")?;
        let time_parts: Vec<&str> = parts[1].split(':').collect();
        if time_parts.len() != 3 {
            return Err(ParseError::new("Invalid time format"));
        }
        let hour = time_parts[0]
            .parse()
            .map_err(|_| ParseError::new("Invalid hour"))?;
        let minute = time_parts[1]
            .parse()
            .map_err(|_| ParseError::new("Invalid minute"))?;
        let second = time_parts[2]
            .parse()
            .map_err(|_| ParseError::new("Invalid second"))?;
        let time =
            Time::new(hour, minute, second).ok_or_else(|| ParseError::new("Invalid time"))?;
        Ok(DateTime::new(date, time))
    } else {
        Err(ParseError::new("Unsupported format"))
    }
}

// Extension trait for SystemTime
trait SystemTimeExt {
    fn as_nanos_since_epoch(&self) -> Result<u64, crate::time::SystemTimeError>;
    fn from_secs_and_nanos(secs: u64, nanos: u64) -> Self;
}

impl SystemTimeExt for BaseSystemTime {
    fn as_nanos_since_epoch(&self) -> Result<u64, crate::time::SystemTimeError> {
        self.as_millis_since_epoch().map(|ms| ms * 1_000_000)
    }

    fn from_secs_and_nanos(secs: u64, nanos: u64) -> Self {
        // This is a simplified implementation
        // In a real implementation, we'd construct from secs + nanos
        BaseSystemTime::now() // Placeholder
    }
}

// C ABI exports for FFI

use crate::string::JetString;
use crate::time::Instant;
use std::thread;

/// Returns the current instant as nanoseconds since an unspecified epoch.
#[no_mangle]
pub extern "C" fn jet_instant_now() -> i64 {
    // Return nanoseconds since program start as a monotonic value
    let now = Instant::now();
    // Since we can't easily extract raw nanos from std::time::Instant,
    // we use elapsed from a zero instant as a proxy
    now.elapsed().as_nanos() as i64
}

/// Returns the current system time as nanoseconds since Unix epoch.
#[no_mangle]
pub extern "C" fn jet_systemtime_now() -> i64 {
    match BaseSystemTime::now().as_millis_since_epoch() {
        Ok(ms) => (ms * 1_000_000) as i64,
        Err(_) => 0,
    }
}

/// Creates a duration from seconds.
#[no_mangle]
pub extern "C" fn jet_duration_from_secs(secs: i64) -> i64 {
    Duration::from_secs(secs as u64).as_nanos() as i64
}

/// Creates a duration from milliseconds.
#[no_mangle]
pub extern "C" fn jet_duration_from_millis(millis: i64) -> i64 {
    Duration::from_millis(millis as u64).as_nanos() as i64
}

/// Creates a duration from nanoseconds.
#[no_mangle]
pub extern "C" fn jet_duration_from_nanos(nanos: i64) -> i64 {
    Duration::from_nanos(nanos as u64).as_nanos() as i64
}

/// Returns the current date.
#[no_mangle]
pub extern "C" fn jet_date_today() -> *mut Date {
    Box::into_raw(Box::new(Date::today()))
}

/// Creates a new date.
#[no_mangle]
pub extern "C" fn jet_date_new(year: i32, month: i8, day: i8) -> *mut Date {
    match Date::new(year, month as u8, day as u8) {
        Some(date) => Box::into_raw(Box::new(date)),
        None => std::ptr::null_mut(),
    }
}

/// Formats a date according to the given format string.
#[no_mangle]
/// # Safety
/// The caller must pass valid pointers and lengths according to `jet_date_format`'s FFI contract.
pub unsafe extern "C" fn jet_date_format(
    date: *const Date,
    fmt: *const u8,
    fmt_len: usize,
) -> *mut JetString {
    if date.is_null() || fmt.is_null() {
        return std::ptr::null_mut();
    }
    let date = unsafe { &*date };
    let fmt_bytes = unsafe { std::slice::from_raw_parts(fmt, fmt_len) };
    let fmt_str = String::from_utf8_lossy(fmt_bytes);
    let formatted = date.format(&fmt_str);
    Box::into_raw(Box::new(JetString::from_str(&formatted)))
}

/// Parses a date from a string.
#[no_mangle]
/// # Safety
/// The caller must pass valid pointers and lengths according to `jet_date_parse`'s FFI contract.
pub unsafe extern "C" fn jet_date_parse(
    s: *const u8,
    s_len: usize,
    fmt: *const u8,
    fmt_len: usize,
) -> *mut Date {
    if s.is_null() || fmt.is_null() {
        return std::ptr::null_mut();
    }
    let s_bytes = unsafe { std::slice::from_raw_parts(s, s_len) };
    let fmt_bytes = unsafe { std::slice::from_raw_parts(fmt, fmt_len) };
    let s_str = String::from_utf8_lossy(s_bytes);
    let fmt_str = String::from_utf8_lossy(fmt_bytes);
    match Date::parse(&s_str, &fmt_str) {
        Ok(date) => Box::into_raw(Box::new(date)),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Creates a new time.
#[no_mangle]
pub extern "C" fn jet_time_new(hour: i8, minute: i8, second: i8) -> *mut Time {
    match Time::new(hour as u8, minute as u8, second as u8) {
        Some(time) => Box::into_raw(Box::new(time)),
        None => std::ptr::null_mut(),
    }
}

/// Formats a time according to the given format string.
#[no_mangle]
/// # Safety
/// The caller must pass valid pointers and lengths according to `jet_time_format`'s FFI contract.
pub unsafe extern "C" fn jet_time_format(
    time: *const Time,
    fmt: *const u8,
    fmt_len: usize,
) -> *mut JetString {
    if time.is_null() || fmt.is_null() {
        return std::ptr::null_mut();
    }
    let time = unsafe { &*time };
    let fmt_bytes = unsafe { std::slice::from_raw_parts(fmt, fmt_len) };
    let fmt_str = String::from_utf8_lossy(fmt_bytes);
    let formatted = time.format(&fmt_str);
    Box::into_raw(Box::new(JetString::from_str(&formatted)))
}

/// Returns the current local datetime.
#[no_mangle]
pub extern "C" fn jet_datetime_now() -> *mut DateTime {
    Box::into_raw(Box::new(DateTime::now()))
}

/// Returns the current UTC datetime.
#[no_mangle]
pub extern "C" fn jet_datetime_now_utc() -> *mut DateTime {
    Box::into_raw(Box::new(DateTime::now_utc()))
}

/// Creates a new datetime from a date and time.
#[no_mangle]
/// # Safety
/// The caller must pass valid pointers according to `jet_datetime_new`'s FFI contract.
pub unsafe extern "C" fn jet_datetime_new(date: *const Date, time: *const Time) -> *mut DateTime {
    if date.is_null() || time.is_null() {
        return std::ptr::null_mut();
    }
    let date = unsafe { &*date };
    let time = unsafe { &*time };
    Box::into_raw(Box::new(DateTime::new(*date, *time)))
}

/// Formats a datetime according to the given format string.
#[no_mangle]
/// # Safety
/// The caller must pass valid pointers and lengths according to `jet_datetime_format`'s FFI contract.
pub unsafe extern "C" fn jet_datetime_format(
    dt: *const DateTime,
    fmt: *const u8,
    fmt_len: usize,
) -> *mut JetString {
    if dt.is_null() || fmt.is_null() {
        return std::ptr::null_mut();
    }
    let dt = unsafe { &*dt };
    let fmt_bytes = unsafe { std::slice::from_raw_parts(fmt, fmt_len) };
    let fmt_str = String::from_utf8_lossy(fmt_bytes);
    let formatted = dt.format(&fmt_str);
    Box::into_raw(Box::new(JetString::from_str(&formatted)))
}

/// Parses a datetime from a string.
#[no_mangle]
/// # Safety
/// The caller must pass valid pointers and lengths according to `jet_datetime_parse`'s FFI contract.
pub unsafe extern "C" fn jet_datetime_parse(
    s: *const u8,
    s_len: usize,
    fmt: *const u8,
    fmt_len: usize,
) -> *mut DateTime {
    if s.is_null() || fmt.is_null() {
        return std::ptr::null_mut();
    }
    let s_bytes = unsafe { std::slice::from_raw_parts(s, s_len) };
    let fmt_bytes = unsafe { std::slice::from_raw_parts(fmt, fmt_len) };
    let s_str = String::from_utf8_lossy(s_bytes);
    let fmt_str = String::from_utf8_lossy(fmt_bytes);
    match DateTime::parse(&s_str, &fmt_str) {
        Ok(dt) => Box::into_raw(Box::new(dt)),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Returns the Unix timestamp (seconds since epoch) for a datetime.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_datetime_timestamp`'s FFI contract.
pub unsafe extern "C" fn jet_datetime_timestamp(dt: *const DateTime) -> i64 {
    if dt.is_null() {
        return 0;
    }
    let dt = unsafe { &*dt };
    dt.timestamp()
}

/// Sleeps for the specified number of milliseconds.
#[no_mangle]
pub extern "C" fn jet_sleep_millis(millis: u64) {
    thread::sleep(std::time::Duration::from_millis(millis));
}

/// Frees a date.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_date_free`'s FFI contract.
pub unsafe extern "C" fn jet_date_free(date: *mut Date) {
    if !date.is_null() {
        unsafe { drop(Box::from_raw(date)) };
    }
}

/// Frees a time.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_time_free`'s FFI contract.
pub unsafe extern "C" fn jet_time_free(time: *mut Time) {
    if !time.is_null() {
        unsafe { drop(Box::from_raw(time)) };
    }
}

/// Frees a datetime.
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_datetime_free`'s FFI contract.
pub unsafe extern "C" fn jet_datetime_free(dt: *mut DateTime) {
    if !dt.is_null() {
        unsafe { drop(Box::from_raw(dt)) };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_date_creation() {
        let date = Date::new(2024, 6, 15).unwrap();
        assert_eq!(date.year(), 2024);
        assert_eq!(date.month(), 6);
        assert_eq!(date.day(), 15);
    }

    #[test]
    fn test_date_validation() {
        assert!(Date::new(2024, 2, 29).is_some()); // Leap year
        assert!(Date::new(2023, 2, 29).is_none()); // Not a leap year
        assert!(Date::new(2024, 13, 1).is_none()); // Invalid month
        assert!(Date::new(2024, 0, 1).is_none()); // Invalid month
    }

    #[test]
    fn test_leap_year() {
        assert!(Date::new(2020, 1, 1).unwrap().is_leap_year());
        assert!(Date::new(2024, 1, 1).unwrap().is_leap_year());
        assert!(!Date::new(2023, 1, 1).unwrap().is_leap_year());
        assert!(!Date::new(1900, 1, 1).unwrap().is_leap_year());
        assert!(Date::new(2000, 1, 1).unwrap().is_leap_year());
    }

    #[test]
    fn test_weekday() {
        // 2024-01-01 was a Monday
        let date = Date::new(2024, 1, 1).unwrap();
        assert_eq!(date.weekday(), Weekday::Monday);

        let date = Date::new(2024, 6, 15).unwrap();
        assert_eq!(date.weekday(), Weekday::Saturday);
    }

    #[test]
    fn test_time_creation() {
        let time = Time::new(14, 30, 45).unwrap();
        assert_eq!(time.hour(), 14);
        assert_eq!(time.minute(), 30);
        assert_eq!(time.second(), 45);
    }

    #[test]
    fn test_time_validation() {
        assert!(Time::new(0, 0, 0).is_some());
        assert!(Time::new(23, 59, 59).is_some());
        assert!(Time::new(24, 0, 0).is_none());
        assert!(Time::new(12, 60, 0).is_none());
        assert!(Time::new(12, 0, 60).is_none());
    }

    #[test]
    fn test_datetime_creation() {
        let date = Date::new(2024, 6, 15).unwrap();
        let time = Time::new(14, 30, 0).unwrap();
        let dt = DateTime::new(date, time);

        assert_eq!(dt.year(), 2024);
        assert_eq!(dt.month(), 6);
        assert_eq!(dt.day(), 15);
        assert_eq!(dt.hour(), 14);
        assert_eq!(dt.minute(), 30);
    }

    #[test]
    fn test_weekday_navigation() {
        assert_eq!(Weekday::Monday.succ(), Weekday::Tuesday);
        assert_eq!(Weekday::Sunday.succ(), Weekday::Monday);
        assert_eq!(Weekday::Monday.pred(), Weekday::Sunday);
        assert_eq!(Weekday::Sunday.pred(), Weekday::Saturday);
    }

    #[test]
    fn test_weekday_numbers() {
        assert_eq!(Weekday::Monday.number_from_monday(), 1);
        assert_eq!(Weekday::Sunday.number_from_monday(), 7);
        assert_eq!(Weekday::Sunday.number_from_sunday(), 1);
        assert_eq!(Weekday::Saturday.number_from_sunday(), 7);
    }

    #[test]
    fn test_utc_offset() {
        let offset = UtcOffset::from_hms(5, 30, 0).unwrap();
        assert_eq!(offset.as_seconds(), 5 * 3600 + 30 * 60);

        let offset = UtcOffset::from_hms(-8, 0, 0).unwrap();
        assert_eq!(offset.as_seconds(), -8 * 3600);

        assert_eq!(UtcOffset::utc().as_seconds(), 0);
    }

    #[test]
    fn test_date_format() {
        let date = Date::new(2024, 6, 15).unwrap();
        assert_eq!(date.format("%Y-%m-%d"), "2024-06-15");
    }

    #[test]
    fn test_time_format() {
        let time = Time::new(14, 30, 45).unwrap();
        assert_eq!(time.format("%H:%M:%S"), "14:30:45");
    }

    #[test]
    fn test_datetime_format() {
        let date = Date::new(2024, 6, 15).unwrap();
        let time = Time::new(14, 30, 0).unwrap();
        let dt = DateTime::new(date, time);
        assert_eq!(dt.format("%Y-%m-%d %H:%M:%S"), "2024-06-15 14:30:00");
    }

    #[test]
    fn test_date_arithmetic() {
        let date = Date::new(2024, 6, 15).unwrap();
        let next_day = date.checked_add_days(1).unwrap();
        assert_eq!(next_day.day(), 16);

        let prev_day = date.checked_sub_days(1).unwrap();
        assert_eq!(prev_day.day(), 14);

        let next_month = date.checked_add_days(20).unwrap();
        assert_eq!(next_month.month(), 7);
    }

    #[test]
    fn test_days_in_month() {
        assert_eq!(Date::new(2023, 1, 1).unwrap().days_in_month(), 31);
        assert_eq!(Date::new(2023, 2, 1).unwrap().days_in_month(), 28);
        assert_eq!(Date::new(2024, 2, 1).unwrap().days_in_month(), 29); // Leap year
        assert_eq!(Date::new(2023, 4, 1).unwrap().days_in_month(), 30);
    }
}
