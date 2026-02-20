//! IO - Input/Output operations for Jet
//!
//! This module provides file I/O, buffered I/O, and standard streams
//! for the Jet programming language.

use crate::string::JetString;
use std::fs::{self, File as StdFile, OpenOptions};
use std::io::{self, BufRead, BufReader, BufWriter, Read, Seek, SeekFrom, Write};
use std::path::Path;

/// A file handle for reading and writing files.
#[derive(Debug)]
pub struct File {
    /// The underlying std file handle
    inner: StdFile,
    /// Path to the file (stored for error messages)
    path: JetString,
}

/// A buffered reader for efficient reading.
pub struct BufferedReader {
    /// The underlying buffered reader
    inner: BufReader<StdFile>,
}

/// A buffered writer for efficient writing.
pub struct BufferedWriter {
    /// The underlying buffered writer
    inner: BufWriter<StdFile>,
}

/// File open mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileMode {
    /// Read-only mode
    Read,
    /// Write mode (creates or truncates)
    Write,
    /// Append mode
    Append,
    /// Read-write mode
    ReadWrite,
}

/// Result type for I/O operations
pub type IoResult<T> = std::result::Result<T, IoError>;

/// I/O error type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IoError {
    /// Error message
    pub message: JetString,
    /// Error kind
    pub kind: IoErrorKind,
}

/// I/O error kind
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IoErrorKind {
    /// File not found
    NotFound,
    /// Permission denied
    PermissionDenied,
    /// File already exists
    AlreadyExists,
    /// Invalid input
    InvalidInput,
    /// Unexpected end of file
    UnexpectedEof,
    /// Write zero bytes
    WriteZero,
    /// Interrupted
    Interrupted,
    /// Other error
    Other,
    /// Not a directory
    NotADirectory,
    /// Is a directory
    IsADirectory,
}

impl File {
    /// Opens a file with the specified mode.
    ///
    /// # Arguments
    /// * `path` - Path to the file
    /// * `mode` - File open mode
    ///
    /// # Returns
    /// Returns the file handle on success, or an error on failure.
    pub fn open(path: &str, mode: FileMode) -> IoResult<Self> {
        let path_obj = Path::new(path);

        let file = match mode {
            FileMode::Read => OpenOptions::new().read(true).open(path_obj),
            FileMode::Write => OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(path_obj),
            FileMode::Append => OpenOptions::new().append(true).create(true).open(path_obj),
            FileMode::ReadWrite => OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .open(path_obj),
        };

        match file {
            Ok(inner) => Ok(File {
                inner,
                path: JetString::from_str(path),
            }),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Creates a new file for writing (truncates if exists).
    pub fn create(path: &str) -> IoResult<Self> {
        Self::open(path, FileMode::Write)
    }

    /// Opens a file for reading.
    pub fn open_read(path: &str) -> IoResult<Self> {
        Self::open(path, FileMode::Read)
    }

    /// Opens a file for appending.
    pub fn open_append(path: &str) -> IoResult<Self> {
        Self::open(path, FileMode::Append)
    }

    /// Reads the entire file contents into a string.
    pub fn read_to_string(&mut self) -> IoResult<JetString> {
        let mut contents = String::new();
        match self.inner.read_to_string(&mut contents) {
            Ok(_) => Ok(JetString::from_str(&contents)),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Reads the entire file contents into a byte vector.
    pub fn read_to_bytes(&mut self) -> IoResult<Vec<u8>> {
        let mut contents = Vec::new();
        match self.inner.read_to_end(&mut contents) {
            Ok(_) => Ok(contents),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Writes a string slice to the file.
    pub fn write_str(&mut self, s: &str) -> IoResult<usize> {
        match self.inner.write(s.as_bytes()) {
            Ok(n) => Ok(n),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Writes bytes to the file.
    pub fn write_bytes(&mut self, bytes: &[u8]) -> IoResult<usize> {
        match self.inner.write(bytes) {
            Ok(n) => Ok(n),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Reads data from the file into a buffer.
    ///
    /// Returns the number of bytes read, or 0 if at end of file.
    ///
    /// # Arguments
    /// * `buf` - The buffer to read into
    ///
    /// # Returns
    /// The number of bytes read on success, or an error on failure.
    pub fn read(&mut self, buf: &mut [u8]) -> IoResult<usize> {
        match self.inner.read(buf) {
            Ok(n) => Ok(n),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Writes data to the file.
    ///
    /// Returns the number of bytes written.
    ///
    /// # Arguments
    /// * `buf` - The data to write
    ///
    /// # Returns
    /// The number of bytes written on success, or an error on failure.
    pub fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
        match self.inner.write(buf) {
            Ok(n) => Ok(n),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Closes the file.
    ///
    /// Consumes the file handle and ensures all data is flushed.
    ///
    /// # Returns
    /// Ok(()) on success, or an error on failure.
    pub fn close(self) -> IoResult<()> {
        // File is automatically closed when dropped
        // Just ensure any buffered data is flushed
        drop(self);
        Ok(())
    }

    /// Flushes any buffered data to the file.
    pub fn flush(&mut self) -> IoResult<()> {
        match self.inner.flush() {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Seeks to a position in the file.
    pub fn seek(&mut self, position: u64) -> IoResult<u64> {
        match self.inner.seek(SeekFrom::Start(position)) {
            Ok(pos) => Ok(pos),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Returns the current position in the file.
    pub fn position(&mut self) -> IoResult<u64> {
        match self.inner.stream_position() {
            Ok(pos) => Ok(pos),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Returns the file size in bytes.
    pub fn size(&self) -> IoResult<u64> {
        match self.inner.metadata() {
            Ok(meta) => Ok(meta.len()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Returns true if the file is a regular file.
    pub fn is_file(&self) -> bool {
        self.inner.metadata().map(|m| m.is_file()).unwrap_or(false)
    }

    /// Returns true if the file is a directory.
    pub fn is_dir(&self) -> bool {
        self.inner.metadata().map(|m| m.is_dir()).unwrap_or(false)
    }

    /// Returns the file path.
    pub fn path(&self) -> &str {
        self.path.as_str()
    }

    /// Reads a single byte from the file.
    pub fn read_byte(&mut self) -> IoResult<Option<u8>> {
        let mut buf = [0u8; 1];
        match self.inner.read_exact(&mut buf) {
            Ok(_) => Ok(Some(buf[0])),
            Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => Ok(None),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Reads up to `len` bytes from the file.
    pub fn read_bytes(&mut self, len: usize) -> IoResult<Vec<u8>> {
        let mut buf = vec![0u8; len];
        match self.inner.read(&mut buf) {
            Ok(n) => {
                buf.truncate(n);
                Ok(buf)
            }
            Err(e) => Err(map_io_error(e)),
        }
    }
}

impl BufferedReader {
    /// Creates a new buffered reader from a file.
    pub fn new(file: File) -> IoResult<Self> {
        // We need to create a new file handle since we consume the original
        let file = File::open_read(file.path())?;
        Ok(BufferedReader {
            inner: BufReader::new(file.inner),
        })
    }

    /// Creates a buffered reader from a file path.
    pub fn open(path: &str) -> IoResult<Self> {
        let file = File::open_read(path)?;
        Ok(BufferedReader {
            inner: BufReader::new(file.inner),
        })
    }

    /// Reads a line from the buffered reader.
    pub fn read_line(&mut self) -> IoResult<Option<JetString>> {
        let mut line = String::new();
        match self.inner.read_line(&mut line) {
            Ok(0) => Ok(None),
            Ok(_) => {
                // Remove trailing newline
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                Ok(Some(JetString::from_str(&line)))
            }
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Reads all lines into a vector.
    pub fn read_lines(&mut self) -> IoResult<Vec<JetString>> {
        let mut lines = Vec::new();
        while let Some(line) = self.read_line()? {
            lines.push(line);
        }
        Ok(lines)
    }

    /// Reads the entire contents into a string.
    pub fn read_to_string(&mut self) -> IoResult<JetString> {
        let mut contents = String::new();
        match self.inner.read_to_string(&mut contents) {
            Ok(_) => Ok(JetString::from_str(&contents)),
            Err(e) => Err(map_io_error(e)),
        }
    }
}

impl BufferedWriter {
    /// Creates a new buffered writer from a file.
    pub fn new(file: File) -> IoResult<Self> {
        let file = File::open(file.path(), FileMode::Write)?;
        Ok(BufferedWriter {
            inner: BufWriter::new(file.inner),
        })
    }

    /// Creates a buffered writer from a file path.
    pub fn create(path: &str) -> IoResult<Self> {
        let file = File::create(path)?;
        Ok(BufferedWriter {
            inner: BufWriter::new(file.inner),
        })
    }

    /// Creates a buffered writer for appending.
    pub fn append(path: &str) -> IoResult<Self> {
        let file = File::open_append(path)?;
        Ok(BufferedWriter {
            inner: BufWriter::new(file.inner),
        })
    }

    /// Writes a string slice.
    pub fn write_str(&mut self, s: &str) -> IoResult<()> {
        match self.inner.write_all(s.as_bytes()) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Writes a line (adds newline).
    pub fn write_line(&mut self, s: &str) -> IoResult<()> {
        match self.inner.write_all(s.as_bytes()) {
            Ok(_) => match self.inner.write_all(b"\n") {
                Ok(_) => Ok(()),
                Err(e) => Err(map_io_error(e)),
            },
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Writes bytes.
    pub fn write_bytes(&mut self, bytes: &[u8]) -> IoResult<()> {
        match self.inner.write_all(bytes) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Flushes the buffer.
    pub fn flush(&mut self) -> IoResult<()> {
        match self.inner.flush() {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }
}

/// Reads the entire file contents into a string.
pub fn read_file_to_string(path: &str) -> IoResult<JetString> {
    match fs::read_to_string(path) {
        Ok(contents) => Ok(JetString::from_str(&contents)),
        Err(e) => Err(map_io_error(e)),
    }
}

/// Reads the entire file contents into bytes.
pub fn read_file_to_bytes(path: &str) -> IoResult<Vec<u8>> {
    match fs::read(path) {
        Ok(contents) => Ok(contents),
        Err(e) => Err(map_io_error(e)),
    }
}

/// Writes a string to a file.
pub fn write_string_to_file(path: &str, contents: &str) -> IoResult<()> {
    match fs::write(path, contents) {
        Ok(_) => Ok(()),
        Err(e) => Err(map_io_error(e)),
    }
}

/// Writes bytes to a file.
pub fn write_bytes_to_file(path: &str, contents: &[u8]) -> IoResult<()> {
    match fs::write(path, contents) {
        Ok(_) => Ok(()),
        Err(e) => Err(map_io_error(e)),
    }
}

/// Checks if a file exists.
pub fn file_exists(path: &str) -> bool {
    Path::new(path).exists()
}

/// Checks if a path is a file.
pub fn is_file(path: &str) -> bool {
    Path::new(path).is_file()
}

/// Checks if a path is a directory.
pub fn is_dir(path: &str) -> bool {
    Path::new(path).is_dir()
}

/// Creates a directory.
pub fn create_dir(path: &str) -> IoResult<()> {
    match fs::create_dir(path) {
        Ok(_) => Ok(()),
        Err(e) => Err(map_io_error(e)),
    }
}

/// Creates a directory and all parent directories.
pub fn create_dir_all(path: &str) -> IoResult<()> {
    match fs::create_dir_all(path) {
        Ok(_) => Ok(()),
        Err(e) => Err(map_io_error(e)),
    }
}

/// Removes a file.
pub fn remove_file(path: &str) -> IoResult<()> {
    match fs::remove_file(path) {
        Ok(_) => Ok(()),
        Err(e) => Err(map_io_error(e)),
    }
}

/// Removes a directory.
pub fn remove_dir(path: &str) -> IoResult<()> {
    match fs::remove_dir(path) {
        Ok(_) => Ok(()),
        Err(e) => Err(map_io_error(e)),
    }
}

/// Removes a directory and all its contents.
pub fn remove_dir_all(path: &str) -> IoResult<()> {
    match fs::remove_dir_all(path) {
        Ok(_) => Ok(()),
        Err(e) => Err(map_io_error(e)),
    }
}

/// Returns the standard input stream reader.
///
/// Note: This returns a simple wrapper since we don't have
/// direct access to stdin in all contexts.
pub fn stdin() -> Stdin {
    Stdin
}

/// Returns the standard output stream writer.
pub fn stdout() -> Stdout {
    Stdout
}

/// Returns the standard error stream writer.
pub fn stderr() -> Stderr {
    Stderr
}

/// Standard input reader
pub struct Stdin;

impl Stdin {
    /// Reads a line from stdin.
    pub fn read_line(&self) -> IoResult<JetString> {
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(_) => {
                // Remove trailing newline
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                Ok(JetString::from_str(&line))
            }
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Reads all input from stdin.
    pub fn read_to_string(&self) -> IoResult<JetString> {
        let mut contents = String::new();
        match io::stdin().read_to_string(&mut contents) {
            Ok(_) => Ok(JetString::from_str(&contents)),
            Err(e) => Err(map_io_error(e)),
        }
    }
}

/// Standard output writer
pub struct Stdout;

impl Stdout {
    /// Writes a string to stdout.
    pub fn write(&self, s: &str) -> IoResult<()> {
        match io::stdout().write_all(s.as_bytes()) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Writes a line to stdout.
    pub fn write_line(&self, s: &str) -> IoResult<()> {
        let mut stdout = io::stdout();
        match stdout.write_all(s.as_bytes()) {
            Ok(_) => match stdout.write_all(b"\n") {
                Ok(_) => {
                    let _ = stdout.flush();
                    Ok(())
                }
                Err(e) => Err(map_io_error(e)),
            },
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Flushes stdout.
    pub fn flush(&self) -> IoResult<()> {
        match io::stdout().flush() {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }
}

/// Standard error writer
pub struct Stderr;

impl Stderr {
    /// Writes a string to stderr.
    pub fn write(&self, s: &str) -> IoResult<()> {
        match io::stderr().write_all(s.as_bytes()) {
            Ok(_) => Ok(()),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Writes a line to stderr.
    pub fn write_line(&self, s: &str) -> IoResult<()> {
        let mut stderr = io::stderr();
        match stderr.write_all(s.as_bytes()) {
            Ok(_) => match stderr.write_all(b"\n") {
                Ok(_) => Ok(()),
                Err(e) => Err(map_io_error(e)),
            },
            Err(e) => Err(map_io_error(e)),
        }
    }
}

/// Maps a std::io::Error to our IoError
fn map_io_error(e: io::Error) -> IoError {
    let kind = match e.kind() {
        io::ErrorKind::NotFound => IoErrorKind::NotFound,
        io::ErrorKind::PermissionDenied => IoErrorKind::PermissionDenied,
        io::ErrorKind::AlreadyExists => IoErrorKind::AlreadyExists,
        io::ErrorKind::InvalidInput => IoErrorKind::InvalidInput,
        io::ErrorKind::UnexpectedEof => IoErrorKind::UnexpectedEof,
        io::ErrorKind::WriteZero => IoErrorKind::WriteZero,
        io::ErrorKind::Interrupted => IoErrorKind::Interrupted,
        io::ErrorKind::NotADirectory => IoErrorKind::NotADirectory,
        io::ErrorKind::IsADirectory => IoErrorKind::IsADirectory,
        _ => IoErrorKind::Other,
    };

    IoError {
        message: JetString::from_str(&e.to_string()),
        kind,
    }
}

impl IoError {
    /// Creates a new I/O error.
    pub fn new(message: &str, kind: IoErrorKind) -> Self {
        IoError {
            message: JetString::from_str(message),
            kind,
        }
    }

    /// Returns the error message.
    pub fn message(&self) -> &str {
        self.message.as_str()
    }

    /// Returns the error kind.
    pub fn kind(&self) -> IoErrorKind {
        self.kind
    }
}

impl std::fmt::Display for IoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "IO error ({:?}): {}", self.kind, self.message.as_str())
    }
}

impl std::error::Error for IoError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_io_error_creation() {
        let err = IoError::new("test error", IoErrorKind::NotFound);
        assert_eq!(err.kind(), IoErrorKind::NotFound);
        assert_eq!(err.message(), "test error");
    }

    #[test]
    fn test_file_exists() {
        // This test assumes Cargo.toml exists
        assert!(file_exists("Cargo.toml"));
        assert!(!file_exists("nonexistent_file_xyz.jet"));
    }

    #[test]
    fn test_is_file() {
        assert!(is_file("Cargo.toml"));
        assert!(!is_file("src"));
    }

    #[test]
    fn test_is_dir() {
        assert!(is_dir("src"));
        assert!(!is_dir("Cargo.toml"));
    }
}
