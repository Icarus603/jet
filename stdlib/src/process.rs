//! Process management for Jet
//!
//! This module provides functionality for spawning child processes,
//! managing their input/output, and querying process status.
//!
//! # Examples
//!
//! ```
//! use jet_stdlib::process::{Command, Stdio};
//!
//! // Run a command and capture output
//! let output = Command::new("echo")
//!     .arg("hello")
//!     .output()
//!     .expect("Failed to execute command");
//!
//! assert!(output.status.success());
//! ```

use crate::io::{IoError, IoErrorKind, IoResult};
use crate::string::JetString;
use std::ffi::{c_char, c_int, CStr, CString};
use std::path::PathBuf;
use std::process::{
    Child as StdChild, Command as StdCommand, ExitStatus as StdExitStatus, Stdio as StdStdio,
};

/// A process builder, providing fine-grained control over how a new process
/// should be spawned.
///
/// # Examples
///
/// ```
/// use jet_stdlib::process::Command;
///
/// let cmd = Command::new("ls")
///     .arg("-l")
///     .arg("-a");
///
/// match cmd.spawn() {
///     Ok(child) => println!("Spawned process with PID: {:?}", child.id()),
///     Err(e) => println!("Failed to spawn: {}", e),
/// }
/// ```
pub struct Command {
    inner: StdCommand,
    program: JetString,
}

/// Representation of a running or exited child process.
///
/// This wraps the standard library's Child type and provides
/// a Jet-compatible interface.
pub struct Child {
    inner: Option<StdChild>,
    stdin: Option<crate::io::Stdout>,
    stdout: Option<crate::io::Stdin>,
    stderr: Option<crate::io::Stdin>,
}

/// Describes the result of a process after it has terminated.
///
/// This type mirrors `std::process::ExitStatus`.
#[derive(Debug, Clone, Copy)]
pub struct ExitStatus {
    code: Option<i32>,
    #[cfg(unix)]
    signal: Option<i32>,
}

/// The output of a finished process.
///
/// This includes the exit status, stdout, and stderr.
#[derive(Debug, Clone)]
pub struct Output {
    /// The status of the process
    pub status: ExitStatus,
    /// The stdout output
    pub stdout: Vec<u8>,
    /// The stderr output
    pub stderr: Vec<u8>,
}

/// Describes what to do with a standard I/O stream for a child process.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stdio {
    /// The stream will inherit from the parent process
    Inherit,
    /// The stream will be piped to/from the child process
    Piped,
    /// The stream will be redirected to /dev/null (or NUL on Windows)
    Null,
}

/// Error type for environment variable operations
#[derive(Debug, Clone)]
pub enum VarError {
    /// The variable was not found
    NotPresent,
    /// The variable was found but was not valid UTF-8
    NotUnicode(JetString),
}

impl std::fmt::Display for VarError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarError::NotPresent => write!(f, "environment variable not found"),
            VarError::NotUnicode(s) => {
                write!(f, "environment variable was not valid unicode: {}", s)
            }
        }
    }
}

impl std::error::Error for VarError {}

impl Command {
    /// Creates a new `Command` for launching the program at `program`.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// let cmd = Command::new("echo");
    /// ```
    pub fn new(program: impl AsRef<str>) -> Self {
        let program_str = program.as_ref();
        let inner = StdCommand::new(program_str);

        Self {
            inner,
            program: JetString::from_str(program_str),
        }
    }

    /// Adds an argument to pass to the program.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// let cmd = Command::new("ls").arg("-l");
    /// ```
    pub fn arg(mut self, arg: impl AsRef<str>) -> Self {
        self.inner.arg(arg.as_ref());
        self
    }

    /// Adds multiple arguments to pass to the program.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// let cmd = Command::new("ls").args(["-l", "-a"]);
    /// ```
    pub fn args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        for arg in args {
            self.inner.arg(arg.as_ref());
        }
        self
    }

    /// Sets an environment variable for the child process.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// let cmd = Command::new("printenv")
    ///     .env("MY_VAR", "my_value");
    /// ```
    pub fn env(mut self, key: impl AsRef<str>, val: impl AsRef<str>) -> Self {
        self.inner.env(key.as_ref(), val.as_ref());
        self
    }

    /// Removes an environment variable from the child process.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// let cmd = Command::new("printenv")
    ///     .env_remove("PATH");
    /// ```
    pub fn env_remove(mut self, key: impl AsRef<str>) -> Self {
        self.inner.env_remove(key.as_ref());
        self
    }

    /// Clears all environment variables for the child process.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// let cmd = Command::new("printenv")
    ///     .env_clear();
    /// ```
    pub fn env_clear(mut self) -> Self {
        self.inner.env_clear();
        self
    }

    /// Sets the working directory for the child process.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// let cmd = Command::new("ls")
    ///     .current_dir("/tmp");
    /// ```
    pub fn current_dir(mut self, dir: impl AsRef<std::path::Path>) -> Self {
        self.inner.current_dir(dir.as_ref());
        self
    }

    /// Sets the stdin configuration for the child process.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::{Command, Stdio};
    ///
    /// let cmd = Command::new("cat")
    ///     .stdin(Stdio::Piped);
    /// ```
    pub fn stdin(mut self, cfg: Stdio) -> Self {
        self.inner.stdin(stdio_to_std(cfg));
        self
    }

    /// Sets the stdout configuration for the child process.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::{Command, Stdio};
    ///
    /// let cmd = Command::new("echo")
    ///     .stdout(Stdio::Piped);
    /// ```
    pub fn stdout(mut self, cfg: Stdio) -> Self {
        self.inner.stdout(stdio_to_std(cfg));
        self
    }

    /// Sets the stderr configuration for the child process.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::{Command, Stdio};
    ///
    /// let cmd = Command::new("echo")
    ///     .stderr(Stdio::Piped);
    /// ```
    pub fn stderr(mut self, cfg: Stdio) -> Self {
        self.inner.stderr(stdio_to_std(cfg));
        self
    }

    /// Spawns the command as a child process.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// match Command::new("echo").arg("hello").spawn() {
    ///     Ok(child) => {
    ///         println!("Spawned process with PID: {}", child.id());
    ///     }
    ///     Err(e) => eprintln!("Failed to spawn: {}", e),
    /// }
    /// ```
    pub fn spawn(mut self) -> IoResult<Child> {
        match self.inner.spawn() {
            Ok(child) => Ok(Child::from_std(child)),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Executes the command as a child process, waiting for it to finish
    /// and collecting all of its output.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// match Command::new("echo").arg("hello").output() {
    ///     Ok(output) => {
    ///         if output.status.success() {
    ///             println!("Output: {:?}", String::from_utf8_lossy(&output.stdout));
    ///         }
    ///     }
    ///     Err(e) => eprintln!("Failed to execute: {}", e),
    /// }
    /// ```
    pub fn output(mut self) -> IoResult<Output> {
        match self.inner.output() {
            Ok(output) => Ok(Output {
                status: ExitStatus::from_std(output.status),
                stdout: output.stdout,
                stderr: output.stderr,
            }),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Executes a command as a child process, waiting for it to finish
    /// and returning just the exit status.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// match Command::new("true").status() {
    ///     Ok(status) => {
    ///         if status.success() {
    ///             println!("Command succeeded");
    ///         }
    ///     }
    ///     Err(e) => eprintln!("Failed to execute: {}", e),
    /// }
    /// ```
    pub fn status(mut self) -> IoResult<ExitStatus> {
        match self.inner.status() {
            Ok(status) => Ok(ExitStatus::from_std(status)),
            Err(e) => Err(map_io_error(e)),
        }
    }

    /// Returns the program that will be executed.
    pub fn get_program(&self) -> &str {
        self.program.as_str()
    }

    /// Returns the arguments that will be passed to the program.
    pub fn get_args(&self) -> Vec<&str> {
        // Note: std::process::Command doesn't expose args directly
        // This would need to be tracked separately if needed
        Vec::new()
    }
}

impl Child {
    /// Creates a Child from a std::process::Child
    fn from_std(child: StdChild) -> Self {
        Self {
            inner: Some(child),
            stdin: None,
            stdout: None,
            stderr: None,
        }
    }

    /// Returns the OS-assigned process identifier.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// if let Ok(child) = Command::new("sleep").arg("10").spawn() {
    ///     println!("PID: {}", child.id());
    /// }
    /// ```
    pub fn id(&self) -> u32 {
        self.inner.as_ref().map(|c| c.id()).unwrap_or(0)
    }

    /// Forces the child process to exit.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// if let Ok(mut child) = Command::new("sleep").arg("10").spawn() {
    ///     child.kill().expect("Failed to kill process");
    /// }
    /// ```
    pub fn kill(&mut self) -> IoResult<()> {
        match self.inner.as_mut() {
            Some(child) => child.kill().map_err(map_io_error),
            None => Err(IoError::new("Process already exited", IoErrorKind::Other)),
        }
    }

    /// Waits for the child to exit completely, returning the status.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// if let Ok(mut child) = Command::new("echo").arg("hello").spawn() {
    ///     let status = child.wait().expect("Failed to wait");
    ///     println!("Exit code: {:?}", status.code());
    /// }
    /// ```
    pub fn wait(&mut self) -> IoResult<ExitStatus> {
        match self.inner.take() {
            Some(mut child) => match child.wait() {
                Ok(status) => Ok(ExitStatus::from_std(status)),
                Err(e) => Err(map_io_error(e)),
            },
            None => Err(IoError::new("Process already waited", IoErrorKind::Other)),
        }
    }

    /// Attempts to collect the exit status of the child if it has already exited.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// if let Ok(mut child) = Command::new("echo").arg("hello").spawn() {
    ///     match child.try_wait() {
    ///         Ok(Some(status)) => println!("Exited with: {:?}", status.code()),
    ///         Ok(None) => println!("Still running"),
    ///         Err(e) => eprintln!("Error: {}", e),
    ///     }
    /// }
    /// ```
    pub fn try_wait(&mut self) -> IoResult<Option<ExitStatus>> {
        match self.inner.as_mut() {
            Some(child) => match child.try_wait() {
                Ok(Some(status)) => Ok(Some(ExitStatus::from_std(status))),
                Ok(None) => Ok(None),
                Err(e) => Err(map_io_error(e)),
            },
            None => Ok(None),
        }
    }
}

impl ExitStatus {
    /// Creates an ExitStatus from a std::process::ExitStatus
    fn from_std(status: StdExitStatus) -> Self {
        #[cfg(unix)]
        use std::os::unix::process::ExitStatusExt;

        Self {
            code: status.code(),
            #[cfg(unix)]
            signal: status.signal(),
        }
    }

    /// Was termination successful? Returns true if the exit code is 0.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// let status = Command::new("true").status().unwrap();
    /// assert!(status.success());
    ///
    /// let status = Command::new("false").status().unwrap();
    /// assert!(!status.success());
    /// ```
    pub fn success(&self) -> bool {
        self.code == Some(0)
    }

    /// Returns the exit code of the process, if any.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// let status = Command::new("sh")
    ///     .args(["-c", "exit 42"])
    ///     .status()
    ///     .unwrap();
    /// assert_eq!(status.code(), Some(42));
    /// ```
    pub fn code(&self) -> Option<i32> {
        self.code
    }

    /// Returns the signal that caused the process to exit, if any.
    ///
    /// This is only available on Unix platforms.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::process::Command;
    ///
    /// // On Unix, this would return the signal number if killed by signal
    /// let status = Command::new("true").status().unwrap();
    /// assert_eq!(status.signal(), None);
    /// ```
    #[cfg(unix)]
    pub fn signal(&self) -> Option<i32> {
        self.signal
    }

    /// Returns the signal that caused the process to exit, if any.
    ///
    /// On non-Unix platforms, this always returns None.
    #[cfg(not(unix))]
    pub fn signal(&self) -> Option<i32> {
        None
    }
}

impl Output {
    /// Returns true if the process exited successfully.
    pub fn success(&self) -> bool {
        self.status.success()
    }

    /// Returns the stdout as a string (lossy conversion).
    pub fn stdout_string(&self) -> JetString {
        JetString::from_str(&String::from_utf8_lossy(&self.stdout))
    }

    /// Returns the stderr as a string (lossy conversion).
    pub fn stderr_string(&self) -> JetString {
        JetString::from_str(&String::from_utf8_lossy(&self.stderr))
    }
}

/// Converts our Stdio enum to std::process::Stdio
fn stdio_to_std(stdio: Stdio) -> StdStdio {
    match stdio {
        Stdio::Inherit => StdStdio::inherit(),
        Stdio::Piped => StdStdio::piped(),
        Stdio::Null => StdStdio::null(),
    }
}

/// Maps a std::io::Error to our IoError
fn map_io_error(e: std::io::Error) -> IoError {
    use std::io::ErrorKind;

    let kind = match e.kind() {
        ErrorKind::NotFound => IoErrorKind::NotFound,
        ErrorKind::PermissionDenied => IoErrorKind::PermissionDenied,
        ErrorKind::AlreadyExists => IoErrorKind::AlreadyExists,
        ErrorKind::InvalidInput => IoErrorKind::InvalidInput,
        ErrorKind::UnexpectedEof => IoErrorKind::UnexpectedEof,
        ErrorKind::WriteZero => IoErrorKind::WriteZero,
        ErrorKind::Interrupted => IoErrorKind::Interrupted,
        ErrorKind::NotADirectory => IoErrorKind::NotADirectory,
        ErrorKind::IsADirectory => IoErrorKind::IsADirectory,
        _ => IoErrorKind::Other,
    };

    IoError::new(&e.to_string(), kind)
}

/// Terminates the current process with the specified exit code.
///
/// # Examples
///
/// ```text
/// // Exit with code 0 (success)
/// // jet_stdlib::process::exit(0);
///
/// // Exit with code 1 (failure)
/// // jet_stdlib::process::exit(1);
/// ```
pub fn exit(code: i32) -> ! {
    std::process::exit(code)
}

/// Aborts the current process immediately.
///
/// This is more abrupt than `exit()` and may not run destructors.
///
/// # Examples
///
/// ```text
/// // Abort the process
/// // jet_stdlib::process::abort();
/// ```
pub fn abort() -> ! {
    std::process::abort()
}

/// Returns the path of the current executable.
///
/// # Examples
///
/// ```
/// match jet_stdlib::process::current_exe() {
///     Ok(path) => println!("Current executable: {:?}", path),
///     Err(e) => eprintln!("Failed to get current exe: {}", e),
/// }
/// ```
pub fn current_exe() -> IoResult<PathBuf> {
    std::env::current_exe().map_err(map_io_error)
}

/// Returns the current working directory.
///
/// # Examples
///
/// ```
/// match jet_stdlib::process::current_dir() {
///     Ok(path) => println!("Current directory: {:?}", path),
///     Err(e) => eprintln!("Failed to get current dir: {}", e),
/// }
/// ```
pub fn current_dir() -> IoResult<PathBuf> {
    std::env::current_dir().map_err(map_io_error)
}

/// Sets the current working directory.
///
/// # Examples
///
/// ```
/// if let Err(e) = jet_stdlib::process::set_current_dir("/tmp") {
///     eprintln!("Failed to change directory: {}", e);
/// }
/// ```
pub fn set_current_dir(path: impl AsRef<std::path::Path>) -> IoResult<()> {
    std::env::set_current_dir(path.as_ref()).map_err(map_io_error)
}

/// Returns the command-line arguments passed to the current process.
///
/// # Examples
///
/// ```
/// for arg in jet_stdlib::process::args() {
///     println!("Arg: {}", arg);
/// }
/// ```
pub fn args() -> impl Iterator<Item = String> {
    std::env::args()
}

/// Returns all environment variables.
///
/// # Examples
///
/// ```
/// for (key, value) in jet_stdlib::process::vars() {
///     println!("{}={}", key, value);
/// }
/// ```
pub fn vars() -> impl Iterator<Item = (String, String)> {
    std::env::vars()
}

/// Returns the value of an environment variable.
///
/// # Examples
///
/// ```
/// use jet_stdlib::process::{var, VarError};
///
/// match var("PATH") {
///     Ok(path) => println!("PATH: {}", path),
///     Err(VarError::NotPresent) => println!("PATH not set"),
///     Err(VarError::NotUnicode(s)) => println!("PATH not valid unicode: {}", s),
/// }
/// ```
pub fn var(key: impl AsRef<str>) -> Result<String, VarError> {
    match std::env::var(key.as_ref()) {
        Ok(val) => Ok(val),
        Err(std::env::VarError::NotPresent) => Err(VarError::NotPresent),
        Err(std::env::VarError::NotUnicode(os_str)) => Err(VarError::NotUnicode(
            JetString::from_str(&os_str.to_string_lossy()),
        )),
    }
}

// C ABI exports for Jet code generation

/// Opaque handle for Child process
pub struct ChildHandle(pub Child);

/// Opaque handle for Command
pub struct CommandHandle(pub Command);

/// Opaque handle for Output
pub struct OutputHandle(pub Output);

/// Result type for FFI calls
#[repr(C)]
pub struct FfiResult<T> {
    pub success: bool,
    pub value: T,
    pub error_msg: *mut c_char,
}

impl<T> FfiResult<T> {
    pub fn ok(value: T) -> Self {
        Self {
            success: true,
            value,
            error_msg: std::ptr::null_mut(),
        }
    }

    pub fn err(error: &str) -> Self {
        let c_msg = CString::new(error).unwrap_or_default();
        Self {
            success: false,
            // This is unsafe but necessary for FFI - the value is undefined on error
            value: unsafe { std::mem::zeroed() },
            error_msg: c_msg.into_raw(),
        }
    }
}

/// Creates a new Command
///
/// # Safety
/// The caller must ensure program is a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn jet_command_new(program: *const c_char) -> *mut CommandHandle {
    if program.is_null() {
        return std::ptr::null_mut();
    }
    let program_str = unsafe { CStr::from_ptr(program).to_str().unwrap_or("") };
    Box::into_raw(Box::new(CommandHandle(Command::new(program_str))))
}

/// Frees a Command handle
#[no_mangle]
pub unsafe extern "C" fn jet_command_free(handle: *mut CommandHandle) {
    if !handle.is_null() {
        unsafe { drop(Box::from_raw(handle)) };
    }
}

/// Adds an argument to a Command
///
/// # Safety
/// The caller must ensure handle and arg are valid pointers.
#[no_mangle]
pub unsafe extern "C" fn jet_command_arg(
    handle: *mut CommandHandle,
    arg: *const c_char,
) -> *mut CommandHandle {
    if handle.is_null() || arg.is_null() {
        return handle;
    }
    let cmd = unsafe { Box::from_raw(handle) };
    let arg_str = unsafe { CStr::from_ptr(arg).to_str().unwrap_or("") };
    let new_cmd = cmd.0.arg(arg_str);
    Box::into_raw(Box::new(CommandHandle(new_cmd)))
}

/// Sets the current directory for a Command
///
/// # Safety
/// The caller must ensure handle and dir are valid pointers.
#[no_mangle]
pub unsafe extern "C" fn jet_command_current_dir(
    handle: *mut CommandHandle,
    dir: *const c_char,
) -> *mut CommandHandle {
    if handle.is_null() || dir.is_null() {
        return handle;
    }
    let cmd = unsafe { Box::from_raw(handle) };
    let dir_str = unsafe { CStr::from_ptr(dir).to_str().unwrap_or("") };
    let new_cmd = cmd.0.current_dir(dir_str);
    Box::into_raw(Box::new(CommandHandle(new_cmd)))
}

/// Spawns a Command
///
/// # Safety
/// The caller must ensure handle is a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_command_spawn(handle: *mut CommandHandle) -> *mut ChildHandle {
    if handle.is_null() {
        return std::ptr::null_mut();
    }
    let cmd = unsafe { Box::from_raw(handle) };
    match cmd.0.spawn() {
        Ok(child) => Box::into_raw(Box::new(ChildHandle(child))),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Executes a Command and returns output
///
/// # Safety
/// The caller must ensure handle is a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_command_output(handle: *mut CommandHandle) -> *mut OutputHandle {
    if handle.is_null() {
        return std::ptr::null_mut();
    }
    let cmd = unsafe { Box::from_raw(handle) };
    match cmd.0.output() {
        Ok(output) => Box::into_raw(Box::new(OutputHandle(output))),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Frees a Child handle
#[no_mangle]
pub unsafe extern "C" fn jet_child_free(handle: *mut ChildHandle) {
    if !handle.is_null() {
        unsafe { drop(Box::from_raw(handle)) };
    }
}

/// Returns the PID of a Child
///
/// # Safety
/// The caller must ensure handle is a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_child_id(handle: *mut ChildHandle) -> u32 {
    if handle.is_null() {
        return 0;
    }
    let child = unsafe { &(*handle).0 };
    child.id()
}

/// Kills a Child process
///
/// # Safety
/// The caller must ensure handle is a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_child_kill(handle: *mut ChildHandle) -> bool {
    if handle.is_null() {
        return false;
    }
    let child = unsafe { &mut (*handle).0 };
    child.kill().is_ok()
}

/// Waits for a Child to exit
///
/// # Safety
/// The caller must ensure handle is a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_child_wait(handle: *mut ChildHandle) -> c_int {
    if handle.is_null() {
        return -1;
    }
    let child = unsafe { &mut (*handle).0 };
    match child.wait() {
        Ok(status) => status.code().unwrap_or(-1),
        Err(_) => -1,
    }
}

/// Frees an Output handle
#[no_mangle]
pub unsafe extern "C" fn jet_output_free(handle: *mut OutputHandle) {
    if !handle.is_null() {
        unsafe { drop(Box::from_raw(handle)) };
    }
}

/// Returns the exit status from an Output
///
/// # Safety
/// The caller must ensure handle is a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_output_status(handle: *mut OutputHandle) -> c_int {
    if handle.is_null() {
        return -1;
    }
    let output = unsafe { &(*handle).0 };
    output.status.code().unwrap_or(-1)
}

/// Returns the stdout from an Output
///
/// # Safety
/// The caller must ensure handle is a valid pointer.
/// The returned string must be freed with jet_process_string_free.
#[no_mangle]
pub unsafe extern "C" fn jet_output_stdout(handle: *mut OutputHandle) -> *mut c_char {
    if handle.is_null() {
        return std::ptr::null_mut();
    }
    let output = unsafe { &(*handle).0 };
    let stdout_str = String::from_utf8_lossy(&output.stdout);
    CString::new(stdout_str.as_bytes())
        .unwrap_or_default()
        .into_raw()
}

/// Returns the stderr from an Output
///
/// # Safety
/// The caller must ensure handle is a valid pointer.
/// The returned string must be freed with jet_process_string_free.
#[no_mangle]
pub unsafe extern "C" fn jet_output_stderr(handle: *mut OutputHandle) -> *mut c_char {
    if handle.is_null() {
        return std::ptr::null_mut();
    }
    let output = unsafe { &(*handle).0 };
    let stderr_str = String::from_utf8_lossy(&output.stderr);
    CString::new(stderr_str.as_bytes())
        .unwrap_or_default()
        .into_raw()
}

/// Frees a string returned by process FFI functions
#[no_mangle]
pub unsafe extern "C" fn jet_process_string_free(s: *mut c_char) {
    if !s.is_null() {
        unsafe {
            let _ = CString::from_raw(s);
        }
    }
}

/// Returns the current executable path
///
/// # Safety
/// The returned string must be freed with jet_process_string_free.
#[no_mangle]
pub unsafe extern "C" fn jet_process_current_exe() -> *mut c_char {
    match current_exe() {
        Ok(path) => CString::new(path.to_string_lossy().as_bytes())
            .unwrap_or_default()
            .into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Returns the current working directory
///
/// # Safety
/// The returned string must be freed with jet_process_string_free.
#[no_mangle]
pub unsafe extern "C" fn jet_process_current_dir() -> *mut c_char {
    match current_dir() {
        Ok(path) => CString::new(path.to_string_lossy().as_bytes())
            .unwrap_or_default()
            .into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Sets the current working directory
///
/// # Safety
/// The caller must ensure path is a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn jet_process_set_current_dir(path: *const c_char) -> bool {
    if path.is_null() {
        return false;
    }
    let path_str = unsafe { CStr::from_ptr(path).to_str().unwrap_or("") };
    set_current_dir(path_str).is_ok()
}

/// Exits the current process
#[no_mangle]
pub extern "C" fn jet_process_exit(code: c_int) -> ! {
    exit(code)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_command_builder() {
        let cmd = Command::new("echo").arg("hello").arg("world");
        assert_eq!(cmd.get_program(), "echo");
    }

    #[test]
    fn test_command_output() {
        // Use /bin/echo for consistent behavior across platforms
        let output = Command::new("/bin/echo")
            .arg("hello")
            .output()
            .expect("Failed to execute");

        assert!(output.success());
        let stdout = output.stdout_string();
        let stdout_str = stdout.as_str();
        assert!(
            stdout_str.contains("hello"),
            "stdout '{}' should contain 'hello'",
            stdout_str
        );
    }

    #[test]
    fn test_command_status() {
        let status = Command::new("true").status().expect("Failed to execute");
        assert!(status.success());
        assert_eq!(status.code(), Some(0));

        let status = Command::new("false").status().expect("Failed to execute");
        assert!(!status.success());
    }

    #[test]
    fn test_exit_status() {
        let status = ExitStatus {
            code: Some(0),
            #[cfg(unix)]
            signal: None,
        };
        assert!(status.success());

        let status = ExitStatus {
            code: Some(1),
            #[cfg(unix)]
            signal: None,
        };
        assert!(!status.success());
    }

    #[test]
    fn test_env_var() {
        // This test assumes PATH is set on most systems
        if let Ok(path) = var("PATH") {
            assert!(!path.is_empty());
        }
    }

    #[test]
    fn test_current_dir() {
        let dir = current_dir().expect("Failed to get current dir");
        assert!(dir.exists());
    }

    #[test]
    fn test_current_exe() {
        let exe = current_exe().expect("Failed to get current exe");
        assert!(exe.exists());
    }
}
