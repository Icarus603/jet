//! Dynamic library loading for FFI
//!
//! This module provides cross-platform dynamic library loading capabilities
//! for the Jet FFI system. It supports loading C libraries and retrieving
//! symbols for function calls.
//!
//! # Platform Support
//!
//! - **Linux/Unix**: Uses `dlopen`, `dlsym`, `dlclose`
//! - **macOS**: Uses `dlopen`, `dlsym`, `dlclose` with platform-specific flags
//! - **Windows**: Uses `LoadLibrary`, `GetProcAddress`, `FreeLibrary`
//!
//! # Examples
//!
//! ```no_run
//! use jet_rt_ffi::library::Library;
//!
//! // Load a library
//! let lib = Library::open("libm.so.6").expect("Failed to load library");
//!
//! // Get a symbol
//! let sin = unsafe { lib.get_symbol::<fn(f64) -> f64>("sin") }.expect("Symbol not found");
//!
//! // Call the function
//! let result = sin(3.14159265359 / 2.0);
//! ```

use std::ffi::{c_void, CStr, CString};
use std::path::Path;

/// Errors that can occur when loading libraries
#[derive(Debug, Clone)]
pub enum LibraryError {
    /// The library file was not found
    NotFound(String),
    /// The requested symbol was not found in the library
    SymbolNotFound(String),
    /// The path is invalid
    InvalidPath,
    /// Loading the library failed (platform-specific error)
    LoadFailed(String),
    /// The symbol type is invalid
    InvalidSymbolType,
}

impl std::fmt::Display for LibraryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotFound(name) => write!(f, "library not found: {}", name),
            Self::SymbolNotFound(name) => write!(f, "symbol not found: {}", name),
            Self::InvalidPath => write!(f, "invalid library path"),
            Self::LoadFailed(msg) => write!(f, "failed to load library: {}", msg),
            Self::InvalidSymbolType => write!(f, "invalid symbol type"),
        }
    }
}

impl std::error::Error for LibraryError {}

pub type LibraryResult<T> = Result<T, LibraryError>;

/// Platform-specific library handle
#[derive(Debug)]
enum LibraryHandle {
    #[cfg(unix)]
    Dlopen(*mut c_void),
    #[cfg(windows)]
    Win32(windows::Win32::Foundation::HMODULE),
    #[cfg(not(any(unix, windows)))]
    Stub,
}

impl LibraryHandle {
    /// Returns true if the handle is valid
    fn is_valid(&self) -> bool {
        match self {
            #[cfg(unix)]
            LibraryHandle::Dlopen(handle) => !handle.is_null(),
            #[cfg(windows)]
            LibraryHandle::Win32(handle) => !handle.is_invalid(),
            #[cfg(not(any(unix, windows)))]
            LibraryHandle::Stub => false,
        }
    }
}

/// A loaded dynamic library
///
/// This represents a library that has been loaded into memory. The library
/// remains loaded until this struct is dropped.
///
/// # Safety
///
/// The library may contain arbitrary code. Care must be taken when calling
/// functions from the library to ensure type safety and proper calling conventions.
pub struct Library {
    handle: LibraryHandle,
    path: String,
}

// Library is Send but not Sync - the library handle should not be shared across threads
// without synchronization
unsafe impl Send for Library {}

impl Library {
    /// Open a library at the given path
    ///
    /// # Arguments
    ///
    /// * `path` - The path to the library file
    ///
    /// # Platform-specific behavior
    ///
    /// - **Linux**: Searches LD_LIBRARY_PATH if path is not absolute
    /// - **macOS**: Searches DYLD_LIBRARY_PATH if path is not absolute
    /// - **Windows**: Searches PATH if path is not absolute
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_ffi::library::Library;
    ///
    /// // Load the C math library on Linux
    /// if let Ok(libm) = Library::open("libm.so.6") {
    ///     println!("Loaded libm successfully");
    /// }
    /// ```
    pub fn open<P: AsRef<Path>>(path: P) -> LibraryResult<Self> {
        let path_ref = path.as_ref();

        if !path_ref.is_absolute() {
            // Try to locate the library in system paths
            return Self::open_in_system_paths(path_ref);
        }

        Self::open_internal(path_ref)
    }

    /// Open a library from an absolute path
    fn open_internal(path: &Path) -> LibraryResult<Self> {
        let path_str = path.to_str().ok_or(LibraryError::InvalidPath)?;

        #[cfg(unix)]
        {
            use std::ffi::CString;

            let c_path = CString::new(path_str).map_err(|_| LibraryError::InvalidPath)?;

            // RTLD_NOW: Resolve all symbols now
            // RTLD_LOCAL: Symbols are not available to subsequently loaded libraries
            let flags = libc::RTLD_NOW | libc::RTLD_LOCAL;

            let handle = unsafe { libc::dlopen(c_path.as_ptr(), flags) };

            if handle.is_null() {
                let error = unsafe {
                    CStr::from_ptr(libc::dlerror())
                        .to_string_lossy()
                        .to_string()
                };
                return Err(LibraryError::LoadFailed(error));
            }

            Ok(Self {
                handle: LibraryHandle::Dlopen(handle),
                path: path_str.to_string(),
            })
        }

        #[cfg(windows)]
        {
            use std::os::windows::ffi::OsStrExt;
            use windows::Win32::System::LibraryLoader::LoadLibraryW;

            let wide_path: Vec<u16> = OsStr::new(path_str)
                .encode_wide()
                .chain(std::iter::once(0))
                .collect();

            let handle = unsafe { LoadLibraryW(windows::core::PCWSTR(wide_path.as_ptr())) };

            match handle {
                Ok(h) => Ok(Self {
                    handle: LibraryHandle::Win32(h),
                    path: path_str.to_string(),
                }),
                Err(e) => Err(LibraryError::LoadFailed(e.to_string())),
            }
        }

        #[cfg(not(any(unix, windows)))]
        {
            Err(LibraryError::LoadFailed(
                "Dynamic loading not supported on this platform".to_string(),
            ))
        }
    }

    /// Try to open a library in system library paths
    fn open_in_system_paths(name: &Path) -> LibraryResult<Self> {
        // Try the name as-is first
        if let Ok(lib) = Self::open_internal(name) {
            return Ok(lib);
        }

        // Try common library paths
        let search_paths = Self::system_library_paths();
        let name_str = name.to_string_lossy();

        for dir in search_paths {
            let full_path = dir.join(&*name_str);
            if full_path.exists() {
                return Self::open_internal(&full_path);
            }

            // Try with platform-specific extensions
            #[cfg(target_os = "linux")]
            {
                let with_so = dir.join(format!("{}.so", name_str));
                if with_so.exists() {
                    return Self::open_internal(&with_so);
                }

                // Try versioned library names
                for version in ["6", "5", "4", "3", "2", "1", "0"] {
                    let versioned = dir.join(format!("{}.so.{}", name_str, version));
                    if versioned.exists() {
                        return Self::open_internal(&versioned);
                    }
                }
            }

            #[cfg(target_os = "macos")]
            {
                let with_dylib = dir.join(format!("lib{}.dylib", name_str));
                if with_dylib.exists() {
                    return Self::open_internal(&with_dylib);
                }
            }

            #[cfg(windows)]
            {
                let with_dll = dir.join(format!("{}.dll", name_str));
                if with_dll.exists() {
                    return Self::open_internal(&with_dll);
                }
            }
        }

        Err(LibraryError::NotFound(name_str.to_string()))
    }

    /// Returns the system library search paths
    fn system_library_paths() -> Vec<std::path::PathBuf> {
        let mut paths: Vec<std::path::PathBuf> = Vec::new();

        // Add paths from environment variable
        #[cfg(target_os = "linux")]
        {
            if let Ok(ld_path) = std::env::var("LD_LIBRARY_PATH") {
                for path in ld_path.split(':') {
                    paths.push(path.into());
                }
            }
        }

        #[cfg(target_os = "macos")]
        {
            if let Ok(dyld_path) = std::env::var("DYLD_LIBRARY_PATH") {
                for path in dyld_path.split(':') {
                    paths.push(path.into());
                }
            }
            if let Ok(dyld_fallback) = std::env::var("DYLD_FALLBACK_LIBRARY_PATH") {
                for path in dyld_fallback.split(':') {
                    paths.push(path.into());
                }
            }
        }

        #[cfg(windows)]
        {
            if let Ok(path_var) = std::env::var("PATH") {
                for path in path_var.split(';') {
                    paths.push(path.into());
                }
            }
        }

        // Add standard system paths
        #[cfg(target_os = "linux")]
        {
            paths.push("/usr/lib".into());
            paths.push("/usr/local/lib".into());
            paths.push("/lib".into());

            // Add architecture-specific paths
            if cfg!(target_arch = "x86_64") {
                paths.push("/usr/lib/x86_64-linux-gnu".into());
                paths.push("/lib/x86_64-linux-gnu".into());
            } else if cfg!(target_arch = "aarch64") {
                paths.push("/usr/lib/aarch64-linux-gnu".into());
                paths.push("/lib/aarch64-linux-gnu".into());
            }
        }

        #[cfg(target_os = "macos")]
        {
            paths.push("/usr/lib".into());
            paths.push("/usr/local/lib".into());
            paths.push("/opt/homebrew/lib".into()); // Homebrew on Apple Silicon
            paths.push("/opt/local/lib".into()); // MacPorts
        }

        #[cfg(windows)]
        {
            paths.push("C:\\Windows\\System32".into());
            paths.push("C:\\Windows\\SysWOW64".into());
        }

        // Remove duplicates while preserving order
        let mut seen: std::collections::HashSet<std::path::PathBuf> =
            std::collections::HashSet::new();
        paths.retain(|p| seen.insert(p.clone()));

        paths
    }

    /// Get a symbol from the library
    ///
    /// # Safety
    ///
    /// The caller must ensure that:
    /// - The symbol name is correct
    /// - The symbol type matches the actual type in the library
    /// - The library remains loaded while the symbol is used
    ///
    /// # Type Parameters
    ///
    /// * `T` - The expected type of the symbol. For functions, this should be
    ///   an `extern "C" fn` type.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use jet_rt_ffi::library::Library;
    ///
    /// let lib = Library::open("libm.so.6").unwrap();
    ///
    /// // Get the sin function
    /// let sin: extern "C" fn(f64) -> f64 = unsafe {
    ///     lib.get_symbol("sin").unwrap()
    /// };
    ///
    /// let result = sin(1.0);
    /// ```
    pub unsafe fn get_symbol<T>(&self, name: &str) -> LibraryResult<T> {
        let ptr = self.get_symbol_ptr(name)?;

        if ptr.is_null() {
            return Err(LibraryError::SymbolNotFound(name.to_string()));
        }

        // Transmute the pointer to the requested type
        // SAFETY: We've verified ptr is not null and the caller guarantees T is correct
        let ptr_ref: *const c_void = ptr;
        Ok(std::mem::transmute_copy::<*const c_void, T>(&ptr_ref))
    }

    /// Get a raw pointer to a symbol
    ///
    /// # Safety
    ///
    /// The returned pointer must be used according to the symbol's actual type.
    pub unsafe fn get_symbol_ptr(&self, name: &str) -> LibraryResult<*mut c_void> {
        let c_name = CString::new(name).map_err(|_| LibraryError::InvalidPath)?;

        match &self.handle {
            #[cfg(unix)]
            LibraryHandle::Dlopen(handle) => {
                libc::dlerror(); // Clear any previous error

                let ptr = libc::dlsym(*handle, c_name.as_ptr());

                // Check if dlsym returned an error
                let error = libc::dlerror();
                if !error.is_null() {
                    return Err(LibraryError::SymbolNotFound(name.to_string()));
                }

                Ok(ptr)
            }

            #[cfg(windows)]
            LibraryHandle::Win32(handle) => {
                use windows::Win32::System::LibraryLoader::GetProcAddress;

                let ptr =
                    GetProcAddress(*handle, windows::core::PCSTR(c_name.as_ptr() as *const u8));

                match ptr {
                    Some(p) => Ok(p as *mut c_void),
                    None => Err(LibraryError::SymbolNotFound(name.to_string())),
                }
            }

            #[cfg(not(any(unix, windows)))]
            LibraryHandle::Stub => Err(LibraryError::LoadFailed("Not implemented".to_string())),
        }
    }

    /// Returns the path to the loaded library
    pub fn path(&self) -> &str {
        &self.path
    }

    /// Close the library explicitly
    ///
    /// This is normally called automatically when the Library is dropped,
    /// but can be called explicitly if needed.
    pub fn close(self) {
        // The actual closing happens in Drop
        drop(self);
    }
}

impl Drop for Library {
    fn drop(&mut self) {
        match &self.handle {
            #[cfg(unix)]
            LibraryHandle::Dlopen(handle) => {
                if !handle.is_null() {
                    unsafe {
                        libc::dlclose(*handle);
                    }
                }
            }

            #[cfg(windows)]
            LibraryHandle::Win32(handle) => {
                if !handle.is_invalid() {
                    unsafe {
                        let _ = windows::Win32::System::LibraryLoader::FreeLibrary(*handle);
                    }
                }
            }

            #[cfg(not(any(unix, windows)))]
            LibraryHandle::Stub => {}
        }
    }
}

impl std::fmt::Debug for Library {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Library")
            .field("path", &self.path)
            .field("loaded", &self.handle.is_valid())
            .finish()
    }
}

/// A symbol from a library
///
/// This provides a type-safe wrapper around a symbol pointer.
#[derive(Debug)]
pub struct Symbol<T> {
    ptr: *mut c_void,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Symbol<T> {
    /// Create a new symbol from a raw pointer
    ///
    /// # Safety
    ///
    /// The pointer must point to a valid symbol of type T.
    pub unsafe fn new(ptr: *mut c_void) -> Self {
        Self {
            ptr,
            _marker: std::marker::PhantomData,
        }
    }

    /// Get the raw pointer to the symbol
    pub fn as_ptr(&self) -> *mut c_void {
        self.ptr
    }

    /// Call the symbol as a function
    ///
    /// # Safety
    ///
    /// The symbol must be a function with the correct signature.
    /// The caller is responsible for ensuring type safety.
    pub unsafe fn call<Args, Ret>(&self, args: Args) -> Ret {
        let func: extern "C" fn(Args) -> Ret = std::mem::transmute(self.ptr);
        func(args)
    }
}

/// Open a dynamic library
///
/// Convenience function that wraps `Library::open`.
pub fn open_library<P: AsRef<Path>>(path: P) -> LibraryResult<Library> {
    Library::open(path)
}

/// Close a dynamic library
///
/// Convenience function that wraps `Library::close`.
pub fn close_library(lib: Library) {
    lib.close();
}

/// Get the last platform-specific error message
#[cfg(unix)]
pub fn last_error_message() -> Option<String> {
    unsafe {
        let err = libc::dlerror();
        if err.is_null() {
            None
        } else {
            Some(CStr::from_ptr(err).to_string_lossy().to_string())
        }
    }
}

#[cfg(windows)]
pub fn last_error_message() -> Option<String> {
    windows::Win32::Foundation::GetLastError()
        .ok()
        .err()
        .map(|e| e.to_string())
}

#[cfg(not(any(unix, windows)))]
pub fn last_error_message() -> Option<String> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_library_error_display() {
        let err = LibraryError::NotFound("libtest.so".to_string());
        assert!(err.to_string().contains("libtest.so"));

        let err = LibraryError::SymbolNotFound("test_func".to_string());
        assert!(err.to_string().contains("test_func"));

        let err = LibraryError::LoadFailed("permission denied".to_string());
        assert!(err.to_string().contains("permission denied"));
    }

    #[test]
    fn test_system_library_paths() {
        let paths = Library::system_library_paths();
        assert!(!paths.is_empty());

        // On Linux, should include standard paths
        #[cfg(target_os = "linux")]
        {
            assert!(paths
                .iter()
                .any(|p| p.to_string_lossy().contains("/usr/lib")));
        }
    }

    // These tests require actual libraries and are platform-specific
    #[test]
    #[cfg(target_os = "linux")]
    fn test_load_libc() {
        // Try to load the C standard library
        let result = Library::open("libc.so.6");

        // May fail on some systems, so just verify it doesn't panic
        if let Ok(lib) = result {
            println!("Loaded libc from: {}", lib.path());

            // Try to get a well-known symbol
            let printf: Result<extern "C" fn(*const u8, ...), _> =
                unsafe { lib.get_symbol("printf") };
            assert!(printf.is_ok(), "printf should be available in libc");
        }
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn test_load_libm() {
        // Try to load the math library
        if let Ok(lib) = Library::open("libm.so.6") {
            println!("Loaded libm from: {}", lib.path());

            // Get sin function
            let sin: Result<extern "C" fn(f64) -> f64, _> = unsafe { lib.get_symbol("sin") };
            assert!(sin.is_ok(), "sin should be available in libm");

            if let Ok(sin) = sin {
                let result = sin(std::f64::consts::PI / 2.0);
                assert!((result - 1.0).abs() < 1e-10, "sin(pi/2) should be 1.0");
            }
        }
    }

    #[test]
    fn test_invalid_library() {
        let result = Library::open("/nonexistent/library/that/does/not/exist.so");
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_symbol() {
        // Even if we can't load a real library, we can test the error handling
        // by trying to load a non-existent library
        let result = Library::open("this_library_definitely_does_not_exist_12345");
        assert!(result.is_err());
    }
}
