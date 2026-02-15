//! Build script for jet-codegen
//!
//! This script handles LLVM configuration for the inkwell crate.
//! It detects LLVM installation and sets up the appropriate paths.

use std::env;
use std::path::PathBuf;

fn main() {
    // Only configure LLVM on macOS with Homebrew
    if env::var("CARGO_CFG_TARGET_OS").unwrap_or_default() == "macos" {
        // Try common Homebrew LLVM locations
        let possible_paths = [
            "/opt/homebrew/opt/llvm@21", // Versioned Apple Silicon
            "/usr/local/opt/llvm@21",    // Versioned Intel
        ];

        for path in &possible_paths {
            if PathBuf::from(path).exists() {
                // Set library search path for linking
                println!("cargo:rustc-link-search=native={}/lib", path);
                println!("cargo:warning=Using LLVM from: {}", path);

                // Also set the env var for any subprocesses
                // Note: This won't affect llvm-sys since it's already built,
                // but it helps if we're running in an environment where
                // LLVM_SYS_211_PREFIX needs to be set
                if env::var("LLVM_SYS_211_PREFIX").is_err() {
                    println!(
                        "cargo:warning=Set LLVM_SYS_211_PREFIX={} before building",
                        path
                    );
                }
                break;
            }
        }
    }

    // Link to the runtime GC library when building tests
    println!("cargo:rustc-link-search=native=../runtime/gc");

    // Re-run if LLVM configuration changes
    println!("cargo:rerun-if-env-changed=LLVM_SYS_211_PREFIX");
    println!("cargo:rerun-if-changed=build.rs");
}
