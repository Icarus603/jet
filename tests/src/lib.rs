//! Jet Integration Test Framework
//!
//! This crate provides comprehensive integration tests for the Jet compiler
//! and runtime. It includes:
//!
//! - Test harness for compiling and running Jet programs
//! - Golden file tests for error messages
//! - End-to-end tests for example programs
//! - Compiler pipeline tests

pub mod golden;
pub mod harness;

pub use golden::{GoldenFile, GoldenTest};
pub use harness::{CompileResult, RunResult, TestHarness};
