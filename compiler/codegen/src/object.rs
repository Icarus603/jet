//! Object Generation
//!
//! This module handles the generation of object files from LLVM IR.
//! It provides utilities for target selection, optimization levels,
//! and writing compiled code to disk.

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::OptimizationLevel;
use std::path::Path;

/// Configuration for object file generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ObjectConfig {
    /// The optimization level.
    pub opt_level: OptimizationLevel,
    /// The target triple (None for native).
    pub target_triple: Option<&'static str>,
    /// The relocation mode.
    pub reloc_mode: RelocMode,
    /// The code model.
    pub code_model: CodeModel,
}

impl Default for ObjectConfig {
    fn default() -> Self {
        Self {
            opt_level: OptimizationLevel::Default,
            target_triple: None,
            reloc_mode: RelocMode::Default,
            code_model: CodeModel::Default,
        }
    }
}

impl ObjectConfig {
    /// Creates a new configuration with the given optimization level.
    pub fn with_opt_level(mut self, level: OptimizationLevel) -> Self {
        self.opt_level = level;
        self
    }

    /// Creates a new configuration with the given target triple.
    pub fn with_target_triple(mut self, triple: &'static str) -> Self {
        self.target_triple = Some(triple);
        self
    }

    /// Creates a debug configuration (no optimizations).
    pub fn debug() -> Self {
        Self {
            opt_level: OptimizationLevel::None,
            ..Default::default()
        }
    }

    /// Creates a release configuration (aggressive optimizations).
    pub fn release() -> Self {
        Self {
            opt_level: OptimizationLevel::Aggressive,
            ..Default::default()
        }
    }
}

/// Initializes all LLVM targets.
///
/// This must be called before any target-specific operations.
pub fn initialize_targets() {
    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native target");
    Target::initialize_all(&InitializationConfig::default());
}

/// Gets the default target triple for the host system.
pub fn get_default_target_triple() -> TargetTriple {
    TargetMachine::get_default_triple()
}

/// Creates a target machine for code generation.
///
/// # Arguments
///
/// * `config` - The object generation configuration
///
/// # Returns
///
/// Returns a TargetMachine on success, or an error if the target cannot be created.
pub fn create_target_machine(config: ObjectConfig) -> CodegenResult<TargetMachine> {
    initialize_targets();

    let triple = config
        .target_triple
        .map(TargetTriple::create)
        .unwrap_or_else(get_default_target_triple);

    let target = Target::from_triple(&triple)
        .map_err(|e| CodegenError::llvm_error(format!("Failed to get target: {}", e)))?;

    let cpu = TargetMachine::get_host_cpu_name();
    let features = TargetMachine::get_host_cpu_features();

    target
        .create_target_machine(
            &triple,
            &cpu.to_string(),
            &features.to_string(),
            config.opt_level,
            config.reloc_mode,
            config.code_model,
        )
        .ok_or_else(|| CodegenError::llvm_error("Failed to create target machine"))
}

/// Generates an object file from the compiled module.
///
/// # Arguments
///
/// * `codegen` - The code generation context containing the module
/// * `config` - The object generation configuration
/// * `output_path` - The path to write the object file to
///
/// # Returns
///
/// Returns Ok(()) on success, or an error on failure.
pub fn generate_object_file<'ctx>(
    codegen: &CodeGen<'ctx>,
    config: ObjectConfig,
    output_path: &Path,
) -> CodegenResult<()> {
    // Verify the module first
    codegen.verify_module()?;

    // Create the target machine
    let target_machine = create_target_machine(config)?;

    // Set the module's target triple
    let triple = config
        .target_triple
        .map(TargetTriple::create)
        .unwrap_or_else(get_default_target_triple);
    codegen.module.set_triple(&triple);

    // Set the module's data layout
    let data_layout = target_machine.get_target_data().get_data_layout();
    codegen.module.set_data_layout(&data_layout);

    // Write the object file
    target_machine
        .write_to_file(&codegen.module, FileType::Object, output_path)
        .map_err(|e| CodegenError::llvm_error(format!("Failed to write object file: {}", e)))?;

    Ok(())
}

/// Generates an assembly file from the compiled module.
///
/// # Arguments
///
/// * `codegen` - The code generation context containing the module
/// * `config` - The object generation configuration
/// * `output_path` - The path to write the assembly file to
///
/// # Returns
///
/// Returns Ok(()) on success, or an error on failure.
pub fn generate_assembly_file<'ctx>(
    codegen: &CodeGen<'ctx>,
    config: ObjectConfig,
    output_path: &Path,
) -> CodegenResult<()> {
    // Verify the module first
    codegen.verify_module()?;

    // Create the target machine
    let target_machine = create_target_machine(config)?;

    // Set the module's target triple
    let triple = config
        .target_triple
        .map(TargetTriple::create)
        .unwrap_or_else(get_default_target_triple);
    codegen.module.set_triple(&triple);

    // Write the assembly file
    target_machine
        .write_to_file(&codegen.module, FileType::Assembly, output_path)
        .map_err(|e| CodegenError::llvm_error(format!("Failed to write assembly file: {}", e)))?;

    Ok(())
}

/// Generates LLVM IR text from the compiled module.
///
/// # Arguments
///
/// * `codegen` - The code generation context containing the module
///
/// # Returns
///
/// Returns the LLVM IR as a string.
pub fn generate_llvm_ir<'ctx>(codegen: &CodeGen<'ctx>) -> String {
    codegen.module.print_to_string().to_string()
}

/// Generates LLVM bitcode from the compiled module.
///
/// # Arguments
///
/// * `codegen` - The code generation context containing the module
/// * `output_path` - The path to write the bitcode file to
///
/// # Returns
///
/// Returns Ok(()) on success, or an error on failure.
pub fn generate_bitcode<'ctx>(codegen: &CodeGen<'ctx>, output_path: &Path) -> CodegenResult<()> {
    codegen.module.write_bitcode_to_path(output_path);
    Ok(())
}

/// Compiles a module and generates an object file.
///
/// This is a convenience function that combines all steps.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `output_path` - The path to write the object file to
///
/// # Returns
///
/// Returns Ok(()) on success, or an error on failure.
pub fn compile_to_object<'ctx>(codegen: &CodeGen<'ctx>, output_path: &Path) -> CodegenResult<()> {
    generate_object_file(codegen, ObjectConfig::default(), output_path)
}

/// Gets information about the current target.
pub fn get_target_info() -> CodegenResult<TargetInfo> {
    initialize_targets();

    let triple = get_default_target_triple();
    let target = Target::from_triple(&triple)
        .map_err(|e| CodegenError::llvm_error(format!("Failed to get target: {}", e)))?;

    Ok(TargetInfo {
        triple: triple.to_string(),
        name: target.get_name().to_string_lossy().to_string(),
        description: target.get_description().to_string_lossy().to_string(),
        has_jit: target.has_jit(),
        has_target_machine: target.has_target_machine(),
        has_asm_backend: target.has_asm_backend(),
    })
}

/// Information about a target.
#[derive(Debug, Clone)]
pub struct TargetInfo {
    /// The target triple string.
    pub triple: String,
    /// The target name.
    pub name: String,
    /// The target description.
    pub description: String,
    /// Whether the target supports JIT compilation.
    pub has_jit: bool,
    /// Whether the target has a target machine.
    pub has_target_machine: bool,
    /// Whether the target has an assembly backend.
    pub has_asm_backend: bool,
}

/// Lists all available targets.
pub fn list_available_targets() -> Vec<String> {
    // Note: inkwell doesn't provide a way to list all targets
    // Return the native target as a fallback
    vec![TargetMachine::get_default_triple().to_string()]
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::context::Context;

    #[test]
    fn test_object_config_default() {
        let config = ObjectConfig::default();
        assert_eq!(config.opt_level, OptimizationLevel::Default);
        assert!(config.target_triple.is_none());
    }

    #[test]
    fn test_object_config_debug() {
        let config = ObjectConfig::debug();
        assert_eq!(config.opt_level, OptimizationLevel::None);
    }

    #[test]
    fn test_object_config_release() {
        let config = ObjectConfig::release();
        assert_eq!(config.opt_level, OptimizationLevel::Aggressive);
    }

    #[test]
    fn test_get_default_target_triple() {
        let triple = get_default_target_triple();
        assert!(!triple.to_string().is_empty());
    }

    #[test]
    fn test_create_target_machine() {
        let config = ObjectConfig::default();
        let machine = create_target_machine(config);
        assert!(machine.is_ok());
    }

    #[test]
    fn test_get_target_info() {
        let info = get_target_info();
        assert!(info.is_ok());

        let info = info.unwrap();
        assert!(!info.triple.is_empty());
        assert!(!info.name.is_empty());
    }

    #[test]
    fn test_list_available_targets() {
        let targets = list_available_targets();
        assert!(!targets.is_empty());
        // Should at least have the native target
        assert!(targets.iter().any(|t| !t.is_empty()));
    }

    #[test]
    fn test_generate_llvm_ir() {
        let context = Context::create();
        let codegen = CodeGen::new(&context, "test");

        // Add a simple function
        let fn_type = context.i32_type().fn_type(&[], false);
        let func = codegen.module.add_function("test", fn_type, None);
        let bb = context.append_basic_block(func, "entry");
        codegen.builder.position_at_end(bb);
        codegen
            .builder
            .build_return(Some(&context.i32_type().const_int(42, false)))
            .unwrap();

        let ir = generate_llvm_ir(&codegen);
        assert!(ir.contains("define i32 @test()"));
        assert!(ir.contains("ret i32 42"));
    }
}
