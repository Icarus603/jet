//! Jet Code Generation
//!
//! This crate provides code generation backends for the Jet compiler.
//! Currently supports LLVM via the inkwell crate.
//!
//! # Architecture
//!
//! The codegen crate is organized into several modules:
//!
//! - `context`: The main `CodeGen` struct that holds LLVM state
//! - `types`: Type mapping from Jet IR to LLVM
//! - `instr`: Instruction compilation
//! - `function`: Function compilation
//! - `effect`: Effect system handling
//! - `object`: Object file generation
//! - `gc`: GC integration (allocation hooks, stack maps)
//! - `glue`: Runtime glue code (prologue/epilogue, safepoints, FFI marshaling)
//! - `closure`: Closure compilation
//! - `trait_obj`: Trait object and vtable generation
//! - `pattern`: Pattern matching compilation
//! - `async_gen`: Async/await state machine generation
//!
//! # Example
//!
//! ```rust
//! use inkwell::context::Context;
//! use jet_codegen::CodeGen;
//! use jet_ir::Module;
//!
//! let context = Context::create();
//! let mut codegen = CodeGen::new(&context, "my_module");
//!
//! // Compile a Jet IR module
//! let ir_module = Module::new("test");
//! jet_codegen::compile_module(&mut codegen, &ir_module).unwrap();
//!
//! // Generate LLVM IR
//! let llvm_ir = codegen.print_to_string();
//! ```

#![allow(deprecated)]

pub mod async_gen;
pub mod closure;
pub mod context;
pub mod effect;
pub mod error;
pub mod function;
pub mod gc;
pub mod global;
pub mod glue;
pub mod instr;
pub mod llvm;
pub mod object;
pub mod pattern;
pub mod trait_obj;
pub mod types;

pub use async_gen::{
    compile_async_function, compile_await, compile_spawn, AsyncFunctionInfo, AsyncStage,
};
pub use closure::{closure_type, compile_closure, compile_closure_call, ClosureInfo};
pub use context::CodeGen;
pub use error::{CodegenError, CodegenResult};
pub use gc::{
    emit_gc_alloc, emit_gc_alloc_typed, emit_gc_root, emit_safepoint_poll, emit_write_barrier,
    AllocationSite,
};
pub use global::{compile_globals, compile_type_definitions, get_named_type};
pub use glue::{
    emit_function_epilogue, emit_function_prologue, emit_gc_alloc_call, emit_init_object_header,
    emit_safepoint_poll as emit_glue_safepoint, emit_task_spawn, emit_write_barrier_call,
    marshal_for_ffi, marshal_from_ffi, register_gc_root, update_gc_root, GcRoot, GcRootSet,
};
pub use llvm::LLVMBackend;
pub use object::{compile_to_object, generate_llvm_ir, generate_object_file, ObjectConfig};
pub use pattern::{compile_match, MatchCase, Pattern, PatternLiteral};
pub use trait_obj::{call_trait_method, create_trait_object, generate_vtable, ImplInfo, TraitInfo};

use jet_ir::Module as IrModule;

/// Creates a C-compatible main wrapper that calls the Jet main function.
///
/// This generates a C `main` entry point with the signature:
/// `int main(int argc, char** argv)`
///
/// It calls the renamed Jet main (`jet_main`) and returns 0.
fn create_c_main_wrapper<'ctx>(codegen: &mut CodeGen<'ctx>) -> CodegenResult<()> {
    let context = codegen.context;
    let module = &codegen.module;

    // Get the Jet main function (which has been renamed to "jet_main")
    let jet_main = module
        .get_function("jet_main")
        .ok_or_else(|| CodegenError::function_not_found("jet_main".to_string()))?;

    // Create the C main function: int main(int argc, char** argv)
    let i32_type = context.i32_type();
    let i8_ptr_type = context.i8_type().ptr_type(inkwell::AddressSpace::default());
    let i8_ptr_ptr_type = i8_ptr_type.ptr_type(inkwell::AddressSpace::default());

    let c_main_type = i32_type.fn_type(&[i32_type.into(), i8_ptr_ptr_type.into()], false);

    let c_main = module.add_function("main", c_main_type, None);

    // Create entry block
    let entry_block = context.append_basic_block(c_main, "entry");
    codegen.builder.position_at_end(entry_block);

    // Call the Jet main function
    match codegen.builder.build_call(jet_main, &[], "call_jet_main") {
        Ok(_) => {}
        Err(e) => return Err(CodegenError::instruction_error(e.to_string())),
    }

    // Return 0
    match codegen
        .builder
        .build_return(Some(&i32_type.const_int(0, false)))
    {
        Ok(_) => {}
        Err(e) => return Err(CodegenError::instruction_error(e.to_string())),
    }

    Ok(())
}

/// Compiles a Jet IR module to LLVM IR.
///
/// This is the main entry point for code generation. It compiles all
/// functions, globals, and type definitions in the module.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `ir_module` - The Jet IR module to compile
///
/// # Returns
///
/// Returns Ok(()) on success, or a CodegenError on failure.
///
/// # Example
///
/// ```rust
/// use inkwell::context::Context;
/// use jet_codegen::{CodeGen, compile_module};
/// use jet_ir::Module;
///
/// let context = Context::create();
/// let mut codegen = CodeGen::new(&context, "test");
/// let ir_module = Module::new("test");
///
/// compile_module(&mut codegen, &ir_module).unwrap();
/// ```
pub fn compile_module<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    ir_module: &IrModule,
) -> CodegenResult<()> {
    // Compile type definitions first (they may be referenced by functions/globals)
    global::compile_type_definitions(codegen, &ir_module.type_defs)?;

    // Compile global variables
    global::compile_globals(codegen, &ir_module.globals)?;

    // Check if there's a main function that needs wrapping
    let has_main = ir_module.functions.iter().any(|f| f.name == "main");

    // Compile all functions (main will be renamed to jet_main in declare_function)
    function::compile_all_functions(codegen, &ir_module.functions)?;

    // If there was a main function, create a C main wrapper
    if has_main {
        create_c_main_wrapper(codegen)?;
    }

    // Verify the module
    codegen.verify_module()?;

    Ok(())
}

/// Compiles a Jet IR module to an object file.
///
/// This is a convenience function that compiles the module and writes
/// an object file in one step.
///
/// # Arguments
///
/// * `ir_module` - The Jet IR module to compile
/// * `output_path` - The path to write the object file to
///
/// # Returns
///
/// Returns Ok(()) on success, or a CodegenError on failure.
pub fn compile_to_object_file(
    ir_module: &IrModule,
    output_path: &std::path::Path,
) -> CodegenResult<()> {
    let context = inkwell::context::Context::create();
    let mut codegen = CodeGen::new(&context, &ir_module.name);

    compile_module(&mut codegen, ir_module)?;
    object::compile_to_object(&codegen, output_path)
}

/// Compiles a Jet IR module to LLVM IR text.
///
/// This is a convenience function that compiles the module and returns
/// the LLVM IR as a string.
///
/// # Arguments
///
/// * `ir_module` - The Jet IR module to compile
///
/// # Returns
///
/// Returns the LLVM IR string on success, or a CodegenError on failure.
pub fn compile_to_llvm_ir(ir_module: &IrModule) -> CodegenResult<String> {
    let context = inkwell::context::Context::create();
    let mut codegen = CodeGen::new(&context, &ir_module.name);

    compile_module(&mut codegen, ir_module)?;
    Ok(codegen.print_to_string())
}

/// Configuration for code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodegenConfig {
    /// The optimization level to use.
    pub opt_level: OptimizationLevel,
    /// Whether to enable debug information.
    pub debug_info: bool,
    /// The target triple (None for native).
    pub target_triple: Option<&'static str>,
}

impl Default for CodegenConfig {
    fn default() -> Self {
        Self {
            opt_level: OptimizationLevel::Default,
            debug_info: false,
            target_triple: None,
        }
    }
}

/// Optimization levels for code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    /// No optimizations.
    None,
    /// Less optimizations.
    Less,
    /// Default optimizations.
    Default,
    /// Aggressive optimizations.
    Aggressive,
}

impl From<OptimizationLevel> for inkwell::OptimizationLevel {
    fn from(level: OptimizationLevel) -> Self {
        match level {
            OptimizationLevel::None => inkwell::OptimizationLevel::None,
            OptimizationLevel::Less => inkwell::OptimizationLevel::Less,
            OptimizationLevel::Default => inkwell::OptimizationLevel::Default,
            OptimizationLevel::Aggressive => inkwell::OptimizationLevel::Aggressive,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_ir::{
        BasicBlock, BinaryOp, BlockId, ConstantValue, Function, Instruction, Param, Terminator, Ty,
        ValueId,
    };

    fn create_test_module() -> IrModule {
        let mut module = IrModule::new("test");

        let mut func = Function::new("main", vec![], Ty::I32);

        let mut block = BasicBlock::new(BlockId::new(0));
        block.name = Some("entry".to_string());

        // %0 = const 42
        block.add_instruction(Instruction::Const {
            result: ValueId::new(0),
            value: ConstantValue::Int(42, Ty::I32),
        });

        // ret %0
        block.set_terminator(Terminator::Return(Some(ValueId::new(0))));

        func.add_block(block);
        module.add_function(func);

        module
    }

    fn create_add_module() -> IrModule {
        let mut module = IrModule::new("test");

        let mut func = Function::new(
            "add",
            vec![
                Param::new("a", Ty::I32, ValueId::new(0)),
                Param::new("b", Ty::I32, ValueId::new(1)),
            ],
            Ty::I32,
        );

        let mut block = BasicBlock::new(BlockId::new(0));
        block.name = Some("entry".to_string());

        // %2 = add %0, %1
        block.add_instruction(Instruction::Binary {
            result: ValueId::new(2),
            op: BinaryOp::Add,
            lhs: ValueId::new(0),
            rhs: ValueId::new(1),
        });

        // ret %2
        block.set_terminator(Terminator::Return(Some(ValueId::new(2))));

        func.add_block(block);
        module.add_function(func);

        module
    }

    #[test]
    fn test_compile_simple_function() {
        let module = create_test_module();
        let context = inkwell::context::Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        let result = compile_module(&mut codegen, &module);
        assert!(
            result.is_ok(),
            "Failed to compile module: {:?}",
            result.err()
        );

        let ir = codegen.print_to_string();
        // LLVM IR format may vary slightly, check for key components
        assert!(ir.contains("define"), "IR should contain 'define': {}", ir);
        assert!(ir.contains("@main"), "IR should contain '@main': {}", ir);
        assert!(ir.contains("i32"), "IR should contain 'i32': {}", ir);
    }

    #[test]
    fn test_compile_add_function() {
        let module = create_add_module();
        let context = inkwell::context::Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        let result = compile_module(&mut codegen, &module);
        assert!(
            result.is_ok(),
            "Failed to compile module: {:?}",
            result.err()
        );

        let ir = codegen.print_to_string();
        // LLVM IR format may vary slightly, check for key components
        assert!(ir.contains("define"), "IR should contain 'define': {}", ir);
        assert!(ir.contains("@add"), "IR should contain '@add': {}", ir);
        assert!(ir.contains("add"), "IR should contain 'add': {}", ir);
    }

    #[test]
    fn test_compile_to_llvm_ir() {
        let module = create_test_module();
        let ir = compile_to_llvm_ir(&module);
        assert!(ir.is_ok());

        let ir_str = ir.unwrap();
        assert!(ir_str.contains("define"));
        assert!(ir_str.contains("main"));
    }

    #[test]
    fn test_optimization_level_conversion() {
        assert_eq!(
            inkwell::OptimizationLevel::from(OptimizationLevel::None),
            inkwell::OptimizationLevel::None
        );
        assert_eq!(
            inkwell::OptimizationLevel::from(OptimizationLevel::Default),
            inkwell::OptimizationLevel::Default
        );
        assert_eq!(
            inkwell::OptimizationLevel::from(OptimizationLevel::Aggressive),
            inkwell::OptimizationLevel::Aggressive
        );
    }

    #[test]
    fn test_codegen_config_default() {
        let config = CodegenConfig::default();
        assert_eq!(config.opt_level, OptimizationLevel::Default);
        assert!(!config.debug_info);
        assert!(config.target_triple.is_none());
    }
}
