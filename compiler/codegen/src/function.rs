//! Function Compilation
//!
//! This module handles the compilation of Jet IR functions to LLVM functions.
//! It includes function declaration, body compilation, and parameter handling.

use crate::coerce::coerce_value;
use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use crate::instr::compile_instruction;
use crate::types::TypeMapping;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::FunctionValue;
use jet_ir::{BasicBlock, Function, Terminator};

/// Maps Jet builtin function names to runtime function names.
///
/// For example, "println" maps to "jet_println".
fn map_builtin_name(name: &str) -> Option<String> {
    match name {
        "print" => Some("jet_print".to_string()),
        "println" => Some("jet_println".to_string()),
        "eprint" => Some("jet_eprint".to_string()),
        "eprintln" => Some("jet_eprintln".to_string()),
        "panic" => Some("jet_panic".to_string()),
        "assert" => Some("jet_assert".to_string()),
        "assert_eq" => Some("jet_assert_eq".to_string()),
        "print_int" => Some("jet_print_int".to_string()),
        "print_float" => Some("jet_print_float".to_string()),
        "print_bool" => Some("jet_print_bool".to_string()),
        _ => None,
    }
}

/// Compiles a Jet IR function to an LLVM function.
///
/// This is the main entry point for function compilation. It handles:
/// 1. Creating the function type and declaration
/// 2. Mapping parameters to value IDs
/// 3. Creating basic blocks
/// 4. Compiling instructions in each block
/// 5. Compiling terminators
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `func` - The Jet IR function to compile
///
/// # Returns
///
/// Returns the compiled LLVM function value.
pub fn compile_function<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    func: &Function,
) -> CodegenResult<FunctionValue<'ctx>> {
    // Get the actual LLVM function name (might be renamed from IR name)
    let llvm_func_name = if func.name == "main" {
        "jet_main".to_string()
    } else if let Some(builtin_name) = map_builtin_name(&func.name) {
        builtin_name
    } else {
        func.name.clone()
    };

    // Get the function value. If not declared yet (e.g. unit tests calling
    // compile_function directly), declare and register it now.
    let fn_val = match codegen.get_function(&llvm_func_name) {
        Ok(existing) => existing,
        Err(_) => {
            let declared = declare_function(codegen, func)?;
            codegen.set_function(llvm_func_name.clone(), declared);
            declared
        }
    };

    // If external, we're done
    if func.is_external {
        return Ok(fn_val);
    }

    // Clear per-function state
    codegen.clear_function_state();
    codegen.set_current_function(fn_val);

    // Map function parameters to value IDs
    map_parameters(codegen, func, fn_val)?;

    // Create basic blocks
    create_basic_blocks(codegen, func, fn_val)?;

    // Compile each basic block
    for block in &func.blocks {
        compile_basic_block(codegen, block)?;
    }

    // Run optimization passes on the function
    codegen.optimize_function(fn_val);

    Ok(fn_val)
}

/// Declares a function in the LLVM module.
///
/// This creates the function type and adds it to the module,
/// but does not compile the body.
pub fn declare_function<'ctx>(
    codegen: &CodeGen<'ctx>,
    func: &Function,
) -> CodegenResult<FunctionValue<'ctx>> {
    // Convert parameter types
    let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = func
        .params
        .iter()
        .map(|p| {
            codegen
                .jet_to_llvm(&p.ty)
                .map(|t| t.as_basic_type_enum().into())
        })
        .collect::<CodegenResult<Vec<_>>>()?;

    // Create function type
    let fn_type = if func.return_ty.is_void() {
        codegen.context.void_type().fn_type(&param_types, false)
    } else {
        let ret_type = codegen.jet_to_llvm(&func.return_ty)?;
        ret_type.fn_type(&param_types, false)
    };

    // Map function name: "main" -> "jet_main", builtins -> "jet_*"
    let func_name = if func.name == "main" {
        "jet_main".to_string()
    } else if let Some(builtin_name) = map_builtin_name(&func.name) {
        builtin_name
    } else {
        func.name.clone()
    };

    // Add function to module
    let fn_val = codegen.module.add_function(&func_name, fn_type, None);

    // Set linkage for exported functions
    if func.is_exported {
        fn_val.set_linkage(inkwell::module::Linkage::External);
    }

    Ok(fn_val)
}

/// Maps function parameters to value IDs.
///
/// This associates the LLVM function parameters with the Jet IR value IDs
/// so they can be referenced in the function body.
fn map_parameters<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    func: &Function,
    fn_val: FunctionValue<'ctx>,
) -> CodegenResult<()> {
    for (i, param) in func.params.iter().enumerate() {
        let llvm_param = fn_val.get_nth_param(i as u32).ok_or_else(|| {
            CodegenError::invalid_operand("function parameter", format!("param {}", i))
        })?;

        // Store the parameter value
        codegen.set_value(param.value, llvm_param);

        // Also store the parameter name for debugging
        llvm_param.set_name(&param.name);
    }

    Ok(())
}

/// Creates LLVM basic blocks for all blocks in the function.
///
/// This must be done before compiling instructions so that branch
/// targets can be resolved.
fn create_basic_blocks<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    func: &Function,
    fn_val: FunctionValue<'ctx>,
) -> CodegenResult<()> {
    for (i, block) in func.blocks.iter().enumerate() {
        let llvm_block = if i == 0 {
            // First block is the entry block - use the existing one
            fn_val
                .get_first_basic_block()
                .unwrap_or_else(|| codegen.context.append_basic_block(fn_val, "entry"))
        } else {
            // Create a new block with the given name or a default
            let block_name = block.name.as_deref().unwrap_or("bb");
            codegen.context.append_basic_block(fn_val, block_name)
        };

        codegen.set_block(block.id, llvm_block);
    }

    Ok(())
}

/// Compiles a single basic block.
///
/// This positions the builder at the block and compiles all
/// instructions and the terminator.
fn compile_basic_block<'ctx>(codegen: &mut CodeGen<'ctx>, block: &BasicBlock) -> CodegenResult<()> {
    let llvm_block = codegen.get_block(block.id)?;
    codegen.builder.position_at_end(llvm_block);

    // Compile block parameters (alloca for each parameter)
    for param in &block.params {
        let param_ty = codegen.jet_to_llvm(&param.ty)?;
        let alloca = codegen
            .builder
            .build_alloca(param_ty, &param.name)
            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
        codegen.set_value(param.value, alloca.into());
        codegen.set_stack_slot(param.value, alloca);
    }

    // Compile instructions
    for inst in &block.instructions {
        compile_instruction(codegen, inst)?;
    }

    // Compile terminator
    compile_terminator(codegen, &block.terminator)?;

    Ok(())
}

/// Compiles a terminator instruction.
fn compile_terminator<'ctx>(codegen: &CodeGen<'ctx>, term: &Terminator) -> CodegenResult<()> {
    match term {
        Terminator::Return(val) => {
            // Get the function's declared return type from the current function
            let current_func = codegen.current_function().ok_or_else(|| {
                CodegenError::instruction_error("no current function for return".to_string())
            })?;
            let fn_type = current_func.get_type();
            let ret_type = fn_type.get_return_type();

            if let Some(val_id) = val {
                let val = codegen.get_value(*val_id)?;

                // If the value is an empty struct (which represents Void), treat it as void return
                if val.is_struct_value() && val.into_struct_value().get_type().count_fields() == 0 {
                    codegen
                        .builder
                        .build_return(None)
                        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                } else if let Some(expected_ret_ty) = ret_type {
                    // We have a declared return type - coerce the value to match
                    let expected_basic_ty = expected_ret_ty.as_basic_type_enum();

                    if val.get_type() == expected_basic_ty {
                        // Types match exactly, no coercion needed
                        codegen
                            .builder
                            .build_return(Some(&val))
                            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                    } else {
                        // Need to coerce the return value
                        match coerce_value(codegen, val, expected_basic_ty) {
                            Ok(coerced_val) => {
                                codegen
                                    .builder
                                    .build_return(Some(&coerced_val))
                                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                            }
                            Err(_) => {
                                // If coercion fails, try building return anyway
                                // LLVM will report if there's a type mismatch
                                codegen
                                    .builder
                                    .build_return(Some(&val))
                                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                            }
                        }
                    }
                } else {
                    // Function returns void but we're returning a value - this is an error
                    // However, if it's an empty struct, treat as void
                    if val.is_struct_value()
                        && val.into_struct_value().get_type().count_fields() == 0
                    {
                        codegen
                            .builder
                            .build_return(None)
                            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                    } else {
                        codegen
                            .builder
                            .build_return(Some(&val))
                            .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
                    }
                }
            } else {
                codegen
                    .builder
                    .build_return(None)
                    .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
            }
            Ok(())
        }

        Terminator::Branch(block_id) => {
            let target = codegen.get_block(*block_id)?;
            codegen
                .builder
                .build_unconditional_branch(target)
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
            Ok(())
        }

        Terminator::CondBranch {
            cond,
            then_block,
            else_block,
        } => {
            let cond_val = codegen.get_value(*cond)?.into_int_value();
            let then_bb = codegen.get_block(*then_block)?;
            let else_bb = codegen.get_block(*else_block)?;

            codegen
                .builder
                .build_conditional_branch(cond_val, then_bb, else_bb)
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
            Ok(())
        }

        Terminator::Switch {
            value,
            default_block,
            cases,
        } => {
            let val = codegen.get_value(*value)?.into_int_value();
            let default_bb = codegen.get_block(*default_block)?;

            // Build cases slice
            let llvm_cases: Vec<_> = cases
                .iter()
                .map(|(case_val, case_block)| {
                    let case_bb = codegen.get_block(*case_block)?;
                    let llvm_case_val = val.get_type().const_int(*case_val as u64, true);
                    Ok((llvm_case_val, case_bb))
                })
                .collect::<CodegenResult<Vec<_>>>()?;

            // Create the switch instruction
            codegen
                .builder
                .build_switch(val, default_bb, &llvm_cases)
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

            Ok(())
        }

        Terminator::Unreachable => {
            codegen
                .builder
                .build_unreachable()
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
            Ok(())
        }
    }
}

/// Declares all functions in a module without compiling their bodies.
///
/// This is useful for forward references - we need to declare all
/// functions first so that calls can reference them.
pub fn declare_all_functions<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    functions: &[Function],
) -> CodegenResult<()> {
    for func in functions {
        let fn_val = declare_function(codegen, func)?;

        // Use the renamed function name if it's main
        let func_name = if func.name == "main" {
            "jet_main".to_string()
        } else {
            func.name.clone()
        };

        codegen.set_function(func_name, fn_val);
    }

    Ok(())
}

/// Compiles all functions in a module.
///
/// This declares all functions first, then compiles their bodies.
pub fn compile_all_functions<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    functions: &[Function],
) -> CodegenResult<()> {
    // First pass: declare all functions
    declare_all_functions(codegen, functions)?;

    // Second pass: compile function bodies
    for func in functions {
        // Skip external functions (they have no body)
        if !func.is_external {
            compile_function(codegen, func)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::context::Context;
    use jet_ir::{BinaryOp, BlockId, ConstantValue, Instruction, Param, Ty, ValueId};

    fn create_simple_add_function() -> Function {
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
        func
    }

    #[test]
    fn test_declare_function() {
        let context = Context::create();
        let codegen = CodeGen::new(&context, "test");

        let func = create_simple_add_function();
        let fn_val = declare_function(&codegen, &func).unwrap();

        assert_eq!(fn_val.get_name().to_str().unwrap(), "add");
        assert_eq!(fn_val.count_params(), 2);
    }

    #[test]
    fn test_compile_function() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        let func = create_simple_add_function();
        let fn_val = compile_function(&mut codegen, &func).unwrap();

        assert!(fn_val.verify(true));
    }

    #[test]
    fn test_external_function() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        let func = Function::external(
            "external_func",
            vec![Param::new("x", Ty::I32, ValueId::new(0))],
            Ty::I32,
        );

        let fn_val = compile_function(&mut codegen, &func).unwrap();
        // External functions have no basic blocks
        assert!(fn_val.get_basic_blocks().is_empty());
    }

    #[test]
    fn test_conditional_branch() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        let mut func = Function::new("test_cond", vec![], Ty::Void);

        // Block 0: entry - conditional branch
        let mut block0 = BasicBlock::new(BlockId::new(0));
        block0.name = Some("entry".to_string());
        block0.add_instruction(Instruction::Const {
            result: ValueId::new(0),
            value: ConstantValue::Bool(true),
        });
        block0.set_terminator(Terminator::CondBranch {
            cond: ValueId::new(0),
            then_block: BlockId::new(1),
            else_block: BlockId::new(2),
        });
        func.add_block(block0);

        // Block 1: then
        let mut block1 = BasicBlock::new(BlockId::new(1));
        block1.name = Some("then".to_string());
        block1.set_terminator(Terminator::Return(None));
        func.add_block(block1);

        // Block 2: else
        let mut block2 = BasicBlock::new(BlockId::new(2));
        block2.name = Some("else".to_string());
        block2.set_terminator(Terminator::Return(None));
        func.add_block(block2);

        let fn_val = compile_function(&mut codegen, &func).unwrap();
        assert!(fn_val.verify(true));
    }

    #[test]
    fn test_switch_terminator() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        let mut func = Function::new("test_switch", vec![], Ty::Void);

        // Block 0: entry - switch
        let mut block0 = BasicBlock::new(BlockId::new(0));
        block0.name = Some("entry".to_string());
        block0.add_instruction(Instruction::Const {
            result: ValueId::new(0),
            value: ConstantValue::Int(1, Ty::I32),
        });
        block0.set_terminator(Terminator::Switch {
            value: ValueId::new(0),
            default_block: BlockId::new(3),
            cases: vec![(0, BlockId::new(1)), (1, BlockId::new(2))],
        });
        func.add_block(block0);

        // Block 1: case 0
        let mut block1 = BasicBlock::new(BlockId::new(1));
        block1.set_terminator(Terminator::Return(None));
        func.add_block(block1);

        // Block 2: case 1
        let mut block2 = BasicBlock::new(BlockId::new(2));
        block2.set_terminator(Terminator::Return(None));
        func.add_block(block2);

        // Block 3: default
        let mut block3 = BasicBlock::new(BlockId::new(3));
        block3.set_terminator(Terminator::Return(None));
        func.add_block(block3);

        let fn_val = compile_function(&mut codegen, &func).unwrap();
        assert!(fn_val.verify(true));
    }

    #[test]
    fn test_declare_all_functions() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");

        let funcs = vec![
            Function::new("foo", vec![], Ty::I32),
            Function::new("bar", vec![], Ty::Void),
        ];

        declare_all_functions(&mut codegen, &funcs).unwrap();

        assert!(codegen.get_function("foo").is_ok());
        assert!(codegen.get_function("bar").is_ok());
    }
}
