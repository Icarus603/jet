//! IR transformation passes for Jet IR.
//!
//! This module defines traits and utilities for writing IR optimization
//! and transformation passes.

use crate::function::Function;
use crate::Module;

/// A pass that transforms a function.
pub trait FunctionPass {
    /// The name of this pass (for debugging).
    fn name(&self) -> &str;

    /// Runs the pass on a function.
    fn run(&mut self, func: &mut Function) -> PassResult;
}

/// A pass that transforms an entire module.
pub trait ModulePass {
    /// The name of this pass (for debugging).
    fn name(&self) -> &str;

    /// Runs the pass on a module.
    fn run(&mut self, module: &mut Module) -> PassResult;
}

/// The result of running a pass.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PassResult {
    /// The pass made no changes.
    Unchanged,
    /// The pass made changes.
    Changed,
    /// The pass encountered an error.
    Error(&'static str),
}

impl PassResult {
    /// Returns true if the pass made changes.
    pub fn changed(&self) -> bool {
        matches!(self, PassResult::Changed)
    }

    /// Returns true if the pass succeeded (with or without changes).
    pub fn is_ok(&self) -> bool {
        !matches!(self, PassResult::Error(_))
    }

    /// Returns true if the pass encountered an error.
    pub fn is_err(&self) -> bool {
        matches!(self, PassResult::Error(_))
    }
}

/// A pass manager that runs multiple passes on a module.
pub struct PassManager {
    module_passes: Vec<Box<dyn ModulePass>>,
    function_passes: Vec<Box<dyn FunctionPass>>,
}

impl PassManager {
    /// Creates a new empty pass manager.
    pub fn new() -> Self {
        Self {
            module_passes: Vec::new(),
            function_passes: Vec::new(),
        }
    }

    /// Adds a module pass.
    pub fn add_module_pass(&mut self, pass: Box<dyn ModulePass>) {
        self.module_passes.push(pass);
    }

    /// Adds a function pass.
    pub fn add_function_pass(&mut self, pass: Box<dyn FunctionPass>) {
        self.function_passes.push(pass);
    }

    /// Runs all passes on a module.
    pub fn run(&mut self, module: &mut Module) -> Vec<(String, PassResult)> {
        let mut results = Vec::new();

        // Run module passes
        for pass in &mut self.module_passes {
            let result = pass.run(module);
            results.push((pass.name().to_string(), result));
        }

        // Run function passes on each function
        for pass in &mut self.function_passes {
            for func in &mut module.functions {
                let result = pass.run(func);
                results.push((format!("{} on {}", pass.name(), func.name), result));
            }
        }

        results
    }
}

impl Default for PassManager {
    fn default() -> Self {
        Self::new()
    }
}

/// A simple dead code elimination pass.
pub struct DeadCodeElimination;

impl DeadCodeElimination {
    /// Creates a new DCE pass.
    pub fn new() -> Self {
        Self
    }
}

impl Default for DeadCodeElimination {
    fn default() -> Self {
        Self::new()
    }
}

impl FunctionPass for DeadCodeElimination {
    fn name(&self) -> &str {
        "dead-code-elimination"
    }

    fn run(&mut self, func: &mut Function) -> PassResult {
        let mut changed = false;

        for block in &mut func.blocks {
            let original_len = block.instructions.len();

            // Remove instructions that have no side effects
            // Instructions with side effects are always kept
            block.instructions.retain(|inst| inst.has_side_effects());

            if block.instructions.len() != original_len {
                changed = true;
            }
        }

        if changed {
            PassResult::Changed
        } else {
            PassResult::Unchanged
        }
    }
}

/// A pass that validates the IR.
pub struct ValidationPass;

impl ValidationPass {
    /// Creates a new validation pass.
    pub fn new() -> Self {
        Self
    }
}

impl Default for ValidationPass {
    fn default() -> Self {
        Self::new()
    }
}

impl ModulePass for ValidationPass {
    fn name(&self) -> &str {
        "validation"
    }

    fn run(&mut self, module: &mut Module) -> PassResult {
        match module.validate() {
            Ok(()) => PassResult::Unchanged,
            Err(e) => {
                eprintln!("Validation error: {}", e);
                PassResult::Error("validation failed")
            }
        }
    }
}

/// A pass that prints the module (for debugging).
pub struct PrintPass {
    prefix: String,
}

impl PrintPass {
    /// Creates a new print pass with the given prefix.
    pub fn new(prefix: impl Into<String>) -> Self {
        Self {
            prefix: prefix.into(),
        }
    }
}

impl ModulePass for PrintPass {
    fn name(&self) -> &str {
        "print"
    }

    fn run(&mut self, module: &mut Module) -> PassResult {
        println!("{}\n{}", self.prefix, module);
        PassResult::Unchanged
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builder::ModuleBuilder;
    use crate::types::Ty;

    #[test]
    fn test_pass_result() {
        assert!(PassResult::Changed.changed());
        assert!(!PassResult::Unchanged.changed());
        assert!(PassResult::Unchanged.is_ok());
        assert!(!PassResult::Error("test").is_ok());
        assert!(PassResult::Error("test").is_err());
    }

    #[test]
    fn test_pass_manager() {
        let mut builder = ModuleBuilder::new("test");

        {
            let mut func = builder.function("test", Vec::<(&str, Ty)>::new(), Ty::Void);
            func.block("entry");
            func.ret_void();
            func.build();
        }

        let mut module = builder.build();

        let mut pm = PassManager::new();
        pm.add_module_pass(Box::new(ValidationPass::new()));
        pm.add_function_pass(Box::new(DeadCodeElimination::new()));

        let results = pm.run(&mut module);
        assert!(!results.is_empty());
    }

    #[test]
    fn test_dead_code_elimination() {
        use crate::instruction::{ConstantValue, Instruction};
        use crate::values::ValueId;

        let mut func = Function::new("test", vec![], Ty::Void);
        let mut block = crate::function::BasicBlock::new(crate::values::BlockId::new(0));

        // Add a dead instruction (no side effects, result unused)
        let result = ValueId::new(0);
        block.add_instruction(Instruction::Const {
            result,
            value: ConstantValue::Int(42, Ty::I32),
        });

        // Add a store (has side effects, should be kept)
        let ptr = ValueId::new(1);
        let val = ValueId::new(2);
        block.add_instruction(Instruction::Store { ptr, value: val });

        block.set_terminator(crate::terminator::Terminator::Return(None));
        func.add_block(block);

        let mut dce = DeadCodeElimination::new();
        let result = dce.run(&mut func);

        // The const should be eliminated, store should remain
        assert!(result.changed());
        assert_eq!(func.blocks[0].instructions.len(), 1);
    }

    #[test]
    fn test_validation_pass() {
        let mut builder = ModuleBuilder::new("test");

        {
            let mut func = builder.function("test", Vec::<(&str, Ty)>::new(), Ty::Void);
            func.block("entry");
            func.ret_void();
            func.build();
        }

        let mut module = builder.build();

        let mut pass = ValidationPass::new();
        let result = pass.run(&mut module);

        assert!(result.is_ok());
    }
}
