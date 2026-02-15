//! Optimization pass infrastructure
//!
//! This module defines the traits and types for implementing optimization passes.

use jet_ir::{Function, Module};

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

/// A pass that transforms a function.
pub trait FunctionPass {
    /// The name of this pass (for debugging).
    fn name(&self) -> &str;

    /// Runs the pass on a function.
    fn run(&mut self, func: &mut Function) -> PassResult;

    /// Returns true if this pass should run on the given function.
    fn should_run(&self, _func: &Function) -> bool {
        true
    }
}

/// A pass that transforms an entire module.
pub trait ModulePass {
    /// The name of this pass (for debugging).
    fn name(&self) -> &str;

    /// Runs the pass on a module.
    fn run(&mut self, module: &mut Module) -> PassResult;
}

/// A pass that can operate at either function or module level.
pub trait Pass {
    /// The name of this pass.
    fn name(&self) -> &str;

    /// Runs the pass on a function.
    fn run_on_function(&mut self, func: &mut Function) -> PassResult;

    /// Runs the pass on a module.
    fn run_on_module(&mut self, module: &mut Module) -> PassResult;

    /// Returns true if this pass should run on the given function.
    fn should_run_on_function(&self, _func: &Function) -> bool {
        true
    }
}

/// Blanket implementation for types that implement both FunctionPass and ModulePass.
impl<T> Pass for T
where
    T: FunctionPass + ModulePass,
{
    fn name(&self) -> &str {
        FunctionPass::name(self)
    }

    fn run_on_function(&mut self, func: &mut Function) -> PassResult {
        FunctionPass::run(self, func)
    }

    fn run_on_module(&mut self, module: &mut Module) -> PassResult {
        ModulePass::run(self, module)
    }

    fn should_run_on_function(&self, func: &Function) -> bool {
        FunctionPass::should_run(self, func)
    }
}

/// A pass manager that runs multiple passes on a module.
pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
    max_iterations: usize,
}

impl PassManager {
    /// Creates a new empty pass manager.
    pub fn new() -> Self {
        Self {
            passes: Vec::new(),
            max_iterations: 10,
        }
    }

    /// Sets the maximum number of iterations for fixed-point passes.
    pub fn with_max_iterations(mut self, max: usize) -> Self {
        self.max_iterations = max;
        self
    }

    /// Adds a pass to the manager.
    pub fn add_pass(&mut self, pass: Box<dyn Pass>) {
        self.passes.push(pass);
    }

    /// Runs all passes on a module.
    pub fn run(&mut self, module: &mut Module) -> Vec<(String, PassResult)> {
        let mut results = Vec::new();

        for pass in &mut self.passes {
            // First run module-level transformations
            let module_result = pass.run_on_module(module);
            results.push((pass.name().to_string(), module_result));

            // Then run function-level transformations on each function
            for func in &mut module.functions {
                if pass.should_run_on_function(func) {
                    let func_result = pass.run_on_function(func);
                    results.push((format!("{} on {}", pass.name(), func.name), func_result));
                }
            }
        }

        results
    }

    /// Runs passes until a fixed point is reached (no more changes).
    pub fn run_to_fixed_point(&mut self, module: &mut Module) -> Vec<(String, PassResult)> {
        let mut all_results = Vec::new();
        let mut iterations = 0;

        loop {
            let mut any_changed = false;
            let results = self.run(module);

            for (name, result) in results {
                if result.changed() {
                    any_changed = true;
                }
                all_results.push((name, result));
            }

            iterations += 1;

            if !any_changed || iterations >= self.max_iterations {
                break;
            }
        }

        all_results
    }

    /// Returns the number of passes in the manager.
    pub fn len(&self) -> usize {
        self.passes.len()
    }

    /// Returns true if there are no passes.
    pub fn is_empty(&self) -> bool {
        self.passes.is_empty()
    }
}

impl Default for PassManager {
    fn default() -> Self {
        Self::new()
    }
}

/// A validation pass that checks the IR is well-formed.
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

impl FunctionPass for ValidationPass {
    fn name(&self) -> &str {
        "validation"
    }

    fn run(&mut self, func: &mut Function) -> PassResult {
        match func.validate() {
            Ok(()) => PassResult::Unchanged,
            Err(e) => {
                eprintln!("Validation error in function '{}': {}", func.name, e);
                PassResult::Error("validation failed")
            }
        }
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
                eprintln!("Validation error in module '{}': {}", module.name, e);
                PassResult::Error("validation failed")
            }
        }
    }
}

/// A print pass for debugging.
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

impl FunctionPass for PrintPass {
    fn name(&self) -> &str {
        "print"
    }

    fn run(&mut self, func: &mut Function) -> PassResult {
        println!("{} function {}:\n{}", self.prefix, func.name, func);
        PassResult::Unchanged
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
    use jet_ir::builder::ModuleBuilder;
    use jet_ir::types::Ty;

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
            let mut func = builder.function("test", Vec::<(String, Ty)>::new(), Ty::Void);
            func.block("entry");
            func.ret_void();
            func.build();
        }

        let mut module = builder.build();

        let mut pm = PassManager::new();
        pm.add_pass(Box::new(ValidationPass::new()));

        let results = pm.run(&mut module);
        assert!(!results.is_empty());
    }

    #[test]
    fn test_validation_pass() {
        let mut builder = ModuleBuilder::new("test");

        {
            let mut func = builder.function("test", Vec::<(String, Ty)>::new(), Ty::Void);
            func.block("entry");
            func.ret_void();
            func.build();
        }

        let mut module = builder.build();

        let mut pass = ValidationPass::new();
        let result = ModulePass::run(&mut pass, &mut module);

        assert!(result.is_ok());
    }
}
