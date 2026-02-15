//! Jet IR Optimization Passes
//!
//! This crate provides optimization passes for the Jet intermediate representation.
//! It includes:
//!
//! - **Dead Code Elimination (DCE)**: Removes unreachable code and unused instructions
//! - **Constant Folding**: Evaluates constant expressions at compile time
//! - **Function Inlining**: Inlines small functions to reduce call overhead
//! - **Common Subexpression Elimination (CSE)**: Eliminates redundant computations
//! - **Control Flow Simplification**: Simplifies and optimizes control flow graphs

pub mod const_fold;
pub mod cse;
pub mod dce;
pub mod inline;
pub mod pass;
pub mod simplify;

pub use const_fold::ConstantFolding;
pub use cse::CommonSubexpressionElimination;
pub use dce::DeadCodeElimination;
pub use inline::FunctionInlining;
pub use pass::{FunctionPass, ModulePass, Pass, PassManager, PassResult};
pub use simplify::ControlFlowSimplification;

use jet_ir::Module;

/// Run the standard optimization pipeline on a module.
pub fn optimize_module(module: &mut Module) -> Vec<(String, PassResult)> {
    let mut manager = PassManager::new();

    // Add passes in order
    manager.add_pass(Box::new(ConstantFolding::new()));
    manager.add_pass(Box::new(DeadCodeElimination::new()));
    manager.add_pass(Box::new(ControlFlowSimplification::new()));
    manager.add_pass(Box::new(CommonSubexpressionElimination::new()));
    manager.add_pass(Box::new(FunctionInlining::new()));

    // Run one more round of cleanup after inlining
    manager.add_pass(Box::new(DeadCodeElimination::new()));
    manager.add_pass(Box::new(ConstantFolding::new()));

    manager.run(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_ir::builder::ModuleBuilder;
    use jet_ir::types::Ty;

    #[test]
    fn test_optimize_module() {
        let mut builder = ModuleBuilder::new("test");

        {
            let mut func = builder.function("main", Vec::<(String, Ty)>::new(), Ty::I32);
            func.block("entry");
            func.const_int(42, Ty::I32);
            func.ret_void();
            func.build();
        }

        let mut module = builder.build();
        let results = optimize_module(&mut module);

        assert!(!results.is_empty());
    }
}
