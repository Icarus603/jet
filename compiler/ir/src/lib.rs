//! Jet Intermediate Representation (IR)
//!
//! This crate defines the intermediate representation used by the Jet compiler.
//! The IR is designed to be a low-level, SSA-based representation that can be
//! easily lowered to LLVM IR.

pub mod builder;
pub mod function;
pub mod instruction;
pub mod passes;
pub mod terminator;
pub mod types;
pub mod values;

pub use function::{BasicBlock, Effect, Function};
pub use instruction::{BinaryOp, ConstantValue, Instruction, UnaryOp};
pub use terminator::Terminator;
pub use types::{Ty, TypeDef, TypeId, TypeInterner, TypeKind};
pub use values::{BlockId, Constant, Param, ValueId, ValueInfo, ValueTable};

use std::fmt;

/// A module in the Jet IR.
///
/// A module is the top-level container for all functions and type definitions.
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    /// The name of the module.
    pub name: String,
    /// The functions in the module.
    pub functions: Vec<Function>,
    /// Type definitions in the module.
    pub type_defs: Vec<TypeDef>,
    /// Global values in the module.
    pub globals: Vec<Global>,
}

impl Module {
    /// Creates a new empty module with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            functions: Vec::new(),
            type_defs: Vec::new(),
            globals: Vec::new(),
        }
    }

    /// Adds a function to the module.
    pub fn add_function(&mut self, func: Function) {
        self.functions.push(func);
    }

    /// Adds a type definition to the module.
    pub fn add_type_def(&mut self, type_def: TypeDef) {
        self.type_defs.push(type_def);
    }

    /// Adds a global value to the module.
    pub fn add_global(&mut self, global: Global) {
        self.globals.push(global);
    }

    /// Gets a function by name.
    pub fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.iter().find(|f| f.name == name)
    }

    /// Gets a mutable reference to a function by name.
    pub fn get_function_mut(&mut self, name: &str) -> Option<&mut Function> {
        self.functions.iter_mut().find(|f| f.name == name)
    }

    /// Gets a type definition by name.
    pub fn get_type_def(&self, name: &str) -> Option<&TypeDef> {
        self.type_defs.iter().find(|t| t.name == name)
    }

    /// Validates all functions in the module.
    pub fn validate(&self) -> Result<(), String> {
        for func in &self.functions {
            func.validate()?;
        }
        Ok(())
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "module {} {{\n", self.name)?;

        // Type definitions
        for type_def in &self.type_defs {
            writeln!(f, "    type {} = {:?};", type_def.name, type_def.kind)?;
        }
        if !self.type_defs.is_empty() {
            writeln!(f)?;
        }

        // Globals
        for global in &self.globals {
            writeln!(f, "    {}", global)?;
        }
        if !self.globals.is_empty() {
            writeln!(f)?;
        }

        // Functions
        for func in &self.functions {
            writeln!(f, "{}", func)?;
            writeln!(f)?;
        }

        writeln!(f, "}}")
    }
}

/// A global value in the module.
#[derive(Debug, Clone, PartialEq)]
pub struct Global {
    /// The name of the global.
    pub name: String,
    /// The type of the global.
    pub ty: Ty,
    /// The initial value (if any).
    pub initial_value: Option<Constant>,
    /// Whether this global is exported.
    pub is_exported: bool,
    /// Whether this global is external (defined elsewhere).
    pub is_external: bool,
}

impl Global {
    /// Creates a new global with the given name and type.
    pub fn new(name: impl Into<String>, ty: Ty) -> Self {
        Self {
            name: name.into(),
            ty,
            initial_value: None,
            is_exported: false,
            is_external: false,
        }
    }

    /// Creates a new external global declaration.
    pub fn external(name: impl Into<String>, ty: Ty) -> Self {
        Self {
            name: name.into(),
            ty,
            initial_value: None,
            is_exported: false,
            is_external: true,
        }
    }

    /// Sets the initial value for this global.
    pub fn with_initial_value(mut self, value: Constant) -> Self {
        self.initial_value = Some(value);
        self
    }

    /// Marks this global as exported.
    pub fn set_exported(mut self) -> Self {
        self.is_exported = true;
        self
    }
}

impl fmt::Display for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_external {
            write!(f, "extern ")?;
        }
        if self.is_exported {
            write!(f, "export ")?;
        }
        write!(f, "global {}: {}", self.name, self.ty)?;
        if let Some(ref val) = self.initial_value {
            write!(f, " = {}", val)?;
        }
        write!(f, ";")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_creation() {
        let module = Module::new("test");
        assert_eq!(module.name, "test");
        assert!(module.functions.is_empty());
        assert!(module.type_defs.is_empty());
        assert!(module.globals.is_empty());
    }

    #[test]
    fn test_module_add_function() {
        let mut module = Module::new("test");
        let func = Function::new("main", vec![], Ty::I32);
        module.add_function(func);

        assert_eq!(module.functions.len(), 1);
        assert!(module.get_function("main").is_some());
        assert!(module.get_function("nonexistent").is_none());
    }

    #[test]
    fn test_global_creation() {
        let global = Global::new("counter", Ty::I32)
            .with_initial_value(Constant::i32(0))
            .set_exported();

        assert_eq!(global.name, "counter");
        assert_eq!(global.ty, Ty::I32);
        assert!(global.initial_value.is_some());
        assert!(global.is_exported);
    }

    #[test]
    fn test_global_external() {
        let global = Global::external("extern_var", Ty::I64);
        assert!(global.is_external);
        assert!(global.initial_value.is_none());
    }
}
