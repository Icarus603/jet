//! Function and basic block definitions for Jet IR.
//!
//! This module defines the structure of functions and basic blocks,
//! including the control flow graph.

use crate::instruction::Instruction;
use crate::terminator::Terminator;
use crate::types::Ty;
use crate::values::{BlockId, Param, ValueId};
use std::fmt;

/// An effect that a function may have.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Effect {
    /// Can raise an error of the given type.
    Raise(Ty),
    /// Is an async function.
    Async,
    /// Performs I/O operations.
    IO,
    /// Allocates memory.
    Alloc,
    /// Diverges (does not return).
    Diverges,
}

impl fmt::Display for Effect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Effect::Raise(ty) => write!(f, "raise({})", ty),
            Effect::Async => write!(f, "async"),
            Effect::IO => write!(f, "io"),
            Effect::Alloc => write!(f, "alloc"),
            Effect::Diverges => write!(f, "diverges"),
        }
    }
}

/// A basic block in the control flow graph.
#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    /// The unique ID of this block.
    pub id: BlockId,
    /// The name of this block (for debugging).
    pub name: Option<String>,
    /// Parameters to this block (for block arguments in SSA).
    pub params: Vec<Param>,
    /// The instructions in this block.
    pub instructions: Vec<Instruction>,
    /// The terminator that ends this block.
    pub terminator: Terminator,
}

impl BasicBlock {
    /// Creates a new basic block with the given ID.
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            name: None,
            params: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Unreachable,
        }
    }

    /// Creates a new basic block with a name.
    pub fn with_name(id: BlockId, name: impl Into<String>) -> Self {
        Self {
            id,
            name: Some(name.into()),
            params: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Unreachable,
        }
    }

    /// Adds an instruction to this block.
    pub fn add_instruction(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }

    /// Sets the terminator for this block.
    pub fn set_terminator(&mut self, terminator: Terminator) {
        self.terminator = terminator;
    }

    /// Returns true if this block has a terminator set.
    pub fn is_terminated(&self) -> bool {
        !matches!(self.terminator, Terminator::Unreachable)
    }

    /// Returns all successor block IDs.
    pub fn successors(&self) -> Vec<BlockId> {
        self.terminator.successors()
    }

    /// Returns all predecessor block IDs (requires CFG analysis).
    pub fn predecessors(&self, func: &Function) -> Vec<BlockId> {
        func.blocks
            .iter()
            .filter(|b| b.successors().contains(&self.id))
            .map(|b| b.id)
            .collect()
    }

    /// Finds an instruction by its result value ID.
    pub fn find_instruction(&self, result: ValueId) -> Option<&Instruction> {
        self.instructions
            .iter()
            .find(|i| i.result() == Some(result))
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name.clone().unwrap_or_else(|| format!("{}", self.id));

        write!(f, "{}:", name)?;

        // Print parameters if any
        if !self.params.is_empty() {
            write!(f, "(")?;
            for (i, param) in self.params.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", param)?;
            }
            write!(f, ")")?;
        }

        writeln!(f)?;

        // Print instructions
        for inst in &self.instructions {
            writeln!(f, "    {}", inst)?;
        }

        // Print terminator
        writeln!(f, "    {}", self.terminator)
    }
}

/// A function in the IR.
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    /// The name of the function.
    pub name: String,
    /// The parameters to the function.
    pub params: Vec<Param>,
    /// The return type of the function.
    pub return_ty: Ty,
    /// The basic blocks in the function.
    pub blocks: Vec<BasicBlock>,
    /// Whether this is an async function.
    pub is_async: bool,
    /// The effects this function may have.
    pub effects: Vec<Effect>,
    /// Whether this function is exported.
    pub is_exported: bool,
    /// Whether this function is external (defined elsewhere).
    pub is_external: bool,
}

impl Function {
    /// Creates a new function with the given name and signature.
    pub fn new(name: impl Into<String>, params: Vec<Param>, return_ty: Ty) -> Self {
        Self {
            name: name.into(),
            params,
            return_ty,
            blocks: Vec::new(),
            is_async: false,
            effects: Vec::new(),
            is_exported: false,
            is_external: false,
        }
    }

    /// Creates a new external function declaration.
    pub fn external(name: impl Into<String>, params: Vec<Param>, return_ty: Ty) -> Self {
        Self {
            name: name.into(),
            params,
            return_ty,
            blocks: Vec::new(),
            is_async: false,
            effects: Vec::new(),
            is_exported: false,
            is_external: true,
        }
    }

    /// Adds a basic block to this function.
    pub fn add_block(&mut self, block: BasicBlock) {
        self.blocks.push(block);
    }

    /// Creates a new basic block and adds it to the function.
    pub fn create_block(&mut self, id: BlockId) -> &mut BasicBlock {
        self.blocks.push(BasicBlock::new(id));
        self.blocks.last_mut().unwrap()
    }

    /// Gets a block by its ID.
    pub fn get_block(&self, id: BlockId) -> Option<&BasicBlock> {
        self.blocks.iter().find(|b| b.id == id)
    }

    /// Gets a mutable reference to a block by its ID.
    pub fn get_block_mut(&mut self, id: BlockId) -> Option<&mut BasicBlock> {
        self.blocks.iter_mut().find(|b| b.id == id)
    }

    /// Returns the entry block (first block).
    pub fn entry_block(&self) -> Option<&BasicBlock> {
        self.blocks.first()
    }

    /// Marks this function as async.
    pub fn set_async(mut self) -> Self {
        self.is_async = true;
        self.effects.push(Effect::Async);
        self
    }

    /// Adds an effect to this function.
    pub fn add_effect(mut self, effect: Effect) -> Self {
        self.effects.push(effect);
        self
    }

    /// Marks this function as exported.
    pub fn set_exported(mut self) -> Self {
        self.is_exported = true;
        self
    }

    /// Returns true if this function has the given effect.
    pub fn has_effect(&self, effect: &Effect) -> bool {
        self.effects.contains(effect)
    }

    /// Returns true if this function can raise errors.
    pub fn can_raise(&self) -> Option<&Ty> {
        self.effects.iter().find_map(|e| match e {
            Effect::Raise(ty) => Some(ty),
            _ => None,
        })
    }

    /// Validates that the function is well-formed.
    pub fn validate(&self) -> Result<(), String> {
        // Check that all blocks are terminated
        for block in &self.blocks {
            if !block.is_terminated() {
                return Err(format!("Block {} is not terminated", block.id.0));
            }
        }

        // Check that block IDs are unique
        let mut ids = std::collections::HashSet::new();
        for block in &self.blocks {
            if !ids.insert(block.id) {
                return Err(format!("Duplicate block ID: {}", block.id.0));
            }
        }

        // Check that all successor blocks exist
        for block in &self.blocks {
            for succ in block.successors() {
                if self.get_block(succ).is_none() {
                    return Err(format!(
                        "Block {} references non-existent successor block {}",
                        block.id.0, succ.0
                    ));
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Function signature
        if self.is_external {
            write!(f, "extern ")?;
        }
        if self.is_exported {
            write!(f, "export ")?;
        }
        if self.is_async {
            write!(f, "async ")?;
        }

        write!(f, "fn {}(", self.name)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, ") -> {}", self.return_ty)?;

        // Effects
        if !self.effects.is_empty() {
            write!(f, " [")?;
            for (i, effect) in self.effects.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", effect)?;
            }
            write!(f, "]")?;
        }

        // Body
        if self.is_external {
            writeln!(f, ";")
        } else {
            writeln!(f, " {{")?;
            for block in &self.blocks {
                write!(f, "{}", block)?;
            }
            writeln!(f, "}}")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::ConstantValue;
    use crate::values::ValueId;

    #[test]
    fn test_basic_block_creation() {
        let mut block = BasicBlock::new(BlockId::new(0));
        block.name = Some("entry".to_string());

        assert_eq!(block.id.0, 0);
        assert_eq!(block.name, Some("entry".to_string()));
        assert!(!block.is_terminated());
    }

    #[test]
    fn test_basic_block_instructions() {
        let mut block = BasicBlock::new(BlockId::new(0));

        let result = ValueId::new(0);
        let inst = Instruction::Const {
            result,
            value: ConstantValue::Int(42, Ty::I32),
        };
        block.add_instruction(inst);

        assert_eq!(block.instructions.len(), 1);
        assert_eq!(
            block.find_instruction(result).unwrap().result(),
            Some(result)
        );
    }

    #[test]
    fn test_basic_block_terminator() {
        let mut block = BasicBlock::new(BlockId::new(0));
        block.set_terminator(Terminator::Return(None));

        assert!(block.is_terminated());
        assert_eq!(block.successors(), vec![]);
    }

    #[test]
    fn test_function_creation() {
        let func = Function::new(
            "test",
            vec![Param::new("x", Ty::I32, ValueId::new(0))],
            Ty::I32,
        );

        assert_eq!(func.name, "test");
        assert_eq!(func.params.len(), 1);
        assert_eq!(func.return_ty, Ty::I32);
        assert!(!func.is_async);
    }

    #[test]
    fn test_function_effects() {
        let func = Function::new("test", vec![], Ty::Void)
            .set_async()
            .add_effect(Effect::IO);

        assert!(func.is_async);
        assert!(func.has_effect(&Effect::Async));
        assert!(func.has_effect(&Effect::IO));
    }

    #[test]
    fn test_function_validation() {
        let mut func = Function::new("test", vec![], Ty::Void);

        // Add an unterminated block - should fail validation
        func.add_block(BasicBlock::new(BlockId::new(0)));
        assert!(func.validate().is_err());

        // Terminate the block - should pass validation
        func.get_block_mut(BlockId::new(0))
            .unwrap()
            .set_terminator(Terminator::Return(None));
        assert!(func.validate().is_ok());
    }

    #[test]
    fn test_function_validation_missing_successor() {
        let mut func = Function::new("test", vec![], Ty::Void);
        let mut block = BasicBlock::new(BlockId::new(0));
        block.set_terminator(Terminator::Branch(BlockId::new(1)));
        func.add_block(block);

        // Block references non-existent successor
        assert!(func.validate().is_err());
    }

    #[test]
    fn test_function_external() {
        let func = Function::external(
            "external_func",
            vec![Param::new("x", Ty::I32, ValueId::new(0))],
            Ty::I32,
        );

        assert!(func.is_external);
        assert!(func.blocks.is_empty());
    }
}
