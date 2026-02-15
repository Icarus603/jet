//! Memory layout calculation for C types

pub use crate::types::{Alignment, CType, CTypeKind, Size, StructField, StructLayout};

/// Errors in layout calculation
#[derive(Debug, Clone)]
pub enum LayoutError {
    InvalidType(String),
    Overflow,
}

impl std::fmt::Display for LayoutError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidType(t) => write!(f, "invalid type: {}", t),
            Self::Overflow => write!(f, "size overflow"),
        }
    }
}

impl std::error::Error for LayoutError {}

pub type LayoutResult<T> = Result<T, LayoutError>;

/// Layout calculator for C types
#[derive(Debug, Default)]
pub struct LayoutCalculator;

impl LayoutCalculator {
    pub fn new() -> Self {
        Self
    }

    pub fn calculate(&self, ty: &CType) -> LayoutResult<CLayout> {
        Ok(CLayout {
            size: ty.size(),
            alignment: ty.alignment(),
        })
    }
}

/// Computed layout for a type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CLayout {
    pub size: usize,
    pub alignment: usize,
}

/// Field offset information
#[derive(Debug, Clone)]
pub struct FieldOffset {
    pub name: String,
    pub offset: usize,
    pub ty: CType,
}

/// Calculate struct layout
pub fn calculate_struct_layout(fields: &[StructField]) -> LayoutResult<StructLayout> {
    let mut offset = 0;
    let mut max_align = 1;

    for field in fields {
        let align = field.alignment();
        let size = field.size();
        offset = crate::align_up(offset, align);
        offset += size;
        if align > max_align {
            max_align = align;
        }
    }

    let total_size = crate::align_up(offset, max_align);
    let tail_padding = total_size - offset;

    Ok(StructLayout {
        size: total_size,
        alignment: max_align,
        tail_padding,
    })
}

/// Calculate union layout
pub fn calculate_union_layout(fields: &[StructField]) -> LayoutResult<StructLayout> {
    let mut max_size = 0;
    let mut max_align = 1;

    for field in fields {
        let size = field.size();
        let align = field.alignment();
        if size > max_size {
            max_size = size;
        }
        if align > max_align {
            max_align = align;
        }
    }

    let total_size = crate::align_up(max_size, max_align);
    let tail_padding = total_size - max_size;

    Ok(StructLayout {
        size: total_size,
        alignment: max_align,
        tail_padding,
    })
}
