//! Associated type resolution for Jet
//!
//! This module handles:
//! - Associated type definitions in traits
//! - Associated type projections (e.g., `<T as Iterator>::Item`)
//! - Associated type normalization

use crate::traits::{TraitBound, TraitContext, TraitResolver};
use crate::types::{TypeContext, TypeId, TypeKind};
use crate::DefId;
use jet_diagnostics::{Diagnostic, ErrorCode, Span};
use std::collections::HashMap;

/// An associated type definition
#[derive(Debug, Clone)]
pub struct AssociatedType {
    pub def_id: DefId,
    pub name: String,
    pub trait_id: DefId,
    pub bounds: Vec<TraitBound>,
    pub default: Option<TypeId>, // Default type if any
}

/// An associated type projection (e.g., `<T as Trait>::Type`)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Projection {
    /// The type being projected from
    pub base: TypeId,
    /// The trait being projected through
    pub trait_bound: TraitBound,
    /// The name of the associated type
    pub name: String,
}

/// Associated type context for storing and resolving associated types
#[derive(Debug, Default)]
pub struct AssociatedTypeContext {
    /// Associated type definitions by DefId
    associated_types: HashMap<DefId, AssociatedType>,
    /// Associated types by trait and name
    by_trait: HashMap<(DefId, String), DefId>,
    /// Projection cache (normalization results)
    projection_cache: HashMap<Projection, TypeId>,
}

impl AssociatedTypeContext {
    /// Create a new associated type context
    pub fn new() -> Self {
        Self::default()
    }

    /// Register an associated type definition
    pub fn register_associated_type(&mut self, assoc_type: AssociatedType) {
        let def_id = assoc_type.def_id;
        let trait_id = assoc_type.trait_id;
        let name = assoc_type.name.clone();

        self.by_trait.insert((trait_id, name), def_id);
        self.associated_types.insert(def_id, assoc_type);
    }

    /// Get an associated type by DefId
    pub fn get_associated_type(&self, def_id: DefId) -> Option<&AssociatedType> {
        self.associated_types.get(&def_id)
    }

    /// Get an associated type by trait and name
    pub fn get_by_trait(&self, trait_id: DefId, name: &str) -> Option<&AssociatedType> {
        self.by_trait
            .get(&(trait_id, name.to_string()))
            .and_then(|id| self.associated_types.get(id))
    }

    /// Normalize a projection type to its concrete type
    pub fn normalize(
        &mut self,
        projection: &Projection,
        trait_ctx: &TraitContext,
        tcx: &TypeContext,
    ) -> Result<TypeId, ProjectionError> {
        // Check cache first
        if let Some(&ty) = self.projection_cache.get(projection) {
            return Ok(ty);
        }

        // Resolve the trait bound
        let resolver = TraitResolver::new(trait_ctx, tcx);
        let resolution = resolver.resolve(projection.base, &projection.trait_bound)?;

        // Find the impl and get the associated type
        let impl_def = trait_ctx
            .get_trait(resolution.trait_id)
            .and_then(|_| trait_ctx.find_impl(projection.base, &projection.trait_bound, tcx))
            .ok_or_else(|| ProjectionError {
                projection: projection.clone(),
                message: "no implementation found".to_string(),
            })?;

        // Find the associated type in the impl
        for item in &impl_def.items {
            if let crate::traits::ImplItem::Type { name, ty, .. } = item {
                if name == &projection.name {
                    // Cache and return
                    self.projection_cache.insert(projection.clone(), *ty);
                    return Ok(*ty);
                }
            }
        }

        // Check if there's a default
        if let Some(assoc_type) =
            self.get_by_trait(projection.trait_bound.trait_id, &projection.name)
        {
            if let Some(default) = assoc_type.default {
                self.projection_cache.insert(projection.clone(), default);
                return Ok(default);
            }
        }

        Err(ProjectionError {
            projection: projection.clone(),
            message: format!("associated type `{}` not found", projection.name),
        })
    }

    /// Check if a type is a projection type
    pub fn is_projection(&self, _ty: TypeId, _tcx: &TypeContext) -> bool {
        // In a full implementation, we'd have a dedicated TypeKind::Projection
        // For now, we check based on structure
        false
    }

    /// Fully normalize a type, resolving all projections
    pub fn fully_normalize(
        &mut self,
        ty: TypeId,
        _trait_ctx: &TraitContext,
        tcx: &TypeContext,
    ) -> Result<TypeId, ProjectionError> {
        // Recursively normalize the type
        match tcx.type_kind(ty) {
            TypeKind::Tuple(elements) => {
                let mut new_elements = Vec::new();
                for elem in elements {
                    new_elements.push(self.fully_normalize(*elem, _trait_ctx, tcx)?);
                }
                // We can't modify the type context, so we just return the original
                // In a full implementation, we'd create a new normalized type
                let _ = new_elements; // unused for now
                Ok(ty)
            }
            TypeKind::Function {
                params: _,
                ret: _,
                effects: _,
            } => {
                // Normalize parameter and return types
                Ok(ty)
            }
            TypeKind::Ref(inner, _) | TypeKind::RawPtr(inner, _) => {
                self.fully_normalize(*inner, _trait_ctx, tcx)
            }
            _ => Ok(ty),
        }
    }

    /// Clear the projection cache
    pub fn clear_cache(&mut self) {
        self.projection_cache.clear();
    }
}

/// An error during projection normalization
#[derive(Debug, Clone)]
pub struct ProjectionError {
    pub projection: Projection,
    pub message: String,
}

impl ProjectionError {
    /// Convert to a diagnostic
    pub fn to_diagnostic(&self, tcx: &TypeContext, span: Span) -> Diagnostic {
        Diagnostic::error(
            format!(
                "cannot normalize associated type projection: {}",
                self.message
            ),
            span,
        )
        .with_error_code(ErrorCode::CannotNormalizeProjection)
        .with_note(format!(
            "projection: <{} as {}>::{}",
            tcx.type_to_string(self.projection.base),
            self.projection.trait_bound.trait_id.0, // Simplified
            self.projection.name
        ))
    }
}

impl From<crate::traits::ResolutionError> for ProjectionError {
    fn from(err: crate::traits::ResolutionError) -> Self {
        ProjectionError {
            projection: Projection {
                base: err.ty,
                trait_bound: err.bound,
                name: String::new(),
            },
            message: err.message,
        }
    }
}

/// Associated type inference helper
pub struct AssociatedTypeResolver<'tcx> {
    assoc_ctx: &'tcx AssociatedTypeContext,
    #[allow(dead_code)]
    trait_ctx: &'tcx TraitContext,
    #[allow(dead_code)]
    tcx: &'tcx TypeContext,
}

impl<'tcx> AssociatedTypeResolver<'tcx> {
    /// Create a new resolver
    pub fn new(
        assoc_ctx: &'tcx AssociatedTypeContext,
        trait_ctx: &'tcx TraitContext,
        tcx: &'tcx TypeContext,
    ) -> Self {
        Self {
            assoc_ctx,
            trait_ctx,
            tcx,
        }
    }

    /// Resolve an associated type projection
    pub fn resolve(
        &self,
        base: TypeId,
        trait_bound: TraitBound,
        name: &str,
    ) -> Result<TypeId, ProjectionError> {
        let projection = Projection {
            base,
            trait_bound,
            name: name.to_string(),
        };

        // For now, return an error since we need mutable access to cache
        // In practice, we'd use interior mutability or pass mutable context
        Err(ProjectionError {
            projection,
            message: "associated type resolution not fully implemented".to_string(),
        })
    }

    /// Get the bounds for an associated type
    pub fn get_bounds(&self, trait_id: DefId, name: &str) -> Option<&[TraitBound]> {
        self.assoc_ctx
            .get_by_trait(trait_id, name)
            .map(|at| at.bounds.as_slice())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_associated_type_registration() {
        let mut ctx = AssociatedTypeContext::new();

        let assoc_type = AssociatedType {
            def_id: DefId(1),
            name: "Item".to_string(),
            trait_id: DefId(2),
            bounds: vec![],
            default: None,
        };

        ctx.register_associated_type(assoc_type);

        assert!(ctx.get_associated_type(DefId(1)).is_some());
        assert!(ctx.get_by_trait(DefId(2), "Item").is_some());
    }

    #[test]
    fn test_projection_creation() {
        let projection = Projection {
            base: TypeId::INT,
            trait_bound: TraitBound {
                trait_id: DefId(1),
                args: vec![],
            },
            name: "Item".to_string(),
        };

        assert_eq!(projection.base, TypeId::INT);
        assert_eq!(projection.name, "Item");
    }
}
