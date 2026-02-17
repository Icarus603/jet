//! Workspace symbols for the Jet LSP server
//!
//! Provides cross-file symbol indexing and search capabilities.

use crate::document::Document;
use jet_parser::ast::{Module, ModuleItem};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::{Location, SymbolInformation, SymbolKind, Url, WorkspaceSymbolParams};

/// Workspace symbol index
#[derive(Debug, Default)]
#[allow(dead_code)]
pub struct WorkspaceSymbolIndex {
    /// Map of symbol names to their locations
    symbols: HashMap<String, Vec<SymbolInfo>>,
    /// Map of URIs to their document symbols (for incremental updates)
    document_symbols: HashMap<Url, Vec<SymbolInfo>>,
}

/// Information about a symbol
#[derive(Debug, Clone)]
struct SymbolInfo {
    name: String,
    kind: SymbolKind,
    location: Location,
    container_name: Option<String>,
}

#[allow(deprecated, dead_code)]
impl WorkspaceSymbolIndex {
    /// Create a new empty symbol index
    pub fn new() -> Self {
        Self::default()
    }

    /// Index a document's symbols
    pub fn index_document(&mut self, doc: &Document, ast: &Module) {
        let uri = doc.uri.clone();
        let symbols = extract_symbols_from_module(doc, ast);

        // Remove old symbols for this document
        self.remove_document(&uri);

        // Add new symbols
        for symbol in &symbols {
            self.symbols
                .entry(symbol.name.clone())
                .or_default()
                .push(symbol.clone());
        }

        self.document_symbols.insert(uri, symbols);
    }

    /// Remove a document from the index
    pub fn remove_document(&mut self, uri: &Url) {
        if let Some(old_symbols) = self.document_symbols.remove(uri) {
            for symbol in old_symbols {
                if let Some(entries) = self.symbols.get_mut(&symbol.name) {
                    entries.retain(|s| s.location.uri != *uri);
                    if entries.is_empty() {
                        self.symbols.remove(&symbol.name);
                    }
                }
            }
        }
    }

    /// Search for symbols matching a query
    pub fn search(&self, query: &str) -> Vec<SymbolInformation> {
        let query_lower = query.to_lowercase();
        let mut results = Vec::new();

        for (name, symbols) in &self.symbols {
            // Check if symbol name matches query
            let match_score = calculate_match_score(name, &query_lower);

            if match_score > 0 {
                for symbol in symbols {
                    results.push((match_score, symbol));
                }
            }
        }

        // Sort by match score (higher is better), then by name
        results.sort_by(|a, b| {
            b.0.cmp(&a.0)
                .then_with(|| a.1.name.to_lowercase().cmp(&b.1.name.to_lowercase()))
        });

        // Convert to SymbolInformation, limiting results
        results
            .into_iter()
            .take(100)
            .map(|(_, info)| SymbolInformation {
                name: info.name.clone(),
                kind: info.kind,
                tags: None,
                deprecated: None,
                location: info.location.clone(),
                container_name: info.container_name.clone(),
            })
            .collect()
    }

    /// Search for workspace symbols (newer API)
    pub fn search_workspace_symbols(&self, query: &str) -> Vec<SymbolInformation> {
        // For now, return SymbolInformation directly
        self.search(query)
    }

    /// Get all symbols in the workspace
    pub fn get_all_symbols(&self) -> Vec<SymbolInformation> {
        self.symbols
            .values()
            .flatten()
            .map(|info| SymbolInformation {
                name: info.name.clone(),
                kind: info.kind,
                tags: None,
                deprecated: None,
                location: info.location.clone(),
                container_name: info.container_name.clone(),
            })
            .collect()
    }
}

/// Calculate a match score for a symbol name against a query
/// Higher scores indicate better matches
fn calculate_match_score(name: &str, query_lower: &str) -> u32 {
    let name_lower = name.to_lowercase();

    // Exact match
    if name == query_lower {
        return 100;
    }

    // Case-insensitive exact match
    if name_lower == query_lower {
        return 90;
    }

    // Starts with query (case-sensitive)
    if name.starts_with(query_lower) {
        return 80;
    }

    // Starts with query (case-insensitive)
    if name_lower.starts_with(query_lower) {
        return 70;
    }

    // Contains query (case-sensitive)
    if name.contains(query_lower) {
        return 60;
    }

    // Contains query (case-insensitive)
    if name_lower.contains(query_lower) {
        return 50;
    }

    // Fuzzy match - all characters in query appear in order
    if fuzzy_match(&name_lower, query_lower) {
        return 30;
    }

    0
}

/// Check if all characters in query appear in name in order
fn fuzzy_match(name: &str, query: &str) -> bool {
    let mut query_chars = query.chars();
    let mut current_query = query_chars.next();

    for name_char in name.chars() {
        if let Some(qc) = current_query {
            if name_char == qc {
                current_query = query_chars.next();
            }
        } else {
            break;
        }
    }

    current_query.is_none()
}

/// Extract all symbols from a module
fn extract_symbols_from_module(doc: &Document, ast: &Module) -> Vec<SymbolInfo> {
    let mut symbols = Vec::new();

    for item in &ast.items {
        symbols.extend(extract_symbols_from_item(doc, item, None));
    }

    symbols
}

/// Extract symbols from a module item
fn extract_symbols_from_item(
    doc: &Document,
    item: &ModuleItem,
    container_name: Option<String>,
) -> Vec<SymbolInfo> {
    let mut symbols = Vec::new();

    match item {
        ModuleItem::Function(func) => {
            symbols.push(SymbolInfo {
                name: func.name.name.clone(),
                kind: SymbolKind::FUNCTION,
                location: Location {
                    uri: doc.uri.clone(),
                    range: doc.span_to_range(func.name.span),
                },
                container_name: container_name.clone(),
            });
        }
        ModuleItem::Struct(struct_def) => {
            let struct_name = struct_def.name.name.clone();

            symbols.push(SymbolInfo {
                name: struct_name.clone(),
                kind: SymbolKind::STRUCT,
                location: Location {
                    uri: doc.uri.clone(),
                    range: doc.span_to_range(struct_def.name.span),
                },
                container_name: container_name.clone(),
            });

            // Add field symbols
            for field in &struct_def.fields {
                symbols.push(SymbolInfo {
                    name: field.name.name.clone(),
                    kind: SymbolKind::FIELD,
                    location: Location {
                        uri: doc.uri.clone(),
                        range: doc.span_to_range(field.name.span),
                    },
                    container_name: Some(struct_name.clone()),
                });
            }
        }
        ModuleItem::Enum(enum_def) => {
            let enum_name = enum_def.name.name.clone();

            symbols.push(SymbolInfo {
                name: enum_name.clone(),
                kind: SymbolKind::ENUM,
                location: Location {
                    uri: doc.uri.clone(),
                    range: doc.span_to_range(enum_def.name.span),
                },
                container_name: container_name.clone(),
            });

            // Add variant symbols
            for variant in &enum_def.variants {
                symbols.push(SymbolInfo {
                    name: variant.name.name.clone(),
                    kind: SymbolKind::ENUM_MEMBER,
                    location: Location {
                        uri: doc.uri.clone(),
                        range: doc.span_to_range(variant.name.span),
                    },
                    container_name: Some(enum_name.clone()),
                });
            }
        }
        ModuleItem::Trait(trait_def) => {
            let trait_name = trait_def.name.name.clone();

            symbols.push(SymbolInfo {
                name: trait_name.clone(),
                kind: SymbolKind::INTERFACE,
                location: Location {
                    uri: doc.uri.clone(),
                    range: doc.span_to_range(trait_def.name.span),
                },
                container_name: container_name.clone(),
            });

            // Add trait item symbols
            for trait_item in &trait_def.items {
                match trait_item {
                    jet_parser::ast::TraitItem::Method { name, .. } => {
                        symbols.push(SymbolInfo {
                            name: name.name.clone(),
                            kind: SymbolKind::METHOD,
                            location: Location {
                                uri: doc.uri.clone(),
                                range: doc.span_to_range(name.span),
                            },
                            container_name: Some(trait_name.clone()),
                        });
                    }
                    jet_parser::ast::TraitItem::TypeDecl { name, .. } => {
                        symbols.push(SymbolInfo {
                            name: name.name.clone(),
                            kind: SymbolKind::TYPE_PARAMETER,
                            location: Location {
                                uri: doc.uri.clone(),
                                range: doc.span_to_range(name.span),
                            },
                            container_name: Some(trait_name.clone()),
                        });
                    }
                    jet_parser::ast::TraitItem::ConstDecl { name, .. } => {
                        symbols.push(SymbolInfo {
                            name: name.name.clone(),
                            kind: SymbolKind::CONSTANT,
                            location: Location {
                                uri: doc.uri.clone(),
                                range: doc.span_to_range(name.span),
                            },
                            container_name: Some(trait_name.clone()),
                        });
                    }
                }
            }
        }
        ModuleItem::Const(const_def) => {
            symbols.push(SymbolInfo {
                name: const_def.name.name.clone(),
                kind: SymbolKind::CONSTANT,
                location: Location {
                    uri: doc.uri.clone(),
                    range: doc.span_to_range(const_def.name.span),
                },
                container_name: container_name.clone(),
            });
        }
        ModuleItem::TypeAlias(type_alias) => {
            symbols.push(SymbolInfo {
                name: type_alias.name.name.clone(),
                kind: SymbolKind::TYPE_PARAMETER,
                location: Location {
                    uri: doc.uri.clone(),
                    range: doc.span_to_range(type_alias.name.span),
                },
                container_name: container_name.clone(),
            });
        }
        ModuleItem::Effect(effect_def) => {
            symbols.push(SymbolInfo {
                name: effect_def.name.name.clone(),
                kind: SymbolKind::EVENT,
                location: Location {
                    uri: doc.uri.clone(),
                    range: doc.span_to_range(effect_def.name.span),
                },
                container_name: container_name.clone(),
            });
        }
        ModuleItem::Impl(impl_def) => {
            // Add symbols from impl block
            for item in &impl_def.items {
                match item {
                    jet_parser::ast::ImplItem::Method(func) => {
                        symbols.push(SymbolInfo {
                            name: func.name.name.clone(),
                            kind: SymbolKind::METHOD,
                            location: Location {
                                uri: doc.uri.clone(),
                                range: doc.span_to_range(func.name.span),
                            },
                            container_name: container_name.clone(),
                        });
                    }
                    jet_parser::ast::ImplItem::TypeAlias(type_alias) => {
                        symbols.push(SymbolInfo {
                            name: type_alias.name.name.clone(),
                            kind: SymbolKind::TYPE_PARAMETER,
                            location: Location {
                                uri: doc.uri.clone(),
                                range: doc.span_to_range(type_alias.name.span),
                            },
                            container_name: container_name.clone(),
                        });
                    }
                    jet_parser::ast::ImplItem::Const { name, .. } => {
                        symbols.push(SymbolInfo {
                            name: name.name.clone(),
                            kind: SymbolKind::CONSTANT,
                            location: Location {
                                uri: doc.uri.clone(),
                                range: doc.span_to_range(name.span),
                            },
                            container_name: container_name.clone(),
                        });
                    }
                }
            }
        }
        ModuleItem::Import(import) => {
            // Add imported module as a symbol
            let path = match import {
                jet_parser::ast::Import::Simple { path, .. } => path,
                jet_parser::ast::Import::From { path, .. } => path,
            };
            if let Some(first_segment) = path.segments.first() {
                symbols.push(SymbolInfo {
                    name: first_segment.name.clone(),
                    kind: SymbolKind::MODULE,
                    location: Location {
                        uri: doc.uri.clone(),
                        range: doc.span_to_range(first_segment.span),
                    },
                    container_name: container_name.clone(),
                });
            }
        }
    }

    symbols
}

/// Search workspace symbols from documents
pub async fn search_workspace_symbols(
    documents: &Arc<RwLock<HashMap<Url, Document>>>,
    params: WorkspaceSymbolParams,
) -> Vec<SymbolInformation> {
    let query = params.query.to_lowercase();

    if query.is_empty() {
        return Vec::new();
    }

    let docs = documents.read().await;
    let mut results = Vec::new();

    for doc in docs.values() {
        if let Some(ref ast) = doc.ast {
            let symbols = extract_symbols_from_module(doc, ast);

            for symbol in symbols {
                let match_score = calculate_match_score(&symbol.name, &query);

                if match_score > 0 {
                    results.push((match_score, symbol));
                }
            }
        }
    }

    // Sort by match score
    results.sort_by(|a, b| {
        b.0.cmp(&a.0)
            .then_with(|| a.1.name.to_lowercase().cmp(&b.1.name.to_lowercase()))
    });

    // Convert to SymbolInformation
    #[allow(deprecated)]
    results
        .into_iter()
        .take(100)
        .map(|(_, info)| SymbolInformation {
            name: info.name,
            kind: info.kind,
            tags: None,
            deprecated: None,
            location: info.location,
            container_name: info.container_name,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuzzy_match() {
        assert!(fuzzy_match("hello", "hl"));
        assert!(fuzzy_match("hello", "helo"));
        assert!(fuzzy_match("hello", "hello"));
        assert!(!fuzzy_match("hello", "xyz"));
        assert!(!fuzzy_match("hello", "hxlloz"));
    }

    #[test]
    fn test_calculate_match_score() {
        assert_eq!(calculate_match_score("hello", "hello"), 100);
        assert_eq!(calculate_match_score("Hello", "hello"), 90);
        assert!(calculate_match_score("hello", "hel") > 0);
        assert!(calculate_match_score("hello", "xyz") == 0);
    }
}
