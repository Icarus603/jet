//! Jet Language Server Protocol (LSP) Implementation
//!
//! This module provides IDE features for the Jet programming language including:
//! - Code completion
//! - Hover information
//! - Go to definition
//! - Find references
//! - Document symbols
//! - Diagnostic publishing
//! - Incremental text synchronization

#![allow(clippy::collapsible_match)]
#![allow(clippy::single_match)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::explicit_auto_deref)]

use std::collections::HashMap;
use std::sync::Arc;

use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::request::{
    GotoImplementationParams, GotoImplementationResponse, GotoTypeDefinitionParams,
    GotoTypeDefinitionResponse,
};
use tower_lsp::lsp_types::{SymbolInformation, *};
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod code_actions;
mod completion;
mod document;
mod folding_range;
mod format;
mod handlers;
mod inlay_hints;
mod selection_range;
mod semantic_tokens;
mod signature_help;
mod workspace_symbol;

use code_actions::{get_code_actions, get_refactoring_actions, get_source_actions};
use completion::{get_completions, get_trigger_completions};
use document::Document;
use folding_range::handle_folding_range;
use format::{format_document, format_on_type, format_range};
use handlers::{
    find_definition, find_implementation, find_references, find_type_definition,
    get_document_symbols, get_hover_info,
};
use inlay_hints::get_inlay_hints;
use selection_range::get_selection_ranges;
use semantic_tokens::{
    compute_semantic_tokens, compute_semantic_tokens_range, get_semantic_token_modifiers,
    get_semantic_token_types,
};
use signature_help::get_signature_help;
use workspace_symbol::search_workspace_symbols;

/// The backend state for the LSP server
#[derive(Debug)]
struct Backend {
    /// LSP client for sending notifications
    client: Client,
    /// Map of URI to document state
    documents: Arc<RwLock<HashMap<Url, Document>>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    /// Initialize the server and advertise capabilities
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // Text document synchronization with incremental updates
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        will_save: Some(false),
                        will_save_wait_until: Some(false),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(false),
                        })),
                    },
                )),
                // Completion support
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                // Hover information
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                // Go to definition
                definition_provider: Some(OneOf::Left(true)),
                // Find references
                references_provider: Some(OneOf::Left(true)),
                // Document symbols (outline view)
                document_symbol_provider: Some(OneOf::Left(true)),
                // Workspace symbols
                workspace_symbol_provider: Some(OneOf::Left(true)),
                // Code actions
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                // Inlay hints
                inlay_hint_provider: Some(OneOf::Left(true)),
                // Document formatting
                document_formatting_provider: Some(OneOf::Left(true)),
                // Document range formatting
                document_range_formatting_provider: Some(OneOf::Left(true)),
                // Document on-type formatting
                document_on_type_formatting_provider: Some(DocumentOnTypeFormattingOptions {
                    first_trigger_character: ";".to_string(),
                    more_trigger_character: Some(vec!["}".to_string(), ")".to_string()]),
                }),
                // Selection range
                selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
                // Semantic tokens
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions::default(),
                            legend: SemanticTokensLegend {
                                token_types: get_semantic_token_types(),
                                token_modifiers: get_semantic_token_modifiers(),
                            },
                            range: Some(true),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                // Signature help
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                // Folding range
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                // Type definition (go to type)
                type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
                // Implementation provider
                implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
                // Prepare rename for better rename support
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                })),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "Jet LSP".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    /// Server has been initialized
    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Jet LSP server initialized")
            .await;
    }

    /// Server is shutting down
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    /// Document was opened
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let version = params.text_document.version;

        let mut doc = Document::new(uri.clone(), version, text);
        doc.parse();

        let diagnostics = doc.diagnostics.clone();

        {
            let mut docs = self.documents.write().await;
            docs.insert(uri.clone(), doc);
        }

        // Publish diagnostics
        self.client
            .publish_diagnostics(uri, diagnostics, Some(version))
            .await;
    }

    /// Document was changed
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        let mut docs = self.documents.write().await;

        if let Some(doc) = docs.get_mut(&uri) {
            // Apply incremental changes using ropey
            for change in params.content_changes {
                doc.apply_change(&change);
            }

            doc.version = version;
            doc.parse(); // Re-parse the document

            // Publish updated diagnostics
            let diagnostics = doc.diagnostics.clone();
            drop(docs); // Release lock before async call

            self.client
                .publish_diagnostics(uri, diagnostics, Some(version))
                .await;
        }
    }

    /// Document was saved
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("Document saved: {}", params.text_document.uri),
            )
            .await;
    }

    /// Document was closed
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut docs = self.documents.write().await;
        docs.remove(&params.text_document.uri);

        // Clear diagnostics for closed document
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    /// Provide completion items
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            // Even without a valid AST, we can provide keyword completions
            let completions = completion::get_keyword_completions();
            return Ok(Some(CompletionResponse::Array(completions)));
        };

        // Check if triggered by a character
        let completions = if let Some(context) = params.context {
            if let Some(trigger_char) = context.trigger_character {
                if trigger_char.len() == 1 {
                    get_trigger_completions(ast, position, trigger_char.chars().next().unwrap())
                } else {
                    get_completions(ast, position)
                }
            } else {
                get_completions(ast, position)
            }
        } else {
            get_completions(ast, position)
        };

        Ok(Some(CompletionResponse::Array(completions)))
    }

    /// Provide hover information
    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        let Some(contents) = get_hover_info(doc, ast, position) else {
            return Ok(None);
        };

        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: contents,
            }),
            range: None,
        }))
    }

    /// Go to definition
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        let Some(location) = find_definition(doc, ast, position) else {
            return Ok(None);
        };

        Ok(Some(GotoDefinitionResponse::Scalar(location)))
    }

    /// Find references
    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        let locations = find_references(doc, ast, position);

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    /// Document symbols (outline view)
    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        let symbols = get_document_symbols(doc, ast);

        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    /// Rename symbol
    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        // Find all references to rename
        let locations = find_references(doc, ast, position);

        if locations.is_empty() {
            return Ok(None);
        }

        // Build text edits for each location
        let mut changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();

        for location in locations {
            let edit = TextEdit {
                range: location.range,
                new_text: new_name.clone(),
            };
            changes.entry(location.uri).or_default().push(edit);
        }

        Ok(Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }))
    }

    /// Code actions
    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri;
        let range = params.range;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(Some(vec![]));
        };

        let source = doc.text.to_string();
        let mut actions = Vec::new();

        // Add quick fixes for diagnostics
        for diagnostic in &params.context.diagnostics {
            actions.extend(get_code_actions(&uri, diagnostic, &source));
        }

        // Add refactoring actions
        actions.extend(get_refactoring_actions(&uri, range, &source));

        // Add source actions
        actions.extend(get_source_actions(&uri, &source));

        Ok(Some(actions))
    }

    /// Format document
    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let edits = format_document(&doc.text);
        Ok(Some(edits))
    }

    /// Format document range
    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        let range = params.range;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let edits = format_range(
            &doc.text,
            range.start.line as usize,
            range.end.line as usize,
        );
        if edits.is_empty() {
            Ok(None)
        } else {
            Ok(Some(edits))
        }
    }

    /// Format on type
    async fn on_type_formatting(
        &self,
        params: DocumentOnTypeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let ch = params.ch;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let edits = format_on_type(&doc.text, position.line, ch.chars().next().unwrap_or(';'));
        if edits.is_empty() {
            Ok(None)
        } else {
            Ok(Some(edits))
        }
    }

    /// Inlay hints
    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri;
        let range = params.range;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        let hints = get_inlay_hints(doc, ast, range);
        Ok(Some(hints))
    }

    /// Semantic tokens
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        // Tokenize the source to get lexer tokens
        let source = doc.text.to_string();
        let lexer_tokens = jet_lexer::tokenize(&source);

        // Compute semantic tokens
        let tokens = compute_semantic_tokens(&source, ast, &lexer_tokens);

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    /// Semantic tokens range
    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri;
        let range = params.range;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        // Tokenize the source to get lexer tokens
        let source = doc.text.to_string();
        let lexer_tokens = jet_lexer::tokenize(&source);

        // Compute semantic tokens for the specified range
        let tokens = compute_semantic_tokens_range(
            &source,
            ast,
            &lexer_tokens,
            range.start.line,
            range.end.line,
        );

        Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    /// Signature help
    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        let signature_help = get_signature_help(doc, ast, position);
        Ok(signature_help)
    }

    /// Selection ranges
    async fn selection_range(
        &self,
        params: SelectionRangeParams,
    ) -> Result<Option<Vec<SelectionRange>>> {
        let uri = params.text_document.uri;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        let selection_ranges = get_selection_ranges(doc, ast, params.positions);
        Ok(Some(selection_ranges))
    }

    /// Workspace symbols
    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let symbols = search_workspace_symbols(&self.documents, params).await;
        Ok(Some(symbols))
    }

    /// Folding range
    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = params.text_document.uri.clone();

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        let ranges = handle_folding_range(doc, ast, params);
        Ok(Some(ranges))
    }

    /// Go to type definition
    async fn goto_type_definition(
        &self,
        params: GotoTypeDefinitionParams,
    ) -> Result<Option<GotoTypeDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        let Some(location) = find_type_definition(doc, ast, position) else {
            return Ok(None);
        };

        Ok(Some(GotoTypeDefinitionResponse::Scalar(location)))
    }

    /// Go to implementation
    async fn goto_implementation(
        &self,
        params: GotoImplementationParams,
    ) -> Result<Option<GotoImplementationResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        let locations = find_implementation(doc, ast, position);

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(GotoImplementationResponse::Array(locations)))
        }
    }

    /// Prepare rename - provides the range that can be renamed
    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(ref ast) = doc.ast else {
            return Ok(None);
        };

        // Find the identifier at the position
        let Some(range) = handlers::get_identifier_range_at_position(doc, ast, position) else {
            return Ok(None);
        };

        Ok(Some(PrepareRenameResponse::Range(range)))
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        documents: Arc::new(RwLock::new(HashMap::new())),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::LspService;

    #[tokio::test]
    async fn test_server_capabilities() {
        let (service, _) = LspService::build(|client| Backend {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        })
        .finish();

        // Test initialize
        let init = InitializeParams::default();
        let result = service.inner().initialize(init).await.unwrap();

        // Verify capabilities
        assert!(result.capabilities.completion_provider.is_some());
        assert!(result.capabilities.hover_provider.is_some());
        assert!(result.capabilities.definition_provider.is_some());
        assert!(result.capabilities.references_provider.is_some());
        assert!(result.capabilities.document_symbol_provider.is_some());
        assert!(result.capabilities.rename_provider.is_some());
        assert!(result.capabilities.code_action_provider.is_some());
        assert!(result.capabilities.inlay_hint_provider.is_some());
        assert!(result.capabilities.document_formatting_provider.is_some());

        // Check completion trigger characters
        let completion = result.capabilities.completion_provider.unwrap();
        assert_eq!(
            completion.trigger_characters,
            Some(vec![".".to_string(), ":".to_string()])
        );
    }

    #[tokio::test]
    async fn test_document_lifecycle() {
        let (service, _) = LspService::build(|client| Backend {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        })
        .finish();

        let backend = service.inner();

        // Open a document
        let uri = Url::parse("file:///test.jet").unwrap();
        let open_params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "jet".to_string(),
                version: 1,
                text: "fn main() {}".to_string(),
            },
        };

        backend.did_open(open_params).await;

        // Verify document was stored
        let docs = backend.documents.read().await;
        assert!(docs.contains_key(&uri));
        drop(docs);

        // Change the document
        let change_params = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: 2,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "fn main() {\n    println(\"Hello\")\n}".to_string(),
            }],
        };

        backend.did_change(change_params).await;

        // Close the document
        let close_params = DidCloseTextDocumentParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
        };

        backend.did_close(close_params).await;

        // Verify document was removed
        let docs = backend.documents.read().await;
        assert!(!docs.contains_key(&uri));
    }
}
