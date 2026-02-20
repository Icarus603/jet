//! HTML documentation generation
//!
//! This module generates beautiful HTML documentation from extracted
//! documentation data, including search functionality and cross-linking.

use crate::{AiAnnotations, DocConfig, DocDatabase, DocItem, DocItemKind};
use anyhow::{Context, Result};
use comrak::{markdown_to_html, ComrakOptions};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Generate HTML documentation for a database
pub async fn generate_html_docs(database: &DocDatabase) -> Result<()> {
    let output_dir = &database.config.output_dir;

    // Create output directory
    tokio::fs::create_dir_all(output_dir)
        .await
        .with_context(|| {
            format!(
                "Failed to create output directory: {}",
                output_dir.display()
            )
        })?;

    // Generate CSS
    let css = generate_css();
    tokio::fs::write(output_dir.join("style.css"), css)
        .await
        .context("Failed to write CSS file")?;

    // Generate JavaScript for search
    let js = generate_javascript(database);
    tokio::fs::write(output_dir.join("search.js"), js)
        .await
        .context("Failed to write JavaScript file")?;

    // Generate index page
    let index_html = generate_index_page(database);
    tokio::fs::write(output_dir.join("index.html"), index_html)
        .await
        .context("Failed to write index.html")?;

    // Generate all items page
    let all_items_html = generate_all_items_page(database);
    tokio::fs::write(output_dir.join("all.html"), all_items_html)
        .await
        .context("Failed to write all.html")?;

    // Generate search index
    let search_index = generate_search_index(database);
    tokio::fs::write(output_dir.join("search-index.json"), search_index)
        .await
        .context("Failed to write search index")?;

    // Generate individual item pages
    for item in &database.items {
        let item_html = generate_item_page(database, item);
        let file_name = format!("{}.{}.html", item.kind.as_str(), item.name);
        tokio::fs::write(output_dir.join(&file_name), item_html)
            .await
            .with_context(|| format!("Failed to write {}", file_name))?;
    }

    // Generate module pages
    let modules = collect_modules(database);
    for module_path in modules {
        let module_html = generate_module_page(database, &module_path);
        let file_name = if module_path.is_empty() {
            "root.html".to_string()
        } else {
            format!("module.{}.html", module_path.join("."))
        };
        tokio::fs::write(output_dir.join(&file_name), module_html)
            .await
            .with_context(|| format!("Failed to write {}", file_name))?;
    }

    Ok(())
}

/// Generate CSS styles
fn generate_css() -> &'static str {
    r#"
:root {
    --bg-color: #1e1e1e;
    --fg-color: #d4d4d4;
    --accent-color: #4ec9b0;
    --secondary-accent: #569cd6;
    --border-color: #3e3e42;
    --code-bg: #252526;
    --hover-bg: #2a2d2e;
    --ai-badge-bg: #ffd700;
    --ai-badge-fg: #1e1e1e;
    --link-color: #4ec9b0;
    --visited-link: #c586c0;
    --search-bg: #3c3c3c;
    --success-color: #4ec9b0;
    --error-color: #f44747;
    --warning-color: #dcdcaa;
}

* {
    box-sizing: border-box;
}

body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Oxygen, Ubuntu, sans-serif;
    background-color: var(--bg-color);
    color: var(--fg-color);
    line-height: 1.6;
    margin: 0;
    padding: 0;
}

.container {
    max-width: 1200px;
    margin: 0 auto;
    padding: 0 20px;
}

/* Header */
header {
    background-color: var(--code-bg);
    border-bottom: 1px solid var(--border-color);
    padding: 1rem 0;
    position: sticky;
    top: 0;
    z-index: 100;
}

header .container {
    display: flex;
    justify-content: space-between;
    align-items: center;
}

header h1 {
    margin: 0;
    font-size: 1.5rem;
    color: var(--accent-color);
}

header h1 a {
    color: inherit;
    text-decoration: none;
}

/* Search */
.search-container {
    position: relative;
    width: 300px;
}

#search-input {
    width: 100%;
    padding: 0.5rem 1rem;
    background-color: var(--search-bg);
    border: 1px solid var(--border-color);
    border-radius: 4px;
    color: var(--fg-color);
    font-size: 0.9rem;
}

#search-input:focus {
    outline: none;
    border-color: var(--accent-color);
}

#search-results {
    position: absolute;
    top: 100%;
    left: 0;
    right: 0;
    background-color: var(--code-bg);
    border: 1px solid var(--border-color);
    border-top: none;
    border-radius: 0 0 4px 4px;
    max-height: 400px;
    overflow-y: auto;
    display: none;
}

#search-results.visible {
    display: block;
}

.search-result {
    padding: 0.75rem 1rem;
    cursor: pointer;
    border-bottom: 1px solid var(--border-color);
}

.search-result:hover {
    background-color: var(--hover-bg);
}

.search-result:last-child {
    border-bottom: none;
}

.search-result-name {
    font-weight: bold;
    color: var(--accent-color);
}

.search-result-kind {
    font-size: 0.8rem;
    color: var(--secondary-accent);
    margin-left: 0.5rem;
}

.search-result-module {
    font-size: 0.8rem;
    color: #808080;
}

/* Navigation */
nav {
    background-color: var(--code-bg);
    padding: 0.5rem 0;
    border-bottom: 1px solid var(--border-color);
}

nav .container {
    display: flex;
    gap: 1rem;
}

nav a {
    color: var(--fg-color);
    text-decoration: none;
    padding: 0.25rem 0.5rem;
    border-radius: 4px;
}

nav a:hover {
    background-color: var(--hover-bg);
    color: var(--accent-color);
}

/* Main content */
main {
    padding: 2rem 0;
}

/* Item styling */
.doc-item {
    margin-bottom: 2rem;
    padding: 1.5rem;
    background-color: var(--code-bg);
    border: 1px solid var(--border-color);
    border-radius: 8px;
}

.doc-item-header {
    display: flex;
    align-items: flex-start;
    gap: 1rem;
    margin-bottom: 1rem;
}

.item-kind {
    font-size: 0.75rem;
    text-transform: uppercase;
    padding: 0.25rem 0.5rem;
    background-color: var(--secondary-accent);
    color: var(--bg-color);
    border-radius: 4px;
    font-weight: bold;
}

.item-name {
    font-size: 1.5rem;
    font-weight: bold;
    color: var(--accent-color);
    margin: 0;
}

.item-signature {
    font-family: "JetBrains Mono", "Fira Code", Consolas, monospace;
    background-color: var(--bg-color);
    padding: 1rem;
    border-radius: 4px;
    margin: 1rem 0;
    overflow-x: auto;
}

.item-signature .keyword {
    color: var(--secondary-accent);
}

.item-signature .type {
    color: var(--accent-color);
}

.item-signature .function-name {
    color: #dcdcaa;
}

/* AI Annotations */
.ai-annotations {
    background-color: var(--ai-badge-bg);
    color: var(--ai-badge-fg);
    padding: 0.5rem 1rem;
    border-radius: 4px;
    margin: 1rem 0;
    font-size: 0.85rem;
}

.ai-annotations .ai-label {
    font-weight: bold;
}

.ai-confidence {
    display: inline-flex;
    align-items: center;
    gap: 0.5rem;
}

.confidence-bar {
    width: 100px;
    height: 8px;
    background-color: rgba(0, 0, 0, 0.2);
    border-radius: 4px;
    overflow: hidden;
}

.confidence-fill {
    height: 100%;
    background-color: var(--bg-color);
    border-radius: 4px;
}

/* Doc comment */
.doc-comment {
    margin: 1rem 0;
}

.doc-comment h1,
.doc-comment h2,
.doc-comment h3 {
    color: var(--accent-color);
    margin-top: 1.5rem;
    margin-bottom: 0.75rem;
}

.doc-comment p {
    margin: 0.75rem 0;
}

.doc-comment code {
    font-family: "JetBrains Mono", "Fira Code", Consolas, monospace;
    background-color: var(--bg-color);
    padding: 0.2rem 0.4rem;
    border-radius: 3px;
    font-size: 0.9em;
}

.doc-comment pre {
    background-color: var(--bg-color);
    padding: 1rem;
    border-radius: 4px;
    overflow-x: auto;
}

.doc-comment pre code {
    background: none;
    padding: 0;
}

/* Examples */
.example {
    margin: 1rem 0;
    border: 1px solid var(--border-color);
    border-radius: 4px;
    overflow: hidden;
}

.example-header {
    background-color: var(--bg-color);
    padding: 0.5rem 1rem;
    font-weight: bold;
    color: var(--secondary-accent);
}

.example-code {
    background-color: var(--bg-color);
    padding: 1rem;
    margin: 0;
    overflow-x: auto;
}

.example-output {
    background-color: var(--code-bg);
    padding: 1rem;
    border-top: 1px solid var(--border-color);
}

.example-output-label {
    font-size: 0.8rem;
    color: #808080;
    margin-bottom: 0.5rem;
}

/* Module list */
.module-list {
    list-style: none;
    padding: 0;
}

.module-list li {
    padding: 0.5rem 0;
    border-bottom: 1px solid var(--border-color);
}

.module-list li:last-child {
    border-bottom: none;
}

.module-list a {
    color: var(--link-color);
    text-decoration: none;
}

.module-list a:hover {
    text-decoration: underline;
}

/* Item list by kind */
.item-section {
    margin: 2rem 0;
}

.item-section h2 {
    color: var(--accent-color);
    border-bottom: 2px solid var(--border-color);
    padding-bottom: 0.5rem;
}

.item-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
    gap: 1rem;
    margin-top: 1rem;
}

.item-card {
    background-color: var(--code-bg);
    border: 1px solid var(--border-color);
    border-radius: 4px;
    padding: 1rem;
}

.item-card:hover {
    border-color: var(--accent-color);
}

.item-card a {
    color: var(--accent-color);
    text-decoration: none;
    font-weight: bold;
}

.item-card .item-description {
    font-size: 0.9rem;
    color: #808080;
    margin-top: 0.5rem;
}

/* Footer */
footer {
    background-color: var(--code-bg);
    border-top: 1px solid var(--border-color);
    padding: 1rem 0;
    margin-top: 2rem;
    text-align: center;
    font-size: 0.85rem;
    color: #808080;
}

/* Breadcrumbs */
.breadcrumbs {
    margin-bottom: 1rem;
    font-size: 0.9rem;
}

.breadcrumbs a {
    color: var(--link-color);
    text-decoration: none;
}

.breadcrumbs a:hover {
    text-decoration: underline;
}

.breadcrumbs .separator {
    margin: 0 0.5rem;
    color: #808080;
}

/* Source link */
.source-link {
    float: right;
    font-size: 0.85rem;
    color: #808080;
}

.source-link a {
    color: inherit;
}

.source-link a:hover {
    color: var(--link-color);
}

/* Responsive */
@media (max-width: 768px) {
    header .container {
        flex-direction: column;
        gap: 1rem;
    }

    .search-container {
        width: 100%;
    }

    .item-grid {
        grid-template-columns: 1fr;
    }
}
"#
}

/// Generate JavaScript for search functionality
fn generate_javascript(_database: &DocDatabase) -> String {
    r#"
// Search functionality
let searchIndex = [];
let searchDebounceTimer = null;

// Load search index
async function loadSearchIndex() {
    try {
        const response = await fetch('search-index.json');
        searchIndex = await response.json();
    } catch (e) {
        console.error('Failed to load search index:', e);
    }
}

// Perform search
function performSearch(query) {
    if (!query || query.length < 2) {
        return [];
    }

    const lowerQuery = query.toLowerCase();
    const results = [];

    for (const item of searchIndex) {
        const nameScore = item.name.toLowerCase().includes(lowerQuery) ? 10 : 0;
        const descScore = item.description && item.description.toLowerCase().includes(lowerQuery) ? 5 : 0;

        if (nameScore > 0 || descScore > 0) {
            results.push({
                item,
                score: nameScore + descScore
            });
        }
    }

    // Sort by score descending
    results.sort((a, b) => b.score - a.score);

    return results.slice(0, 10).map(r => r.item);
}

// Display search results
function displaySearchResults(results) {
    const container = document.getElementById('search-results');

    if (results.length === 0) {
        container.classList.remove('visible');
        return;
    }

    container.innerHTML = results.map(item => `
        <div class="search-result" onclick="navigateTo('${item.url}')">
            <div>
                <span class="search-result-name">${escapeHtml(item.name)}</span>
                <span class="search-result-kind">${item.kind}</span>
            </div>
            <div class="search-result-module">${item.module}</div>
        </div>
    `).join('');

    container.classList.add('visible');
}

// Navigate to item page
function navigateTo(url) {
    window.location.href = url;
}

// Escape HTML
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// Initialize search
document.addEventListener('DOMContentLoaded', () => {
    loadSearchIndex();

    const searchInput = document.getElementById('search-input');
    if (searchInput) {
        searchInput.addEventListener('input', (e) => {
            clearTimeout(searchDebounceTimer);
            searchDebounceTimer = setTimeout(() => {
                const results = performSearch(e.target.value);
                displaySearchResults(results);
            }, 150);
        });

        // Hide results when clicking outside
        document.addEventListener('click', (e) => {
            if (!e.target.closest('.search-container')) {
                document.getElementById('search-results').classList.remove('visible');
            }
        });
    }
});
"#
    .to_string()
}

/// Generate search index JSON
fn generate_search_index(database: &DocDatabase) -> String {
    let items: Vec<_> = database
        .items
        .iter()
        .map(|item| {
            let module = if item.module_path.is_empty() {
                "crate".to_string()
            } else {
                item.module_path.join("::")
            };

            serde_json::json!({
                "name": item.name,
                "kind": item.kind.as_str(),
                "module": module,
                "description": item.doc_comment.as_ref().map(|d| first_sentence(d)),
                "url": format!("{}.{}.html", item.kind.as_str(), item.name),
            })
        })
        .collect();

    serde_json::to_string_pretty(&items).unwrap_or_default()
}

/// Get first sentence of text
fn first_sentence(text: &str) -> String {
    text.split('.')
        .next()
        .unwrap_or(text)
        .lines()
        .next()
        .unwrap_or(text)
        .trim()
        .to_string()
}

/// Generate index page
fn generate_index_page(database: &DocDatabase) -> String {
    let config = &database.config;

    let mut content = format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{} {} - Documentation</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="container">
            <h1><a href="index.html">{}</a> <span style="color: #808080;">{}</span></h1>
            <div class="search-container">
                <input type="text" id="search-input" placeholder="Search..." autocomplete="off">
                <div id="search-results"></div>
            </div>
        </div>
    </header>
    <nav>
        <div class="container">
            <a href="index.html">Overview</a>
            <a href="all.html">All Items</a>
        </div>
    </nav>
    <main>
        <div class="container">
            <h1>{} Documentation</h1>
            <p>Welcome to the documentation for <strong>{}</strong> version <strong>{}</strong>.</p>
            <p>This documentation contains {} items across various modules.</p>
"#,
        config.project_name,
        config.project_version,
        config.project_name,
        config.project_version,
        config.project_name,
        config.project_name,
        config.project_version,
        database.items.len()
    );

    // Add module list
    let modules = collect_modules(database);
    if !modules.is_empty() {
        content.push_str("<h2>Modules</h2>\n<ul class=\"module-list\">\n");
        for module in modules {
            let module_name = if module.is_empty() {
                "crate".to_string()
            } else {
                module.join("::")
            };
            let file_name = if module.is_empty() {
                "root.html".to_string()
            } else {
                format!("module.{}.html", module.join("."))
            };
            content.push_str(&format!(
                "<li><a href=\"{}\">{}</a></li>\n",
                file_name, module_name
            ));
        }
        content.push_str("</ul>\n");
    }

    // Add quick links by kind
    content.push_str("<h2>Quick Links</h2>\n<div class=\"item-grid\">\n");

    for kind in [
        DocItemKind::Function,
        DocItemKind::Struct,
        DocItemKind::Enum,
        DocItemKind::Trait,
    ] {
        let count = database.items_of_kind(kind.clone()).count();
        if count > 0 {
            content.push_str(&format!(
                r#"
<div class="item-card">
    <a href="all.html#{}s">{}s</a>
    <div class="item-description">{} items</div>
</div>
"#,
                kind.as_str().to_lowercase(),
                kind.as_str(),
                count
            ));
        }
    }

    content.push_str("</div>\n");

    content.push_str(
        r#"
        </div>
    </main>
    <footer>
        <div class="container">
            Generated by jet-doc
        </div>
    </footer>
    <script src="search.js"></script>
</body>
</html>
"#,
    );

    content
}

/// Generate all items page
fn generate_all_items_page(database: &DocDatabase) -> String {
    let config = &database.config;

    let mut content = format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>All Items - {} Documentation</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="container">
            <h1><a href="index.html">{}</a> <span style="color: #808080;">{}</span></h1>
            <div class="search-container">
                <input type="text" id="search-input" placeholder="Search..." autocomplete="off">
                <div id="search-results"></div>
            </div>
        </div>
    </header>
    <nav>
        <div class="container">
            <a href="index.html">Overview</a>
            <a href="all.html">All Items</a>
        </div>
    </nav>
    <main>
        <div class="container">
            <h1>All Items</h1>
"#,
        config.project_name, config.project_name, config.project_version
    );

    // Group items by kind
    let mut by_kind: HashMap<DocItemKind, Vec<&DocItem>> = HashMap::new();
    for item in &database.items {
        by_kind.entry(item.kind.clone()).or_default().push(item);
    }

    // Generate sections for each kind
    for kind in [
        DocItemKind::Module,
        DocItemKind::Function,
        DocItemKind::Struct,
        DocItemKind::Enum,
        DocItemKind::Trait,
        DocItemKind::TypeAlias,
        DocItemKind::Constant,
        DocItemKind::Effect,
    ] {
        if let Some(items) = by_kind.get(&kind) {
            content.push_str(&format!(
                r#"
<div class="item-section" id="{}s">
    <h2>{}s</h2>
    <div class="item-grid">
"#,
                kind.as_str().to_lowercase(),
                kind.as_str()
            ));

            for item in items {
                let description = item
                    .doc_comment
                    .as_ref()
                    .map(|d| first_sentence(d))
                    .unwrap_or_default();

                content.push_str(&format!(
                    r#"
<div class="item-card">
    <a href="{}.{}.html">{}</a>
    <div class="item-description">{}</div>
</div>
"#,
                    item.kind.as_str(),
                    item.name,
                    item.name,
                    escape_html(&description)
                ));
            }

            content.push_str("    </div>\n</div>\n");
        }
    }

    content.push_str(
        r#"
        </div>
    </main>
    <footer>
        <div class="container">
            Generated by jet-doc
        </div>
    </footer>
    <script src="search.js"></script>
</body>
</html>
"#,
    );

    content
}

/// Generate item detail page
fn generate_item_page(database: &DocDatabase, item: &DocItem) -> String {
    let config = &database.config;

    let mut content = format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{} {} - {} Documentation</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="container">
            <h1><a href="index.html">{}</a> <span style="color: #808080;">{}</span></h1>
            <div class="search-container">
                <input type="text" id="search-input" placeholder="Search..." autocomplete="off">
                <div id="search-results"></div>
            </div>
        </div>
    </header>
    <nav>
        <div class="container">
            <a href="index.html">Overview</a>
            <a href="all.html">All Items</a>
        </div>
    </nav>
    <main>
        <div class="container">
            <div class="breadcrumbs">
                <a href="index.html">crate</a>
"#,
        item.kind.as_str(),
        item.name,
        config.project_name,
        config.project_name,
        config.project_version
    );

    // Add breadcrumb for module path
    for (i, segment) in item.module_path.iter().enumerate() {
        let path = item.module_path[..=i].join(".");
        content.push_str(&format!(
            r#"
<span class="separator">::</span><a href="module.{}.html">{}</a>
"#,
            path, segment
        ));
    }

    content.push_str(&format!(
        r#"
<span class="separator">::</span><span>{}</span>
            </div>

            <div class="doc-item">
                <div class="doc-item-header">
                    <span class="item-kind">{}</span>
                    <h1 class="item-name">{}</h1>
                </div>
"#,
        item.name,
        item.kind.as_str(),
        item.name
    ));

    // Add source link
    content.push_str(&format!(
        r#"
<div class="source-link">
    <a href="file://{}">Source</a>
</div>
"#,
        item.source_file.display()
    ));

    // Add type signature
    if let Some(sig) = &item.type_signature {
        content.push_str(&format!(
            r#"
<div class="item-signature">{}</div>
"#,
            highlight_signature(sig)
        ));
    }

    // Add AI annotations
    if let Some(ai) = &item.ai_annotations {
        content.push_str(&generate_ai_annotation_html(ai));
    }

    // Add doc comment
    if let Some(doc) = &item.doc_comment {
        let html_doc = markdown_to_html(doc, &ComrakOptions::default());
        content.push_str(&format!(
            r#"
<div class="doc-comment">{}</div>
"#,
            html_doc
        ));
    }

    // Add examples
    let examples = extract_examples_from_doc(item.doc_comment.as_deref().unwrap_or(""));
    for (i, example) in examples.iter().enumerate() {
        content.push_str(&format!(
            r#"
<div class="example">
    <div class="example-header">Example {}</div>
    <pre class="example-code"><code>{}</code></pre>
</div>
"#,
            i + 1,
            escape_html(example)
        ));
    }

    content.push_str(
        r#"
            </div>
        </div>
    </main>
    <footer>
        <div class="container">
            Generated by jet-doc
        </div>
    </footer>
    <script src="search.js"></script>
</body>
</html>
"#,
    );

    content
}

/// Generate module page
fn generate_module_page(database: &DocDatabase, module_path: &[String]) -> String {
    let config = &database.config;
    let module_name = if module_path.is_empty() {
        "crate".to_string()
    } else {
        module_path.join("::")
    };

    let mut content = format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Module {} - {} Documentation</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="container">
            <h1><a href="index.html">{}</a> <span style="color: #808080;">{}</span></h1>
            <div class="search-container">
                <input type="text" id="search-input" placeholder="Search..." autocomplete="off">
                <div id="search-results"></div>
            </div>
        </div>
    </header>
    <nav>
        <div class="container">
            <a href="index.html">Overview</a>
            <a href="all.html">All Items</a>
        </div>
    </nav>
    <main>
        <div class="container">
            <h1>Module {}</h1>
"#,
        module_name, config.project_name, config.project_name, config.project_version, module_name
    );

    // Get items in this module
    let items: Vec<_> = database.items_in_module(module_path);

    if items.is_empty() {
        content.push_str("<p>No items documented in this module.</p>\n");
    } else {
        // Group by kind
        let mut by_kind: HashMap<DocItemKind, Vec<&&DocItem>> = HashMap::new();
        for item in &items {
            by_kind.entry(item.kind.clone()).or_default().push(item);
        }

        for kind in [
            DocItemKind::Function,
            DocItemKind::Struct,
            DocItemKind::Enum,
            DocItemKind::Trait,
        ] {
            if let Some(kind_items) = by_kind.get(&kind) {
                content.push_str(&format!(
                    "<h2>{}s</h2>\n<ul class=\"module-list\">\n",
                    kind.as_str()
                ));
                for item in kind_items {
                    let description = item
                        .doc_comment
                        .as_ref()
                        .map(|d| first_sentence(d))
                        .unwrap_or_default();
                    content.push_str(&format!(
                        r#"
<li>
    <a href="{}.{}.html">{}</a> - {}
</li>
"#,
                        item.kind.as_str(),
                        item.name,
                        item.name,
                        escape_html(&description)
                    ));
                }
                content.push_str("</ul>\n");
            }
        }
    }

    content.push_str(
        r#"
        </div>
    </main>
    <footer>
        <div class="container">
            Generated by jet-doc
        </div>
    </footer>
    <script src="search.js"></script>
</body>
</html>
"#,
    );

    content
}

/// Generate AI annotation HTML
fn generate_ai_annotation_html(ai: &AiAnnotations) -> String {
    let mut html = String::from(
        r#"
<div class="ai-annotations">"#,
    );
    html.push_str(
        r#"
<span class="ai-label">AI Generated</span>"#,
    );

    if let Some(confidence) = ai.confidence {
        let percentage = (confidence * 100.0) as u32;
        html.push_str(&format!(
            r#"
<span class="ai-confidence">
    <span>Confidence: {}%</span>
    <div class="confidence-bar">
        <div class="confidence-fill" style="width: {}%"></div>
    </div>
</span>"#,
            percentage, percentage
        ));
    }

    if let Some(generated_by) = &ai.generated_by {
        html.push_str(&format!(
            r#"
<span>Generated by: {}</span>"#,
            escape_html(generated_by)
        ));
    }

    if let Some(edit_count) = ai.human_edit_count {
        html.push_str(&format!(
            r#"
<span>Human edits: {}</span>"#,
            edit_count
        ));
    }

    html.push_str("</div>");
    html
}

/// Highlight signature with syntax coloring
fn highlight_signature(sig: &str) -> String {
    // Simple syntax highlighting
    let mut result = String::new();
    let parts: Vec<_> = sig.split_whitespace().collect();

    for (i, part) in parts.iter().enumerate() {
        if i > 0 {
            result.push(' ');
        }

        if matches!(
            *part,
            "pub"
                | "fn"
                | "struct"
                | "enum"
                | "trait"
                | "type"
                | "const"
                | "effect"
                | "impl"
                | "where"
        ) {
            result.push_str(&format!(
                r#"
<span class="keyword">{}</span>"#,
                part
            ));
        } else if part.starts_with("fn") || part.starts_with("struct") {
            // Function or struct name
            let name = &part[2..];
            result.push_str(&format!(
                r#"
<span class="keyword">{}</span><span class="function-name">{}</span>"#,
                &part[..2],
                name
            ));
        } else {
            result.push_str(part);
        }
    }

    result
}

/// Extract examples from doc comment
fn extract_examples_from_doc(doc: &str) -> Vec<&str> {
    let mut examples = Vec::new();
    let lines: Vec<_> = doc.lines().collect();
    let mut in_example = false;
    let mut current_example = Vec::new();

    for line in lines {
        let trimmed = line.trim();

        if trimmed.starts_with("```") {
            if in_example {
                // End of example
                if !current_example.is_empty() {
                    examples.push(current_example.join("\n").leak() as &str);
                }
                current_example.clear();
                in_example = false;
            } else {
                // Start of example
                in_example = true;
            }
        } else if in_example {
            current_example.push(line);
        }
    }

    examples
}

/// Escape HTML special characters
fn escape_html(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// Collect all unique module paths
fn collect_modules(database: &DocDatabase) -> Vec<Vec<String>> {
    let mut modules: Vec<Vec<String>> = database
        .items
        .iter()
        .map(|item| item.module_path.clone())
        .collect();

    // Deduplicate
    modules.sort();
    modules.dedup();

    modules
}

impl DocItemKind {
    fn as_str(&self) -> &'static str {
        match self {
            DocItemKind::Function => "fn",
            DocItemKind::Struct => "struct",
            DocItemKind::Enum => "enum",
            DocItemKind::Trait => "trait",
            DocItemKind::TypeAlias => "type",
            DocItemKind::Constant => "const",
            DocItemKind::Module => "mod",
            DocItemKind::Effect => "effect",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_html() {
        assert_eq!(escape_html("<div>"), "&lt;div&gt;");
        assert_eq!(escape_html("foo & bar"), "foo &amp; bar");
    }

    #[test]
    fn test_first_sentence() {
        assert_eq!(first_sentence("Hello world. This is more."), "Hello world");
        assert_eq!(first_sentence("Single sentence"), "Single sentence");
    }
}
