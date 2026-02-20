use jet_doc::extract::{extract_doc_comments_from_source, extract_examples, parse_doc_comment};

#[test]
fn test_extract_doc_comments() {
    let source = r#"### Adds two numbers together.
###
### @confidence 0.95
### @generated_by claude
###
### ```
### >>> add(2, 3)
### 5
### ```
pub fn add(a: int, b: int) -> int:
    a + b
"#;

    let doc_comments = extract_doc_comments_from_source(source);

    assert!(!doc_comments.is_empty());
    let first_doc = &doc_comments[0];
    assert!(first_doc.content.contains("Adds two numbers"));
}

#[test]
fn test_parse_doc_comment_with_ai_annotations() {
    let comment = "Adds two numbers.\n@confidence 0.95\n@generated_by claude";

    let (doc_text, ai_annotations) = parse_doc_comment(comment);

    assert!(doc_text.is_some());
    assert!(doc_text.unwrap().contains("Adds two numbers"));

    assert!(ai_annotations.is_some());
    let ai = ai_annotations.unwrap();
    assert_eq!(ai.confidence, Some(0.95));
    assert_eq!(ai.generated_by, Some("claude".to_string()));
}

#[test]
fn test_extract_examples() {
    let doc = r#"Adds two numbers.

```
>>> add(2, 3)
5
```
"#;

    let examples = extract_examples(doc);

    assert_eq!(examples.len(), 1);
    assert_eq!(examples[0].code, "add(2, 3)");
    assert_eq!(examples[0].expected_output, Some("5".to_string()));
}
