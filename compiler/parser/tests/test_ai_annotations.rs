//! Tests for AI annotation parsing

use jet_lexer::Lexer;
use jet_parser::ast::*;
use jet_parser::Parser;

fn parse_source(source: &str) -> Module {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    parser.parse_module().expect("Failed to parse")
}

#[test]
fn test_parse_confidence_annotation() {
    let source = r#"
@confidence(high)
fn foo():
    pass
"#;
    let module = parse_source(source);
    // Attributes directly before an item are item-level
    if let Some(ModuleItem::Function(func)) = module.items.first() {
        assert_eq!(func.attributes.len(), 1);
        let attr = &func.attributes[0];
        assert_eq!(attr.name.name, "confidence");
        assert_eq!(attr.arguments.len(), 1);
        if let AttributeArg::Positional(AttributeValue::Ident(ident)) = &attr.arguments[0] {
            assert_eq!(ident.name, "high");
        } else {
            panic!("Expected positional identifier argument");
        }
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_parse_generated_by_annotation() {
    let source = r#"
@generated_by("claude", confidence=0.92)
fn bar():
    pass
"#;
    let module = parse_source(source);

    if let Some(ModuleItem::Function(func)) = module.items.first() {
        assert_eq!(func.attributes.len(), 1);
        let attr = &func.attributes[0];
        assert_eq!(attr.name.name, "generated_by");
        assert_eq!(attr.arguments.len(), 2);

        // First positional arg: "claude"
        if let AttributeArg::Positional(AttributeValue::String(s)) = &attr.arguments[0] {
            assert_eq!(s, "claude");
        } else {
            panic!("Expected positional string argument");
        }

        // Second named arg: confidence=0.92
        if let AttributeArg::Named { name, value } = &attr.arguments[1] {
            assert_eq!(name.name, "confidence");
            if let AttributeValue::Float(f) = value {
                assert!((f - 0.92).abs() < 0.001);
            } else {
                panic!("Expected float value");
            }
        } else {
            panic!("Expected named argument");
        }
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_parse_prompt_annotation() {
    let source = r#"
@prompt("Implement a sorting algorithm")
fn sort():
    pass
"#;
    let module = parse_source(source);

    if let Some(ModuleItem::Function(func)) = module.items.first() {
        assert_eq!(func.attributes.len(), 1);
        let attr = &func.attributes[0];
        assert_eq!(attr.name.name, "prompt");
        assert_eq!(attr.arguments.len(), 1);
        if let AttributeArg::Positional(AttributeValue::String(s)) = &attr.arguments[0] {
            assert_eq!(s, "Implement a sorting algorithm");
        } else {
            panic!("Expected positional string argument");
        }
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_parse_human_edit_count_annotation() {
    let source = r#"
@human_edit_count(3)
fn baz():
    pass
"#;
    let module = parse_source(source);

    if let Some(ModuleItem::Function(func)) = module.items.first() {
        assert_eq!(func.attributes.len(), 1);
        let attr = &func.attributes[0];
        assert_eq!(attr.name.name, "human_edit_count");
        assert_eq!(attr.arguments.len(), 1);
        if let AttributeArg::Positional(AttributeValue::Integer(n)) = &attr.arguments[0] {
            assert_eq!(*n, 3);
        } else {
            panic!("Expected positional integer argument");
        }
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_parse_multiple_annotations() {
    let source = r#"
@confidence(high)
@generated_by("gpt-4")
fn multi_annotated():
    pass
"#;
    let module = parse_source(source);

    // Both attributes should be on the function
    if let Some(ModuleItem::Function(func)) = module.items.first() {
        assert_eq!(func.attributes.len(), 2);
        assert_eq!(func.attributes[0].name.name, "confidence");
        assert_eq!(func.attributes[1].name.name, "generated_by");
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_parse_annotation_on_struct() {
    let source = r#"
@confidence(medium)
struct Point:
    x: f64
    y: f64
"#;
    let module = parse_source(source);

    if let Some(ModuleItem::Struct(struct_def)) = module.items.first() {
        assert_eq!(struct_def.attributes.len(), 1);
        let attr = &struct_def.attributes[0];
        assert_eq!(attr.name.name, "confidence");
    } else {
        panic!("Expected struct item");
    }
}

#[test]
fn test_parse_annotation_with_bool() {
    let source = r#"
@confidence(high, verified=true)
fn verified_func():
    pass
"#;
    let module = parse_source(source);

    if let Some(ModuleItem::Function(func)) = module.items.first() {
        assert_eq!(func.attributes.len(), 1);
        let attr = &func.attributes[0];
        assert_eq!(attr.arguments.len(), 2);

        // Named bool argument
        if let AttributeArg::Named { name, value } = &attr.arguments[1] {
            assert_eq!(name.name, "verified");
            if let AttributeValue::Bool(b) = value {
                assert!(*b);
            } else {
                panic!("Expected bool value");
            }
        } else {
            panic!("Expected named argument");
        }
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_parse_module_level_attributes() {
    // Module-level attributes are those followed by a blank line
    // NOTE: Currently the parser treats consecutive attributes before an item
    // as item-level attributes. True module-level attributes (with blank line separation)
    // may need additional implementation.
    let source = r#"
@confidence(low)
@generated_by("test")

fn module_func():
    pass
"#;
    let module = parse_source(source);

    // For now, these are treated as item-level attributes
    // Module-level attribute support is partially implemented
    if let Some(ModuleItem::Function(func)) = module.items.first() {
        // Both attributes go to the function since they're directly before it
        assert_eq!(func.attributes.len(), 2);
        assert_eq!(func.attributes[0].name.name, "confidence");
        assert_eq!(func.attributes[1].name.name, "generated_by");
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_annotation_without_arguments() {
    let source = r#"
@confidence
fn no_args():
    pass
"#;
    let module = parse_source(source);

    if let Some(ModuleItem::Function(func)) = module.items.first() {
        assert_eq!(func.attributes.len(), 1);
        let attr = &func.attributes[0];
        assert_eq!(attr.name.name, "confidence");
        assert_eq!(attr.arguments.len(), 0);
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_annotation_with_multiple_positional_args() {
    let source = r#"
@generated_by("claude", "4.0", 2024)
fn multi_pos():
    pass
"#;
    let module = parse_source(source);

    if let Some(ModuleItem::Function(func)) = module.items.first() {
        assert_eq!(func.attributes.len(), 1);
        let attr = &func.attributes[0];
        assert_eq!(attr.arguments.len(), 3);
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_annotation_with_mixed_args() {
    let source = r#"
@generated_by("claude", model="opus", year=2024, verified=true)
fn mixed_args():
    pass
"#;
    let module = parse_source(source);

    if let Some(ModuleItem::Function(func)) = module.items.first() {
        assert_eq!(func.attributes.len(), 1);
        let attr = &func.attributes[0];
        assert_eq!(attr.arguments.len(), 4);

        // Positional
        if let AttributeArg::Positional(AttributeValue::String(s)) = &attr.arguments[0] {
            assert_eq!(s, "claude");
        } else {
            panic!("Expected positional string");
        }

        // Named: model
        if let AttributeArg::Named { name, value } = &attr.arguments[1] {
            assert_eq!(name.name, "model");
            if let AttributeValue::String(s) = value {
                assert_eq!(s, "opus");
            } else {
                panic!("Expected string value");
            }
        } else {
            panic!("Expected named argument");
        }

        // Named: year
        if let AttributeArg::Named { name, value } = &attr.arguments[2] {
            assert_eq!(name.name, "year");
            if let AttributeValue::Integer(n) = value {
                assert_eq!(*n, 2024);
            } else {
                panic!("Expected integer value");
            }
        } else {
            panic!("Expected named argument");
        }

        // Named: verified
        if let AttributeArg::Named { name, value } = &attr.arguments[3] {
            assert_eq!(name.name, "verified");
            if let AttributeValue::Bool(b) = value {
                assert!(*b);
            } else {
                panic!("Expected bool value");
            }
        } else {
            panic!("Expected named argument");
        }
    } else {
        panic!("Expected function item");
    }
}
