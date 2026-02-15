//! Integration tests for the Jet parser

use jet_lexer::Lexer;
use jet_parser::Parser;

fn parse_module(input: &str) -> Result<jet_parser::ast::Module, String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    parser.parse_module().map_err(|e| format!("{:?}", e))
}

fn assert_parses(input: &str) {
    match parse_module(input) {
        Ok(_) => {}
        Err(e) => panic!("Expected parsing to succeed, but got error: {}", e),
    }
}

#[test]
fn test_binary_tree() {
    assert_parses(
        r#"
enum Tree[T]:
    | Leaf
    | Node(T, Tree[T], Tree[T])

fn contains[T](tree: Tree[T], value: T) -> bool:
    match tree:
        | Leaf -> false
        | Node(v, left, right) ->
            if v == value:
                true
            else:
                contains(left, value) or contains(right, value)
"#,
    );
}

#[test]
fn test_linked_list() {
    assert_parses(
        r#"
enum List[T]:
    | Nil
    | Cons(T, List[T])

fn map[T, U](list: List[T], f: fn(T) -> U) -> List[U]:
    match list:
        | Nil -> Nil
        | Cons(head, tail) -> Cons(f(head), map(tail, f))
"#,
    );
}

#[test]
fn test_result_type() {
    assert_parses(
        r#"
enum Result[T, E]:
    | Ok(T)
    | Err(E)

fn map_result[T, E, U](result: Result[T, E], f: fn(T) -> U) -> Result[U, E]:
    match result:
        | Ok(value) -> Ok(f(value))
        | Err(error) -> Err(error)
"#,
    );
}

#[test]
fn test_http_server() {
    assert_parses(
        r#"
enum HttpMethod:
    | Get
    | Post
    | Put
    | Delete

struct Request:
    method: HttpMethod
    path: string

struct Response:
    status: int
    body: string

trait Handler:
    fn handle_request(self, req: Request) -> Response

struct Server:
    port: int

impl Server:
    fn new(port: int) -> Server:
        return Server { port: port }

    async fn start(self):
        pass
"#,
    );
}

#[test]
fn test_iterator_pattern() {
    assert_parses(
        r#"
trait Iterator:
    type Item
    fn next(self) -> Option[Self.Item]

impl Iterator for Range:
    type Item = int
    fn next(self) -> Option[int]:
        if self.current < self.end:
            let value = self.current
            self.current += 1
            return Some(value)
        else:
            return None
"#,
    );
}

#[test]
fn test_json_parser() {
    assert_parses(
        r#"
enum JsonValue:
    | Null
    | Bool(bool)
    | Number(float)
    | String(string)
    | Array(Vec[JsonValue])
    | Object(Map[string, JsonValue])

enum ParseError:
    | UnexpectedToken
    | UnexpectedEof

struct Parser:
    input: string
    position: int

impl Parser:
    fn new(input: string) -> Parser:
        return Parser { input, position: 0 }

    fn parse(self) -> Result[JsonValue, ParseError]:
        return self.parse_value()

    fn parse_value(self) -> Result[JsonValue, ParseError]:
        match self.peek():
            | 'n' -> self.parse_null()
            | '"' -> self.parse_string()
            | '[' -> self.parse_array()
            | _ -> Err(UnexpectedToken)

    fn parse_null(self) -> Result[JsonValue, ParseError]:
        return Ok(Null)
"#,
    );
}

#[test]
fn test_query_builder() {
    assert_parses(
        r#"
enum SqlType:
    | Integer
    | Text
    | Real

struct Column:
    name: string
    ty: SqlType

struct SelectQuery:
    table: string
    columns: Vec[string]
    where_clause: Option[string]

impl SelectQuery:
    fn from(table: string) -> SelectQuery:
        return SelectQuery {
            table: table,
            columns: Vec::new(),
            where_clause: None
        }

    fn select(self, columns: Vec[string]) -> SelectQuery:
        self.columns = columns
        return self

    fn where_eq(self, column: string, value: string) -> SelectQuery:
        self.where_clause = Some(column + " = " + value)
        return self
"#,
    );
}
