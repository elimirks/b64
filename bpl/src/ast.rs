use std::str;

enum RootStatement {
    RFun(String, Vec<String>, Box<Statement>),
}

enum Statement {
    SBlock(Vec<Box<Statement>>),
    // Statement representing executing a single expr
    SExpr(Box<Expr>),
}

enum Expr {
    EApp(String, Vec<Box<Expr>>),
    EVar(String),
    EAdd(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
enum Res<T> {
    // All parsing functions return (position_delta, value)
    // All "parse_" functions should increment the context position be position_delta
    // This makes it easy to rewind
    Success(usize, T),
    // Failed parsing should NEVER increase position
    Failure(String),
}

struct ParseContext<'a> {
    content: &'a [u8],
    position: usize,
}

fn parse_anychar(context: &mut ParseContext) -> Res<char> {
    if context.position < context.content.len() {
        context.position += 1;
        Res::Success(1, context.content[context.position - 1] as char)
    } else {
        Res::Failure("Hit EOF, no chars to parse".to_string())
    }
}

// Extract an alphanumeric slice at the given offset
// Returns an empty slice if the offset is out of bounds
// Or if there are no alphanumeric characters at that position
fn alphanumeric_slice(slice: &[u8], offset: usize) -> &[u8] {
    let mut len = 0;
    while offset + len < slice.len() {
        if (slice[offset + len] as char).is_alphanumeric() {
            len += 1;
        } else {
            break;
        }
    }
    &slice[offset..offset + len]
}

fn parse_id(context: &mut ParseContext) -> Res<String> {
    match parse_anychar(context) {
        Res::Success(position_delta, first_char) => {
            if first_char.is_alphabetic() {
                let name_bytes = alphanumeric_slice(context.content,
                                                    context.position - 1);
                let name = str::from_utf8(name_bytes)
                    .expect("Invalid UTF8 character")
                    .to_string();

                context.position += name.len() - 1;
                Res::Success(name.len(), name)
            } else {
                // Rewind
                context.position -= position_delta;
                Res::Failure("Expected alphabetic character".to_string())
            }
        },
        Res::Failure(message) => Res::Failure(message)
    }
}

fn parse_char(context: &mut ParseContext, expected_char: char) -> Res<char> {
    match parse_anychar(context) {
        Res::Success(position_delta, found_char) => {
            if found_char == expected_char {
                Res::Success(position_delta, found_char)
            } else {
                context.position -= position_delta;
                Res::Failure(format!("Char {} not found", expected_char))
            }
        },
        Res::Failure(message) => Res::Failure(message)
    }
}

pub fn parse(content: String) {
    let mut context = ParseContext {
        content: content.as_bytes(),
        position: 0,
    };

    println!("{:?}", parse_id(&mut context));
    println!("{:?}", parse_char(&mut context, '('));
}
