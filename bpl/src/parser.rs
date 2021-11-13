use std::str;

use crate::ast::*;

struct ParseContext<'a> {
    content: &'a [u8],
    // Offset should only increment once we've parsed a "good" value
    offset: usize,
    // toklen is used to track the length of the current token
    toklen: usize,
    error: Option<String>,
}

impl ParseContext<'_> {
    fn position(&self) -> usize {
        self.offset + self.toklen
    }

    fn peek_char(&self) -> Option<char> {
        if self.position() < self.content.len() {
            Some(self.content[self.position()] as char)
        } else {
            None
        }
    }

    fn at_eof(&self) -> bool {
        self.position() >= self.content.len()
    }

    fn has_error(&self) -> bool {
        !self.error.is_none()
    }
}

fn parse_anychar(c: &mut ParseContext) -> char {
    if c.position() < c.content.len() {
        c.toklen += 1;
        c.content[c.offset + c.toklen - 1] as char
    } else {
        c.error = Some("Hit EOF, cannot parse a character".to_string());
        '\0'
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

fn parse_char(c: &mut ParseContext, expected_char: char) {
    if parse_anychar(c) != expected_char {
        c.toklen -= 1;
        c.error = Some(format!("Char {} not found", expected_char));
    }
}

fn parse_tok_char(c: &mut ParseContext, expected_char: char) {
    parse_char(c, expected_char);

    if !c.has_error() {
        c.offset += c.toklen;
    }
    c.toklen = 0;
}

// Parses a word token. Will fail if it's followed by something alphanumeric
// TODO: Test this
fn parse_tok_word(c: &mut ParseContext, s: &str) {
    let end = c.offset + s.len();

    if end >= c.content.len()
            || &c.content[c.offset..end] != s.as_bytes()
            || (end < c.content.len() && (c.content[end] as char).is_alphanumeric()) {
        c.error = Some(format!("Could not parse {}", s));
    } else {
        c.offset += s.len();
    }
}

// Parse any amount of horizontal whitespace
fn parse_hs(c: &mut ParseContext) {
    while !c.at_eof() && c.content[c.offset] as char == ' ' {
        c.offset += 1;
    }
}

fn parse_tok_id(c: &mut ParseContext) -> String {
    let first_char = parse_anychar(c);

    if c.has_error() {
        c.toklen = 0;
        String::new()
    } else if !first_char.is_alphabetic() {
        c.error = Some("ID must begin with an alphabetic char".to_string());
        c.toklen = 0;
        String::new()
    } else {
        let name_bytes = alphanumeric_slice(c.content, c.position() - 1);
        let name = str::from_utf8(name_bytes)
            .expect("Invalid UTF8 character")
            .to_string();

        // We've parsed a token, so we reset the toklen
        c.offset += name_bytes.len();
        c.toklen = 0;
        name
    }
}

// Let the fun begin
fn parse_fun(c: &mut ParseContext) -> Option<RootStatement> {
    let name = parse_tok_id(c);
    if c.has_error() {
        return None;
    }

    parse_hs(c);
    parse_tok_char(c, '(');
    if c.has_error() {
        return None;
    }
    parse_hs(c);

    let mut args = Vec::<String>::new();

    while !c.has_error() {
        parse_hs(c);

        let id = parse_tok_id(c);
        if c.has_error() {
            break;
        }
        args.push(id);

        parse_hs(c);
        parse_tok_char(c, ',');
    }
    // If there was an error, we want to look for the closing paren
    // FIXME: Store the error in case we don't find a closing paren either
    c.error = None;

    parse_tok_char(c, ')');
    if c.has_error() {
        return None;
    }
    parse_hs(c);

    let body = parse_statement(c);
    if c.has_error() {
        return None;
    }

    Some(RootStatement::Function(
        name,
        args,
        body.unwrap()
    ))
}

fn parse_statement(c: &mut ParseContext) -> Option<Statement> {
    let initial_offset = c.offset;

    match parse_statement_return(c) {
        Some(statement) => return Some(statement),
        None => {
            // rewind
            c.offset = initial_offset;
            c.error = None;
        },
    }

    parse_statement_expr(c)
}

fn parse_statement_return(c: &mut ParseContext) -> Option<Statement> {
    parse_tok_word(c, "return");
    if c.has_error() {
        return None;
    }
    parse_hs(c);
    parse_tok_char(c, '(');

    if c.has_error() {
        return None;
    }

    let expr = parse_expr(c);
    if c.has_error() {
        return None;
    }

    parse_hs(c);
    parse_tok_char(c, ')');
    parse_hs(c);
    parse_tok_char(c, ';');

    if c.has_error() {
        return None;
    }

    Some(Statement::Return(expr.unwrap()))
}

fn parse_statement_expr(c: &mut ParseContext) -> Option<Statement> {
    let expr = parse_expr(c);
    if c.has_error() {
        return None;
    }
    parse_hs(c);
    parse_tok_char(c, ';');
    Some(Statement::Expr(expr.unwrap()))
}

fn parse_tok_op(c: &mut ParseContext) -> Option<Op> {
    let op = match c.peek_char() {
        Some('+') => Some(Op::Add),
        _         => None,
    };

    if !op.is_none() {
        c.offset += 1;
    }
    op
}

fn parse_expr(c: &mut ParseContext) -> Option<Expr> {
    let head = parse_expr_unchained(c);
    parse_hs(c);

    // Handle operator chaining
    match parse_tok_op(c) {
        Some(op) => {
            parse_hs(c);
            // TODO: Instead of recursion, aggregate in a loop
            // That will make it easy to implement operator precedence
            let rhs = parse_expr(c);

            if c.has_error() {
                None
            } else {
                Some(Expr::Operator(
                    op,
                    Box::new(head.unwrap()),
                    Box::new(rhs.unwrap())
                ))
            }
        },
        None => head
    }
}

fn parse_expr_unchained(c: &mut ParseContext) -> Option<Expr> {
    parse_expr_id(c)
}

fn parse_expr_id(c: &mut ParseContext) -> Option<Expr> {
    let id = parse_tok_id(c);
    if c.has_error() {
        None
    } else {
        Some(Expr::Id(id))
    }
}

pub fn parse(content: String) -> Vec<RootStatement> {
    let mut c = ParseContext {
        content: content.as_bytes(),
        offset: 0,
        toklen: 0,
        error: None,
    };

    let root = parse_fun(&mut c);
    if c.has_error() {
        println!("Parse error: {}", c.error.unwrap());
        std::process::exit(1);
    }
    vec!(root.unwrap())
}
