use std::str;

use crate::ast::*;

#[derive(Debug, PartialEq)]
enum Token {
    Id(String),
    Int(i64),
    Return,
    Auto,
    Extern,
    Eof,
    While,
    If,
    Else,
    Goto,
    Switch,
    Break,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Comma,
    Plus,
    Minus,
    Equals,
}

struct ParseContext<'a> {
    content: &'a [u8],
    // Offset should only increment once we've parsed a "good" value
    offset: usize,
    error: Option<String>,
}

impl ParseContext<'_> {
    fn peek_char(&self) -> Option<char> {
        if self.offset < self.content.len() {
            Some(self.content[self.offset] as char)
        } else {
            None
        }
    }

    fn peek_chars(&self, len: usize) -> &[u8] {
        if self.at_eof() {
            return &[];
        }
        let end = std::cmp::min(self.offset + len, self.content.len());
        &self.content[self.offset..end]
    }

    fn at_eof(&self) -> bool {
        self.offset >= self.content.len()
    }

    fn has_error(&self) -> bool {
        !self.error.is_none()
    }
}

// Returns None for invalid tokens
// Returns Token::Eof for Eof (considered a valid token)
fn get_tok(c: &mut ParseContext) -> Option<Token> {
    // Seek past useless whitespace
    parse_ws(c);

    // First try tokenizing "word like" things
    // Includes integers
    let current_word = alphanumeric_slice(c.content, c.offset);
    if current_word.len() > 0 {
        c.offset += current_word.len();

        let str_word = str::from_utf8(current_word)
            .expect("Invalid UTF8 found in file");

        match str_word {
            "return" => return Some(Token::Return),
            "auto"   => return Some(Token::Auto),
            "extern" => return Some(Token::Extern),
            "eof"    => return Some(Token::Eof),
            "while"  => return Some(Token::While),
            "if"     => return Some(Token::If),
            "else"   => return Some(Token::Else),
            "goto"   => return Some(Token::Goto),
            "switch" => return Some(Token::Switch),
            "break"  => return Some(Token::Break),
            _ => {},
        }

        if (current_word[0] as char).is_alphabetic() {
            Some(Token::Id(str_word.to_string()))
        } else {
            // Else it must be numeric
            match str_word.parse::<i64>() {
                Ok(num) => Some(Token::Int(num)),
                _       => {
                    c.offset -= current_word.len(); // Rewind
                    c.error = Some(format!("Invalid int literal: {}", str_word));
                    None
                },
            }
        }
    } else {
        let next_char = c.peek_char();
        if next_char.is_none() {
            return Some(Token::Eof);
        }
        c.offset += 1;

        match next_char.unwrap() {
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '=' => Some(Token::Equals),
            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            '{' => Some(Token::LBrace),
            '}' => Some(Token::RBrace),
            ';' => Some(Token::Semicolon),
            ',' => Some(Token::Comma),
            other => {
                c.offset -= 1;
                c.error = Some(format!("Invalid token: {}", other));
                None
            }
        }
    }
}

fn peek_tok(c: &mut ParseContext) -> Option<Token> {
    parse_ws(c); // It's ok to just jump back to right after the whitespace
    let initial = c.offset;
    let tok = get_tok(c);
    c.offset = initial;
    tok
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

// Parse any amount of whitespace, including comments
fn parse_ws(c: &mut ParseContext) {
    let mut parsed = false;
    while !c.at_eof() {
        match c.peek_char() {
            Some(' ')  => c.offset += 1,
            Some('\n') => c.offset += 1,
            Some('/')  => {
                if !parse_comment(c) {
                    break;
                }
            },
            _ => break,
        }
    }
}

/**
 * When calling this, it assumes self.content[self.offset] = '/'
 * @return true if it successfully parsed a comment
 */
fn parse_comment(c: &mut ParseContext) -> bool {
    if c.offset + 1 >= c.content.len() {
        return false;
    }
    if c.content[c.offset + 1] as char != '*' {
        return false;
    }
    c.offset += 2;

    let mut one = '\0';
    let mut two = '\0';

    while !c.at_eof() {
        one = two;
        two = c.content[c.offset] as char;
        c.offset += 1;

        if one == '*' && two == '/' {
            break;
        }
    }
    true
}

// Returns false if it failed to parse the given token
fn parse_tok(c: &mut ParseContext, expected: Token) -> bool {
    match get_tok(c) {
        Some(recieved) => {
            if expected == recieved {
                true
            } else {
                c.error = Some(format!("Expected {:?}, but {:?} was found",
                                       expected, recieved));
                false
            }
        },
        None => false,
    }
}

fn parse_root_statement(c: &mut ParseContext) -> Option<RootStatement> {
    match get_tok(c) {
        // Root statements always begin with an id
        Some(Token::Id(id)) => {
            parse_fun(c, id)
        },
        _ => None,
    }
}

// Parses everything after the name of a function
fn parse_fun(c: &mut ParseContext, name: String) -> Option<RootStatement> {
    if !parse_tok(c, Token::LParen) {
        return None;
    }

    let mut args = Vec::<String>::new();
    // To alternate between comma & arg parsing
    let mut should_parse_arg = true;

    // Parse args and closing paren
    loop {
        let tok = get_tok(c);
        if tok.is_none() {
            return None;
        }

        match tok.unwrap() {
            Token::RParen => break,
            Token::Id(id) => {
                if !should_parse_arg {
                    c.error = Some("Comma expected, id found".to_string());
                    return None;
                }
                args.push(id);
                should_parse_arg = false;
            },
            Token::Comma => {
                if should_parse_arg {
                    c.error = Some("id expected, comma found".to_string());
                    return None;
                }
                should_parse_arg = true;
            },
            other => {
                c.error = Some(format!("Unexpected token: {:?}", other));
                return None;
            },
        }
    }

    let body = parse_statement(c);
    if body.is_none() {
        return None;
    }

    Some(RootStatement::Function(name, args, body.unwrap()))
}

fn parse_statement(c: &mut ParseContext) -> Option<Statement> {
    let initial_offset = c.offset;
    let tok = get_tok(c);
    if tok.is_none() {
        return None;
    }

    match tok.unwrap() {
        Token::Return => parse_statement_return(c),
        Token::LBrace => parse_statement_block(c),
        Token::Auto   => parse_statement_auto(c),
        _ => {
            c.offset = initial_offset;
            parse_statement_expr(c)
        },
    }
}

// Expects opening `auto` to have been parsed
fn parse_statement_auto(c: &mut ParseContext) -> Option<Statement> {
    let mut ids = Vec::<String>::new();
    let mut should_parse_arg = true;

    loop {
        let tok = get_tok(c);
        if tok.is_none() {
            return None;
        }

        match tok.unwrap() {
            Token::Semicolon => break,
            Token::Id(id) => {
                if !should_parse_arg {
                    c.error = Some("Comma expected, id found".to_string());
                    return None;
                }
                ids.push(id);
                should_parse_arg = false;
            },
            Token::Comma => {
                if should_parse_arg {
                    c.error = Some("id expected, comma found".to_string());
                    return None;
                }
                should_parse_arg = true;
            },
            other => {
                c.error = Some(format!("Unexpected token: {:?}", other));
                return None;
            },
        }
    }

    Some(Statement::Auto(ids))
}

// Expects opening `{` to have been parsed
fn parse_statement_block(c: &mut ParseContext) -> Option<Statement> {
    let mut statements = Vec::<Statement>::new();

    loop {
        match peek_tok(c) {
            None                => return None,
            Some(Token::RBrace) => break,
            _ => {
                let statement = parse_statement(c);
                if statement.is_none() {
                    return None;
                }
                statements.push(statement.unwrap());
            },
        }
    }

    Some(Statement::Block(statements))
}

// Expects the `return` keyword to have been parsed already
fn parse_statement_return(c: &mut ParseContext) -> Option<Statement> {
    if !parse_tok(c, Token::LParen) {
        return None;
    }

    let expr = parse_expr(c);
    if expr.is_none() {
        return None;
    }

    if !parse_tok(c, Token::RParen) || !parse_tok(c, Token::Semicolon) {
        return None;
    }

    Some(Statement::Return(expr.unwrap()))
}

fn parse_statement_expr(c: &mut ParseContext) -> Option<Statement> {
    let expr = parse_expr(c);
    if expr.is_none() {
        return None;
    }

    if !parse_tok(c, Token::Semicolon) {
        return None
    }

    Some(Statement::Expr(expr.unwrap()))
}

fn parse_expr(c: &mut ParseContext) -> Option<Expr> {
    let first_expr = parse_expr_unchained(c);
    if first_expr.is_none() {
        return None;
    }

    // Handle operator chaining
    let initial_offset = c.offset;

    let next_tok = get_tok(c);
    if next_tok.is_none() {
        return None;
    }

    // FIXME: This is a mess
    match next_tok.unwrap() {
        Token::Equals => {
            match first_expr.unwrap() {
                Expr::Id(lhs) => {
                    let rhs = parse_expr(c);
                    if rhs.is_none() {
                        None
                    } else {
                        Some(Expr::Assignment(
                            lhs,
                            Box::new(rhs.unwrap())
                        ))
                    }
                },
                _ => {
                    c.error = Some("lhs of assignment must be an ID".to_string());
                    None
                },
            }
        },
        Token::Plus  => chain_expr(c, first_expr.unwrap(), Op::Add),
        Token::Minus => chain_expr(c, first_expr.unwrap(), Op::Sub),
        _ => {
            // The next token isn't a chaining token... Rewind!
            c.offset = initial_offset;
            first_expr
        },
    }
}

fn chain_expr(c: &mut ParseContext, lhs: Expr, op: Op) -> Option<Expr> {
    let rhs = parse_expr(c);

    if rhs.is_none() {
        None
    } else {
        Some(Expr::Operator(
            op,
            Box::new(lhs),
            Box::new(rhs.unwrap())
        ))
    }
}

fn parse_expr_unchained(c: &mut ParseContext) -> Option<Expr> {
    let tok = get_tok(c);
    if tok.is_none() {
        return None;
    }

    match tok.unwrap() {
        Token::Id(id)     => Some(Expr::Id(id)),
        Token::Int(value) => Some(Expr::Int(value)),
        other => {
            c.error = Some(format!("Expected ID. {:?} found", other));
            None
        }
    }
}

/**
 * Returns the (line,row number,column number) of the current offset
 * Meant for displaying error messages
 * It has to traverse the entire content to figure it out, so use this with care
 */
fn get_parse_position(c: &ParseContext) -> (String, usize, usize) {
    let mut row = 1;
    let mut col = 0;
    let mut current_row_offset = 0;

    for i in 0..c.offset {
        if c.content[i] as char == '\n' {
            row += 1;
            col = 0;
            current_row_offset = i + 1;
        } else {
            col += 1
        }
    }

    let mut row_end = current_row_offset;
    while row_end < c.content.len() && c.content[row_end] as char != '\n' {
        row_end += 1;
    }

    let line = str::from_utf8(&c.content[current_row_offset..row_end])
        .unwrap()
        .to_string();

    (line, row, col)
}

fn print_error(c: &mut ParseContext) {
    let (line, row, col) = get_parse_position(&c);
    println!("Parse error: {}", c.error.as_ref().unwrap());

    let prefix = format!("{} |", row);
    println!("{}{}", prefix, line);

    for _ in 0..col + prefix.len() {
        print!(" ");
    }

    println!("^")
}

pub fn parse(content: String) -> Vec<RootStatement> {
    let mut c = ParseContext {
        content: content.as_bytes(),
        offset: 0,
        error: None,
    };

    let mut roots = vec!();
    loop {
        match parse_root_statement(&mut c) {
            Some(statement) => roots.push(statement),
            None => {
                if c.has_error() {
                    print_error(&mut c);
                    std::process::exit(1);
                } else if c.at_eof() {
                    break;
                }
            },
        }
    }
    roots
}
