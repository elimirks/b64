use std::str;
use crate::ast::*;
use crate::tokenizer::*;

pub struct ParseContext<'a> {
    pub content: &'a [u8],
    // Offset should only increment once we've parsed a "good" value
    pub offset: usize,
    pub error: Option<String>,
}

impl ParseContext<'_> {
    pub fn peek_char(&self) -> Option<char> {
        if self.offset < self.content.len() {
            Some(self.content[self.offset] as char)
        } else {
            None
        }
    }

    pub fn at_eof(&self) -> bool {
        self.offset >= self.content.len()
    }

    pub fn has_error(&self) -> bool {
        !self.error.is_none()
    }
}

fn parse_root_statement(c: &mut ParseContext) -> Option<RootStatement> {
    match get_tok(c) {
        // Root statements always begin with an id
        Some(Token::Id(id)) => {
            parse_fun(c, id)
        },
        Some(Token::Eof) => None,
        Some(other) => {
            c.error = Some(format!("Expected id. {:?} found", other));
            None
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
    let mut should_parse_param = true;

    // Parse args and closing paren
    loop {
        let tok = get_tok(c);
        if tok.is_none() {
            return None;
        }

        match tok.unwrap() {
            Token::RParen => break,
            Token::Id(id) => {
                if !should_parse_param {
                    c.error = Some("Comma expected, id found".to_string());
                    return None;
                }
                args.push(id);
                should_parse_param = false;
            },
            Token::Comma => {
                if should_parse_param {
                    c.error = Some("id expected, comma found".to_string());
                    return None;
                }
                should_parse_param = true;
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
        Token::Return      => parse_statement_return(c),
        Token::LBrace      => parse_statement_block(c),
        Token::Auto        => parse_statement_auto(c),
        Token::If          => parse_statement_if(c),
        Token::While       => parse_statement_while(c),
        Token::Semicolon   => Some(Statement::Null),
        Token::Label(name) => Some(Statement::Label(name)),
        Token::Goto        => parse_statement_goto(c),
        _ => {
            c.offset = initial_offset;
            parse_statement_expr(c)
        },
    }
}

// Expects opening `auto` to have been parsed
fn parse_statement_auto(c: &mut ParseContext) -> Option<Statement> {
    let mut ids = Vec::<String>::new();
    let mut should_parse_param = true;

    loop {
        let tok = get_tok(c);
        if tok.is_none() {
            return None;
        }

        match tok.unwrap() {
            Token::Semicolon => break,
            Token::Id(id) => {
                if !should_parse_param {
                    c.error = Some("Comma expected, id found".to_string());
                    return None;
                }
                ids.push(id);
                should_parse_param = false;
            },
            Token::Comma => {
                if should_parse_param {
                    c.error = Some("id expected, comma found".to_string());
                    return None;
                }
                should_parse_param = true;
            },
            other => {
                c.error = Some(format!("Unexpected token: {:?}", other));
                return None;
            },
        }
    }

    Some(Statement::Auto(ids))
}

// Expect "if" to have been parsed already
fn parse_statement_if(c: &mut ParseContext) -> Option<Statement> {
    if !parse_tok(c, Token::LParen) {
        return None;
    }

    let cond_expr = parse_expr(c);
    if cond_expr.is_none() {
        return None;
    }

    if !parse_tok(c, Token::RParen) {
        return None;
    }

    let if_body = parse_statement(c);
    if if_body.is_none() {
        return None;
    }

    let else_body = match peek_tok(c) {
        Some(Token::Else) => {
            get_tok(c); // Actually consume it
            match parse_statement(c) {
                Some(body) => Some(Box::new(body)),
                _          => return None,
            }
        },
        _ => None,
    };

    Some(Statement::If(
        cond_expr.unwrap(),
        Box::new(if_body.unwrap()),
        else_body
    ))
}

// Expect "goto" to have been parsed already
fn parse_statement_goto(c: &mut ParseContext) -> Option<Statement> {
    match get_tok(c) {
        Some(Token::Id(name)) => {
            if !parse_tok(c, Token::Semicolon) {
                return None;
            }

            Some(Statement::Goto(name))
        },
        _ => {
            c.error = Some("Expected ID".to_string());
            None
        },
    }
}

// Expect "while" to have been parsed already
fn parse_statement_while(c: &mut ParseContext) -> Option<Statement> {
    if !parse_tok(c, Token::LParen) {
        return None;
    }

    let cond_expr = parse_expr(c);
    if cond_expr.is_none() {
        return None;
    }

    if !parse_tok(c, Token::RParen) {
        return None;
    }

    let body = parse_statement(c);
    if body.is_none() {
        return None;
    }

    Some(Statement::While(
        cond_expr.unwrap(),
        Box::new(body.unwrap())
    ))
}

// Expects opening `{` to have been parsed
fn parse_statement_block(c: &mut ParseContext) -> Option<Statement> {
    let mut statements = Vec::<Statement>::new();

    loop {
        match peek_tok(c) {
            None => return None,
            Some(Token::RBrace) => {
                c.offset += 1;
                break;
            },
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
    match get_tok(c) {
        Some(Token::LParen) => {},
        Some(Token::Semicolon) => return Some(Statement::Return),
        _ => {
            c.error = Some(format!("Expected ( or ; after return statment"));
            return None;
        },
    }

    let expr = parse_expr(c);
    if expr.is_none() {
        return None;
    }

    if !parse_tok(c, Token::RParen) || !parse_tok(c, Token::Semicolon) {
        return None;
    }

    Some(Statement::ReturnExpr(expr.unwrap()))
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

    match next_tok.unwrap() {
        Token::Eq => {
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
        Token::EqEq  => chain_expr(c, first_expr.unwrap(), Op::Eq),
        Token::Le    => chain_expr(c, first_expr.unwrap(), Op::Le),
        Token::Lt    => chain_expr(c, first_expr.unwrap(), Op::Lt),
        Token::Ge    => chain_expr(c, first_expr.unwrap(), Op::Ge),
        Token::Gt    => chain_expr(c, first_expr.unwrap(), Op::Gt),
        Token::Ne    => chain_expr(c, first_expr.unwrap(), Op::Ne),
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
        Token::Id(id) => {
            match peek_tok(c) {
                Some(Token::LParen) => parse_expr_call(c, id),
                _                   => Some(Expr::Id(id))
            }
        },
        Token::Int(value) => Some(Expr::Int(value)),
        other => {
            c.error = Some(format!("Expected expression. {:?} found", other));
            None
        }
    }
}

// Assumes the rparen has already been parsed
fn parse_expr_call(c: &mut ParseContext, name: String) -> Option<Expr> {
    // Skip the rparen, since we safely assume it has been parsed
    get_tok(c);

    let mut params = Vec::<Expr>::new();
    // To alternate between comma & arg parsing
    let mut should_parse_param = true;

    // Parse args and closing paren
    loop {
        let initial_offset = c.offset;
        let tok = get_tok(c);
        if tok.is_none() {
            return None;
        }

        match tok.unwrap() {
            Token::RParen => break,
            Token::Comma => {
                if should_parse_param {
                    c.error = Some("Expr expected, comma found".to_string());
                    return None;
                }
                should_parse_param = true;
            },
            _ => {
                c.offset = initial_offset;
                if !should_parse_param {
                    c.error = Some("Comma expected".to_string());
                    return None;
                }
                match parse_expr(c) {
                    Some(expr) => params.push(expr),
                    None       => return None,
                }
                should_parse_param = false;
            },
        }
    }

    Some(Expr::Call(name, params))
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
