use crate::ast::*;
use crate::tokenizer::*;

pub struct ParseContext {
    pub content: Vec<char>,
    // Offset should only increment once we've parsed a "good" value
    pub offset: usize,
    // Store parse errors here.
    // By convention, return None whenever there is a parse error
    // And you must always set a message!
    pub error: Option<String>,
    // Used for the tokenizer stack
    pub next_tok: Option<Token>,
}

impl ParseContext {
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

    pub fn pos(&self) -> Pos {
        // TODO: Proper file number
        Pos::new(self.offset, 0)
    }
}

fn parse_global_var(
    c: &mut ParseContext, pos: Pos, id: String
) -> Option<RootStatement> {
    match pop_tok(c) {
        Some(Token::LBracket) => {
            match pop_tok(c) {
                Some(Token::Value(size)) if size > 0 => {
                    if !parse_tok(c, Token::RBracket) {
                        None
                    } else if !parse_tok(c, Token::Semicolon) {
                        None
                    } else {
                        Some(RootStatement::Variable(pos, Var::Vec(id, size)))
                    }
                },
                Some(other) => {
                    c.error = Some(format!(
                        "Expected positive int. {:?} given", other));
                    None
                },
                None => None,
            }
        },
        Some(Token::Semicolon) => {
            Some(RootStatement::Variable(pos, Var::Single(id)))
        },
        Some(other) => {
            c.error = Some(format!(
                "Expected [, (, or ;. Found {:?}", other
            ));
            None
        },
        None => None,
    }
}

fn parse_root_statement(c: &mut ParseContext) -> Option<RootStatement> {
    match pop_tok(c) {
        // Root statements always begin with an id
        Some(Token::Id(id)) => {
            let pos = c.pos();

            match pop_tok(c) {
                Some(Token::LParen) => {
                    push_tok(c, Token::LParen);
                    parse_fun(c, pos, id)
                },
                Some(tok) => {
                    push_tok(c, tok);
                    parse_global_var(c, pos, id)
                },
                None => None,
            }
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
fn parse_fun(
    c: &mut ParseContext, pos: Pos, name: String
) -> Option<RootStatement> {
    if !parse_tok(c, Token::LParen) {
        return None;
    }

    let mut args = Vec::<String>::new();
    // To alternate between comma & arg parsing
    let mut should_parse_param = true;

    // Parse args and closing paren
    loop {
        let tok = pop_tok(c);
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

    Some(RootStatement::Function(pos, name, args, body.unwrap()))
}

fn parse_statement(c: &mut ParseContext) -> Option<Statement> {
    let pos = c.pos();
    let tok = pop_tok(c);
    if tok.is_none() {
        return None;
    }

    match tok.unwrap() {
        Token::Return      => parse_statement_return(c),
        Token::Break       => parse_statement_break(c),
        Token::LBrace      => parse_statement_block(c),
        Token::Auto        => parse_statement_auto(c),
        Token::Extern      => parse_statement_extern(c),
        Token::If          => parse_statement_if(c),
        Token::While       => parse_statement_while(c),
        Token::Semicolon   => Some(Statement::Null),
        Token::Label(name) => Some(Statement::Label(pos, name)),
        Token::Goto        => parse_statement_goto(c),
        tok => {
            push_tok(c, tok);
            parse_statement_expr(c)
        },
    }
}

fn parse_auto_entry(c: &mut ParseContext, name: String) -> Option<Var> {
    match pop_tok(c) {
        Some(Token::LBracket) => {
            match pop_tok(c) {
                Some(Token::Value(value)) if value >= 1 => {
                    if parse_tok(c, Token::RBracket) {
                        Some(Var::Vec(name, value))
                    } else {
                        None
                    }
                },
                Some(other) => {
                    c.error = Some(format!(
                        "Expected positive int. Found {:?}", other
                    ));
                    None
                },
                None => None,
            }
        },
        Some(tok) => {
            push_tok(c, tok);
            Some(Var::Single(name))
        },
        None => None,
    }
}

// TODO: This loop delim technique is used in multiple places. Abstract away!
// Expects opening `extrn` to have been parsed
fn parse_statement_extern(c: &mut ParseContext) -> Option<Statement> {
    let pos = c.pos();
    let mut ids = Vec::<String>::new();
    let mut should_parse_param = true;

    loop {
        let tok = pop_tok(c);
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

    Some(Statement::Extern(pos, ids))
}

// Expects opening `auto` to have been parsed
fn parse_statement_auto(c: &mut ParseContext) -> Option<Statement> {
    let pos = c.pos();
    let mut vars = Vec::<Var>::new();
    let mut should_parse_param = true;

    loop {
        let tok = pop_tok(c);
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

                match parse_auto_entry(c, id) {
                    Some(entry) => {
                        vars.push(entry);
                        should_parse_param = false;
                    },
                    None => return None,
                }
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

    Some(Statement::Auto(pos, vars))
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

    let else_body = match pop_tok(c) {
        Some(Token::Else) => {
            match parse_statement(c) {
                Some(body) => Some(Box::new(body)),
                _          => return None,
            }
        },
        Some(tok) => {
            push_tok(c, tok);
            None
        },
        None => None,
    };

    Some(Statement::If(
        cond_expr.unwrap(),
        Box::new(if_body.unwrap()),
        else_body
    ))
}

// Expect "goto" to have been parsed already
fn parse_statement_goto(c: &mut ParseContext) -> Option<Statement> {
    let pos = c.pos();
    match pop_tok(c) {
        Some(Token::Id(name)) => {
            if !parse_tok(c, Token::Semicolon) {
                return None;
            }

            Some(Statement::Goto(pos, name))
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
        match pop_tok(c) {
            None => return None,
            Some(Token::RBrace) => {
                break;
            },
            Some(tok) => {
                push_tok(c, tok);
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

// Expects the `break` keyword to have been parsed already
fn parse_statement_break(c: &mut ParseContext) -> Option<Statement> {
    let pos = c.pos();
    if !parse_tok(c, Token::Semicolon) {
        return None;
    }

    Some(Statement::Break(pos))
}

// Expects the `return` keyword to have been parsed already
fn parse_statement_return(c: &mut ParseContext) -> Option<Statement> {
    match pop_tok(c) {
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

    let next_tok = pop_tok(c);
    if next_tok.is_none() {
        return None;
    }

    match next_tok.unwrap() {
        Token::Eq => match first_expr.unwrap() {
            Expr::Id(pos, lhs) => match parse_expr(c) {
                Some(rhs) => Some(Expr::Assignment(
                    pos,
                    lhs,
                    Box::new(rhs)
                )),
                None => None,
            },
            Expr::Dereference(pos, lhs) => match parse_expr(c) {
                Some(rhs) => Some(Expr::DerefAssignment(pos, lhs, Box::new(rhs))),
                None      => None,
            },
            _ => {
                c.error = Some(
                    "lhs of assignment must be an ID or dereference".to_string()
                );
                None
            },
        },
        Token::Plus       => chain_expr(c, first_expr.unwrap(), Op::Add),
        Token::Minus      => chain_expr(c, first_expr.unwrap(), Op::Sub),
        Token::EqEq       => chain_expr(c, first_expr.unwrap(), Op::Eq),
        Token::Le         => chain_expr(c, first_expr.unwrap(), Op::Le),
        Token::Lt         => chain_expr(c, first_expr.unwrap(), Op::Lt),
        Token::Ge         => chain_expr(c, first_expr.unwrap(), Op::Ge),
        Token::Gt         => chain_expr(c, first_expr.unwrap(), Op::Gt),
        Token::Ne         => chain_expr(c, first_expr.unwrap(), Op::Ne),
        Token::ShiftLeft  => chain_expr(c, first_expr.unwrap(), Op::ShiftLeft),
        Token::ShiftRight => chain_expr(c, first_expr.unwrap(), Op::ShiftRight),
        Token::Percent    => chain_expr(c, first_expr.unwrap(), Op::Mod),
        Token::Slash      => chain_expr(c, first_expr.unwrap(), Op::Div),
        tok => {
            // The next token isn't a chaining token... Rewind!
            push_tok(c, tok);
            first_expr
        },
    }
}

fn chain_expr(c: &mut ParseContext, lhs: Expr, op: Op) -> Option<Expr> {
    let pos = c.pos();
    let rhs = parse_expr(c);

    if rhs.is_none() {
        None
    } else {
        Some(Expr::Operator(
            pos,
            op,
            Box::new(lhs),
            Box::new(rhs.unwrap())
        ))
    }
}

fn parse_expr_id_unchained(c: &mut ParseContext, id: String) -> Option<Expr> {
    let pos = c.pos();
    match pop_tok(c) {
        Some(Token::LParen) => {
            parse_expr_call(c, id)
        },
        // Handle vector index sugar syntax
        Some(Token::LBracket) => {
            let index_expr = parse_expr(c);

            if index_expr.is_none() || !parse_tok(c, Token::RBracket) {
                return None;
            }

            Some(Expr::Dereference(pos.clone(),
                Box::new(Expr::Operator(
                    pos.clone(),
                    Op::Add,
                    Box::new(Expr::Id(pos.clone(), id)),
                    // Multiply by 8 (aka left shift by 4)
                    Box::new(Expr::Operator(
                        pos.clone(),
                        Op::ShiftLeft,
                        Box::new(index_expr.unwrap()),
                        Box::new(Expr::Int(pos, 4))
                    ))
                ))
            ))
        },
        Some(tok) => {
            push_tok(c, tok);
            Some(Expr::Id(pos, id))
        },
        None => None,
    }
}

fn parse_expr_unchained(c: &mut ParseContext) -> Option<Expr> {
    let pos = c.pos();
    let tok = pop_tok(c);
    if tok.is_none() {
        return None;
    }

    match tok.unwrap() {
        Token::Id(id) => parse_expr_id_unchained(c, id),
        Token::Value(value) => Some(Expr::Int(pos, value)),
        Token::Ampersand => match pop_tok(c) {
            Some(Token::Id(id)) => Some(Expr::Reference(pos, id)),
            Some(tok) => {
                c.error = Some(format!("Expected id, found {:?}", tok));
                None
            },
            None => None,
        },
        Token::Asterisk => match parse_expr_unchained(c) {
            Some(expr) => Some(Expr::Dereference(pos, Box::new(expr))),
            None       => None
        },
        // Allow parens for disambiguation
        Token::LParen => match parse_expr(c) {
            Some(expr) => {
                if parse_tok(c, Token::RParen) {
                    Some(expr)
                } else {
                    None
                }
            },
            None => None,
        },
        other => {
            c.error = Some(format!("Expected expression. {:?} found", other));
            None
        }
    }
}

// Assumes the rparen has already been parsed
fn parse_expr_call(c: &mut ParseContext, name: String) -> Option<Expr> {
    let mut params = Vec::<Expr>::new();
    // To alternate between comma & arg parsing
    let mut should_parse_param = true;
    let pos = c.pos();

    // Parse args and closing paren
    loop {
        let tok = pop_tok(c);
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
            tok => {
                push_tok(c, tok);
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

    Some(Expr::Call(pos, name, params))
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

    let line: &String = &c.content[current_row_offset..row_end]
        .into_iter()
        .collect();

    (line.to_string(), row, col)
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
        content: content.chars().collect(),
        offset: 0,
        error: None,
        next_tok: None,
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
