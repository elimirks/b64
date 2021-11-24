use crate::ast::*;
use crate::tokenizer::*;

pub struct ParseResult {
    // Stored in order of file_id (in Pos)
    // Stores (file_name, file_contents)
    pub file_contents: Vec<(String, String)>,
    pub root_statements: Vec<RootStatement>,
    pub error: Option<CompErr>,
}

pub struct ParseContext {
    pub content: Vec<char>,
    // Offset for use by the tokenizer
    pub offset: usize,
    pub file_id: usize,
    // Used for the tokenizer stack
    pub tok_stack: Vec<(Pos, Token)>,
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

    pub fn pos(&self) -> Pos {
        Pos::new(self.offset, self.file_id)
    }
}

fn parse_global_var(
    c: &mut ParseContext, name: String
) -> Result<RootStatement, CompErr> {
    let pos = c.pos();
    let var = parse_var_entry(c, name)?;
    parse_tok(c, Token::Semicolon)?;
    Ok(RootStatement::Variable(pos, var))
}

fn parse_vec_values(c: &mut ParseContext) -> Result<Vec<i64>, CompErr> {
    let mut values = vec!();

    match pop_tok(c)? {
        (_, Token::Value(value)) => {
            values.push(value);
        },
        other => {
            push_tok(c, other);
            return Ok(values);
        },
    };
    // At this point, we're starting at comma
    loop {
        match pop_tok(c)? {
            (comma_pos, Token::Comma) => {
                match pop_tok(c)? {
                    (_, Token::Value(value)) => {
                        values.push(value);
                    },
                    other => {
                        // Unfortunately, B has ambiguous grammar...
                        // So we're forced to push 2 tokens :(
                        push_tok(c, (comma_pos, Token::Comma));
                        push_tok(c, other);
                        break;
                    },
                }
            },
            other => {
                push_tok(c, other);
                break;
            },
        }
    }
    return Ok(values);
}

fn parse_var_entry(c: &mut ParseContext, name: String) -> Result<Var, CompErr> {
    match pop_tok(c)? {
       (_, Token::LBracket) => {
           let given_vec_size = match pop_tok(c)? {
               (_, Token::Value(max_index)) if max_index >= 0 => {
                   parse_tok(c, Token::RBracket)?;
                   max_index + 1
               },
               (_, Token::RBracket) => 0,
               (pos, other) => {
                   return CompErr::err(&pos, format!(
                       "Expected positive int. Found {:?}", other));
               },
           };
           let vec_values = parse_vec_values(c)?;
           // According to the B spec, we choose the max of these two values for
           // the catual vector size
           let vec_size = std::cmp::max(given_vec_size, vec_values.len() as i64);
           Ok(Var::Vec(name, vec_size, vec_values))
        },
        (_, Token::Value(value)) => {
            Ok(Var::Single(name, Some(value)))
        },
        other => {
            push_tok(c, other);
            Ok(Var::Single(name, None))
        },
    }
}

fn parse_root_statement(
    c: &mut ParseContext
) -> Result<Option<RootStatement>, CompErr> {
    match pop_tok(c)? {
        // Root statements always begin with an id
        (_, Token::Id(id)) => {
            let (pos, tok) = pop_tok(c)?;

            match tok {
                Token::LParen => {
                    push_tok(c, (pos.clone(), Token::LParen));
                    Ok(Some(parse_fun(c, pos, id)?))
                },
                tok => {
                    push_tok(c, (pos, tok));
                    Ok(Some(parse_global_var(c, id)?))
                },
            }
        },
        (_, Token::Eof) => Ok(None),
        (pos, tok) => CompErr::err(&pos, format!(
            "Expected id. {:?} found", tok)),
    }
}

// Parses everything after the name of a function
fn parse_fun(
    c: &mut ParseContext, pos: Pos, name: String
) -> Result<RootStatement, CompErr> {
    parse_tok(c, Token::LParen)?;

    let mut args = Vec::<String>::new();
    // To alternate between comma & arg parsing
    let mut should_parse_param = true;

    // Parse args and closing paren
    loop {
        let (pos, tok) = pop_tok(c)?;

        match tok {
            Token::RParen => break,
            Token::Id(id) => {
                if !should_parse_param {
                    return CompErr::err(
                        &pos, "Comma expected, id found".to_string());
                }
                args.push(id);
                should_parse_param = false;
            },
            Token::Comma => {
                if should_parse_param {
                    return CompErr::err(
                        &pos, "id expected, comma found".to_string());
                }
                should_parse_param = true;
            },
            other => return CompErr::err(
                    &pos, format!("Unexpected token: {:?}", other)),
        }
    }

    let body = parse_statement(c)?;
    Ok(RootStatement::Function(pos, name, args, body))
}

fn parse_statement(c: &mut ParseContext) -> Result<Statement, CompErr> {
    let (pos, tok) = pop_tok(c)?;

    match tok {
        Token::Return      => parse_statement_return(c),
        Token::Break       => parse_statement_break(c, pos),
        Token::LBrace      => parse_statement_block(c),
        Token::Auto        => parse_statement_auto(c, pos),
        Token::Extern      => parse_statement_extern(c, pos),
        Token::If          => parse_statement_if(c),
        Token::While       => parse_statement_while(c),
        Token::Semicolon   => Ok(Statement::Null),
        Token::Label(name) => Ok(Statement::Label(pos, name)),
        Token::Goto        => parse_statement_goto(c),
        tok => {
            push_tok(c, (pos, tok));
            parse_statement_expr(c)
        },
    }
}

// TODO: This loop delim technique is used in multiple places. Abstract away!
// Expects opening `extrn` to have been parsed
fn parse_statement_extern(
    c: &mut ParseContext, pos: Pos
) -> Result<Statement, CompErr> {
    let mut ids = Vec::<String>::new();
    let mut should_parse_param = true;

    loop {
        let (pos, tok) = pop_tok(c)?;

        match tok {
            Token::Semicolon => break,
            Token::Id(id) => {
                if !should_parse_param {
                    return CompErr::err(
                        &pos, "Comma expected, id found".to_string());
                }

                ids.push(id);
                should_parse_param = false;
            },
            Token::Comma => {
                if should_parse_param {
                    return CompErr::err(
                        &pos, "id expected, comma found".to_string());
                }
                should_parse_param = true;
            },
            other => return CompErr::err(
                &pos, format!("Unexpected token: {:?}", other)),
        }
    }

    Ok(Statement::Extern(pos, ids))
}

// Expects opening `auto` to have been parsed
fn parse_statement_auto(
    c: &mut ParseContext, pos: Pos
) -> Result<Statement, CompErr> {
    let mut vars = Vec::<Var>::new();
    let mut should_parse_param = true;

    loop {
        let (pos, tok) = pop_tok(c)?;
        match tok {
            Token::Semicolon => break,
            Token::Id(id) => {
                if !should_parse_param {
                    return CompErr::err(
                        &pos, "Comma expected, id found".to_string());
                }

                vars.push(parse_var_entry(c, id)?);
                should_parse_param = false;
            },
            Token::Comma => {
                if should_parse_param {
                    return CompErr::err(
                        &pos, "id expected, comma found".to_string());
                }
                should_parse_param = true;
            },
            other => return CompErr::err(
                &pos, format!("Unexpected token: {:?}", other)),
        }
    }

    Ok(Statement::Auto(pos, vars))
}

// Expect "if" to have been parsed already
fn parse_statement_if(c: &mut ParseContext) -> Result<Statement, CompErr> {
    parse_tok(c, Token::LParen)?;
    let cond_expr = parse_expr(c)?;
    parse_tok(c, Token::RParen)?;

    let if_body = parse_statement(c)?;

    let else_body = match pop_tok(c)? {
        (_, Token::Else) => Some(Box::new(parse_statement(c)?)),
        other => {
            push_tok(c, other);
            None
        },
    };

    Ok(Statement::If(
        cond_expr,
        Box::new(if_body),
        else_body
    ))
}

// Expect "goto" to have been parsed already
fn parse_statement_goto(c: &mut ParseContext) -> Result<Statement, CompErr> {
    match pop_tok(c)? {
        (pos, Token::Id(name)) => {
            parse_tok(c, Token::Semicolon)?;
            Ok(Statement::Goto(pos, name))
        },
        (pos, _) => CompErr::err(&pos, "Expected ID".to_string()),
    }
}

// Expect "while" to have been parsed already
fn parse_statement_while(c: &mut ParseContext) -> Result<Statement, CompErr> {
    parse_tok(c, Token::LParen)?;
    let cond_expr = parse_expr(c)?;
    parse_tok(c, Token::RParen)?;
    let body = parse_statement(c)?;

    Ok(Statement::While(
        cond_expr,
        Box::new(body)
    ))
}

// Expects opening `{` to have been parsed
fn parse_statement_block(c: &mut ParseContext) -> Result<Statement, CompErr> {
    let mut statements = Vec::<Statement>::new();

    loop {
        match pop_tok(c)? {
            (_, Token::RBrace) => break,
            other => {
                push_tok(c, other);
                statements.push(parse_statement(c)?);
            },
        }
    }

    Ok(Statement::Block(statements))
}

// Expects the `break` keyword to have been parsed already
fn parse_statement_break(
    c: &mut ParseContext, pos: Pos
) -> Result<Statement, CompErr> {
    parse_tok(c, Token::Semicolon)?;
    Ok(Statement::Break(pos))
}

// Expects the `return` keyword to have been parsed already
fn parse_statement_return(c: &mut ParseContext) -> Result<Statement, CompErr> {
    match pop_tok(c)? {
        (_, Token::LParen) => {},
        (_, Token::Semicolon) => return Ok(Statement::Return),
        (pos, _) => return CompErr::err(&pos, format!(
            "Expected ( or ; after return statment")),
    }

    let expr = parse_expr(c)?;
    parse_tok(c, Token::RParen)?;
    parse_tok(c, Token::Semicolon)?;

    Ok(Statement::ReturnExpr(expr))
}

fn parse_statement_expr(c: &mut ParseContext) -> Result<Statement, CompErr> {
    let expr = parse_expr(c)?;
    parse_tok(c, Token::Semicolon)?;
    Ok(Statement::Expr(expr))
}

fn parse_expr(c: &mut ParseContext) -> Result<Expr, CompErr> {
    let first_expr = parse_expr_unchained(c)?;
    let (pos, next_tok) = pop_tok(c)?;

    match next_tok {
        Token::Eq => match first_expr {
            Expr::Id(pos, lhs) => Ok(Expr::Assignment(
                pos,
                lhs,
                Box::new(parse_expr(c)?)
            )),
            Expr::Dereference(pos, lhs) => Ok(Expr::DerefAssignment(
                pos,
                lhs,
                Box::new(parse_expr(c)?)
            )),
            expr => {
                CompErr::err(
                    &expr.pos(),
                    "lhs of assignment must be an ID or dereference".to_string()
                )
            },
        },
        Token::Plus       => chain_expr(c, first_expr, Op::Add),
        Token::Minus      => chain_expr(c, first_expr, Op::Sub),
        Token::EqEq       => chain_expr(c, first_expr, Op::Eq),
        Token::Le         => chain_expr(c, first_expr, Op::Le),
        Token::Lt         => chain_expr(c, first_expr, Op::Lt),
        Token::Ge         => chain_expr(c, first_expr, Op::Ge),
        Token::Gt         => chain_expr(c, first_expr, Op::Gt),
        Token::Ne         => chain_expr(c, first_expr, Op::Ne),
        Token::ShiftLeft  => chain_expr(c, first_expr, Op::ShiftLeft),
        Token::ShiftRight => chain_expr(c, first_expr, Op::ShiftRight),
        Token::Percent    => chain_expr(c, first_expr, Op::Mod),
        Token::Slash      => chain_expr(c, first_expr, Op::Div),
        tok => {
            // The next token isn't a chaining token... Rewind!
            push_tok(c, (pos, tok));
            Ok(first_expr)
        },
    }
}

fn chain_expr(c: &mut ParseContext, lhs: Expr, op: Op) -> Result<Expr, CompErr> {
    let rhs = parse_expr(c)?;

    Ok(Expr::Operator(
        rhs.pos(),
        op,
        Box::new(lhs),
        Box::new(rhs)
    ))
}

fn parse_expr_id_unchained(
    c: &mut ParseContext, fun_id_pos: Pos, id: String
) -> Result<Expr, CompErr> {
    let (pos, tok) = pop_tok(c)?;
    match tok {
        Token::LParen => {
            parse_expr_call(c, fun_id_pos, id)
        },
        // Handle vector index sugar syntax
        Token::LBracket => {
            let index_expr = parse_expr(c)?;
            parse_tok(c, Token::RBracket)?;

            Ok(Expr::Dereference(pos.clone(),
                Box::new(Expr::Operator(
                    pos.clone(),
                    Op::Add,
                    Box::new(Expr::Id(pos.clone(), id)),
                    // Multiply by 8 (aka left shift by 4)
                    Box::new(Expr::Operator(
                        pos.clone(),
                        Op::ShiftLeft,
                        Box::new(index_expr),
                        Box::new(Expr::Int(pos, 4))
                    ))
                ))
            ))
        },
        tok => {
            push_tok(c, (pos.clone(), tok));
            Ok(Expr::Id(pos, id))
        },
    }
}

fn parse_expr_unchained(c: &mut ParseContext) -> Result<Expr, CompErr> {
    let (pos, tok) = pop_tok(c)?;

    match tok {
        Token::Id(id) => parse_expr_id_unchained(c, pos, id),
        Token::Value(value) => Ok(Expr::Int(pos, value)),
        Token::Ampersand => match pop_tok(c)? {
            (pos, Token::Id(id)) => Ok(Expr::Reference(pos, id)),
            (pos, tok) => CompErr::err(&pos, format!("Expected id, found {:?}", tok)),
        },
        Token::Asterisk => Ok(Expr::Dereference(
            pos, Box::new(parse_expr_unchained(c)?))),
        // Allow parens for disambiguation
        Token::LParen => {
            let expr = parse_expr(c)?;
            parse_tok(c, Token::RParen)?;
            Ok(expr)
        },
        other => CompErr::err(&pos, format!(
            "Expected expression. {:?} found", other))
    }
}

// Assumes the rparen has already been parsed
fn parse_expr_call(
    c: &mut ParseContext, fun_id_pos: Pos, name: String
) -> Result<Expr, CompErr> {
    let mut params = Vec::<Expr>::new();
    // To alternate between comma & arg parsing
    let mut should_parse_param = true;

    // Parse args and closing paren
    loop {
        let (pos, tok) = pop_tok(c)?;

        match tok {
            Token::RParen => break,
            Token::Comma => {
                if should_parse_param {
                    return CompErr::err(
                        &pos, "Expr expected, comma found".to_string());
                }
                should_parse_param = true;
            },
            tok => {
                push_tok(c, (pos.clone(), tok));
                if !should_parse_param {
                    return CompErr::err(
                        &pos, "Comma expected".to_string());
                }
                params.push(parse_expr(c)?);
                should_parse_param = false;
            },
        }
    }

    Ok(Expr::Call(fun_id_pos, name, params))
}

/**
 * Returns the (line,row number,column number) of the current offset
 * Meant for displaying error messages
 * It has to traverse the entire content to figure it out, so use this with care
 */
fn get_parse_position(content: &Vec<char>, offset: usize) -> (String, usize, usize) {
    let mut row = 1;
    let mut col = 0;
    let mut current_row_offset = 0;

    for i in 0..offset {
        if content[i] as char == '\n' {
            row += 1;
            col = 0;
            current_row_offset = i + 1;
        } else {
            col += 1
        }
    }

    let mut row_end = current_row_offset;
    while row_end < content.len() && content[row_end] as char != '\n' {
        row_end += 1;
    }

    let line: &String = &content[current_row_offset..row_end]
        .into_iter()
        .collect();

    (line.to_string(), row, col)
}

pub fn print_comp_error(parse_result: &ParseResult, err: &CompErr) {
    println!("Compile error: {}", err.message);
    match &err.pos {
        Some(pos) => {
            let (file_name, content) = &parse_result.file_contents[pos.file_id];
            println!("In file: {}", file_name);

            let (line, row, col) = get_parse_position(
                &content.chars().collect(), pos.offset);

            let prefix = format!("{} |", row);
            println!("{}{}", prefix, line);

            for _ in 0..col + prefix.len() {
                print!(" ");
            }

            println!("^")
        },
        None => {},
    }
}

fn parse_content(file_id: usize, content: &String) -> Result<Vec<RootStatement>, CompErr> {
    let mut c = ParseContext {
        content: content.chars().collect(),
        offset: 0,
        file_id: file_id,
        tok_stack: vec!(),
    };

    let mut roots = vec!();
    loop {
        match parse_root_statement(&mut c)? {
            Some(statement) => roots.push(statement),
            None            => break,
        }
    }
    Ok(roots)
}

pub fn parse_files(paths: &Vec<String>) -> ParseResult {
    let mut result = ParseResult {
        file_contents: vec!(),
        root_statements: vec!(),
        error: None,
    };

    for (file_id, path) in paths.iter().enumerate() {
        let content = std::fs::read_to_string(path)
            .expect(format!("Failed reading {}", path).as_str());

        match parse_content(file_id, &content) {
            Ok(mut statements) => {
                result.root_statements.append(&mut statements);
                result.file_contents.push((path.to_string(), content));
            },
            Err(error) => {
                result.error = Some(error);
                result.file_contents.push((path.to_string(), content));
                break;
            },
        }
    }

    result
}
