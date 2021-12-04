use crate::ast::*;
use crate::tokenizer::*;

use std::path::{Path, PathBuf};
use std::collections::HashSet;

pub struct ParseResult {
    // Stored in order of file_id (in Pos)
    // Stores (file_name, file_contents)
    pub file_contents: Vec<(String, String)>,
    // Stores (file_id, strings_vec)
    pub strings: Vec<(usize, Vec<Vec<char>>)>,
    pub root_statements: Vec<RootStatement>,
    pub error: Option<CompErr>,
}

pub struct ParseContext {
    pub content: Vec<char>,
    // Offset for use by the tokenizer
    pub offset: usize,
    pub file_id: usize,
    // Tracks the "floating" string literals that aren't assigned to vectors
    pub strings: Vec<Vec<char>>,
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

// Packs the given chars into 64 bit wide chars.
// Return value will always be null terminated.
// Expects all chars to be valid!
pub fn pack_chars(chars: &Vec<char>) -> Vec<i64> {
    let mut values = vec!();
    let mut i = 0;

    while i < chars.len() {
        let mut char_index = 0;
        let mut value = 0;
        while char_index < 8 && i < chars.len() {
            value += (chars[i] as i64) << (char_index * 8);
            char_index += 1;
            i += 1;
        }
        values.push(value);
    }
    // Ensure null termination
    if values.is_empty() || (values.last().unwrap() >> (7 * 8)) != 0i64 {
        values.push(0);
    }
    values
}

fn parse_vec_values(c: &mut ParseContext) -> Result<Vec<i64>, CompErr> {
    let mut values = vec!();

    match pop_tok(c)? {
        (_, Token::Int(value)) => {
            values.push(value);
        },
        (_, Token::Char(chars)) => {
            values.push(pack_chars(&chars)[0]);
        },
        (_, Token::Str(chars)) => return Ok(pack_chars(&chars)),
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
                    (_, Token::Int(value)) => {
                        values.push(value);
                    },
                    (_, Token::Char(chars)) => {
                        values.push(pack_chars(&chars)[0]);
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
               (_, Token::Int(max_index)) if max_index >= 0 => {
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
        (_, Token::Int(value)) => {
            Ok(Var::Single(name, Some(value)))
        },
        (_, Token::Char(chars)) => {
            Ok(Var::Single(name, Some(pack_chars(&chars)[0])))
        },
        other => {
            push_tok(c, other);
            Ok(Var::Single(name, None))
        },
    }
}

// Expects the @import token to have been parsed
fn parse_import(
    c: &mut ParseContext
) -> Result<RootStatement, CompErr> {
    match pop_tok(c)? {
        (pos, Token::Str(chars)) => {
            let path: String = chars.into_iter().collect();
            parse_tok(c, Token::Semicolon)?;
            Ok(RootStatement::Import(pos, path))
        },
        (pos, tok) => CompErr::err(&pos, format!(
            "String expected. {:?} given", tok)),
    }
}

fn parse_root_statement(
    c: &mut ParseContext
) -> Result<Option<RootStatement>, CompErr> {
    match pop_tok(c)? {
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
        (_, Token::Import) => Ok(Some(parse_import(c)?)),
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
    // NOTE: The `BinOp::Assign` here has no meaning. It's just a placeholder
    let mut op_exprs = vec!((BinOp::Assign(None), parse_expr_unchained(c)?));
    op_exprs.append(&mut parse_op_exprs(c)?);

    let lr_op_order = vec!(
        vec!(BinOp::Div, BinOp::Mod, BinOp::Mul),
        vec!(BinOp::Add, BinOp::Sub),
        vec!(BinOp::ShiftLeft, BinOp::ShiftRight),
        vec!(BinOp::Gt, BinOp::Lt, BinOp::Ge, BinOp::Le),
        vec!(BinOp::Eq, BinOp::Ne),
        vec!(BinOp::And),
        vec!(BinOp::Xor),
        vec!(BinOp::Or),
    );

    for ops in lr_op_order {
        let mut i = 1;
        while i < op_exprs.len() {
            if !ops.contains(&op_exprs[i].0) {
                i += 1;
                continue;
            }

            let (op, expr) = op_exprs.remove(i);
            op_exprs[i - 1].1 = Expr::BinOperator(
                expr.pos(),
                op,
                Box::new(op_exprs[i - 1].1.clone()),
                Box::new(expr)
            );
        }
    }

    // Last and least, assignments
    // We handle them differently since their priority is Right to Left
    loop {
        match op_exprs.pop() {
            Some((BinOp::Assign(post_op), rhs_expr)) => {
                let lhs_expr = match op_exprs.pop() {
                    Some((BinOp::Assign(_), prev)) => prev,
                    Some(_) => panic!(
                        "There should only be assignments at this point"),
                    None => return Ok(rhs_expr),
                };
                op_exprs.push((
                    BinOp::Assign(None),
                    join_assign_exprs(post_op, lhs_expr, rhs_expr)?
                ));
            },
            Some(_) => panic!("There should only be assignments at this point"),
            None    => panic!("Expect at least one element on the stack"),
        }
    }
}

fn join_assign_exprs(
    post_op: Option<Box<BinOp>>,
    lhs_expr: Expr,
    rhs_expr: Expr,
) -> Result<Expr, CompErr> {
    match lhs_expr {
        Expr::Id(pos, id) => {
            let rhs = match post_op {
                Some(post_op) => Expr::BinOperator(
                    pos.clone(),
                    *post_op,
                    Box::new(Expr::Id(pos.clone(), id.to_string())),
                    Box::new(rhs_expr)
                ),
                None => rhs_expr
            };
            Ok(Expr::Assignment(
                pos,
                id,
                Box::new(rhs)
            ))
        },
        Expr::Dereference(pos, lhs) => {
            let rhs = match post_op {
                Some(post_op) => Expr::BinOperator(
                    pos.clone(),
                    *post_op,
                    Box::new(Expr::Dereference(pos.clone(), lhs.clone())),
                    Box::new(rhs_expr)
                ),
                None => rhs_expr
            };
            Ok(Expr::DerefAssignment(
                pos,
                lhs,
                Box::new(rhs)
            ))
        },
        _ => CompErr::err(
            &lhs_expr.pos(),
            "lhs of assignment must be ID or deref".to_string()),
    }
}

fn parse_op(
    c: &mut ParseContext
) -> Result<Option<BinOp>, CompErr> {
    let (pos, tok) = pop_tok(c)?;
    Ok(match tok {
        Token::EqEq         => Some(BinOp::Eq),
        Token::Eq           => Some(BinOp::Assign(None)),
        Token::EqShiftRight => Some(BinOp::assign(BinOp::ShiftRight)),
        Token::EqGe         => Some(BinOp::assign(BinOp::Ge)),
        Token::EqShiftLeft  => Some(BinOp::assign(BinOp::ShiftLeft)),
        Token::EqLe         => Some(BinOp::assign(BinOp::Le)),
        Token::EqNe         => Some(BinOp::assign(BinOp::Ne)),
        Token::EqEqEq       => Some(BinOp::assign(BinOp::Eq)),
        Token::EqPlus       => Some(BinOp::assign(BinOp::Add)),
        Token::EqMinus      => Some(BinOp::assign(BinOp::Sub)),
        Token::EqLt         => Some(BinOp::assign(BinOp::Lt)),
        Token::EqGt         => Some(BinOp::assign(BinOp::Gt)),
        Token::EqAmpersand  => Some(BinOp::assign(BinOp::And)),
        Token::EqPipe       => Some(BinOp::assign(BinOp::Or)),
        Token::EqCaret      => Some(BinOp::assign(BinOp::Xor)),
        Token::EqPercent    => Some(BinOp::assign(BinOp::Mod)),
        Token::EqSlash      => Some(BinOp::assign(BinOp::Div)),
        Token::EqAsterisk   => Some(BinOp::assign(BinOp::Mul)),
        Token::Plus         => Some(BinOp::Add),
        Token::Minus        => Some(BinOp::Sub),
        Token::Le           => Some(BinOp::Le),
        Token::Lt           => Some(BinOp::Lt),
        Token::Ge           => Some(BinOp::Ge),
        Token::Gt           => Some(BinOp::Gt),
        Token::Ne           => Some(BinOp::Ne),
        Token::ShiftLeft    => Some(BinOp::ShiftLeft),
        Token::ShiftRight   => Some(BinOp::ShiftRight),
        Token::Ampersand    => Some(BinOp::And),
        Token::Pipe         => Some(BinOp::Or),
        Token::Caret        => Some(BinOp::Xor),
        Token::Percent      => Some(BinOp::Mod),
        Token::Slash        => Some(BinOp::Div),
        Token::Asterisk     => Some(BinOp::Mul),
        tok => {
            // The next token isn't a chaining token... Rewind!
            push_tok(c, (pos, tok));
            None
        },
    })
}

fn parse_op_exprs(
    c: &mut ParseContext
) -> Result<Vec<(BinOp, Expr)>, CompErr> {
    let mut op_exprs = vec!();
    loop {
        let op = match parse_op(c)? {
            Some(op) => op,
            None     => break,
        };
        op_exprs.push((op, parse_expr_unchained(c)?));
    }
    Ok(op_exprs)
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
                Box::new(Expr::BinOperator(
                    pos.clone(),
                    BinOp::Add,
                    Box::new(Expr::Id(pos.clone(), id)),
                    // Multiply by 8 (aka left shift by 3)
                    Box::new(Expr::BinOperator(
                        pos.clone(),
                        BinOp::ShiftLeft,
                        Box::new(index_expr),
                        Box::new(Expr::Int(pos, 3))
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
        Token::Int(value) => Ok(Expr::Int(pos, value)),
        Token::Char(chars) => Ok(Expr::Int(pos, pack_chars(&chars)[0])),
        Token::Str(value) => {
            c.strings.push(value);
            Ok(Expr::Str(pos, (c.file_id, c.strings.len() - 1)))
        },
        Token::Ampersand => match pop_tok(c)? {
            (pos, Token::Id(id)) => Ok(Expr::Reference(pos, id)),
            (pos, tok) => CompErr::err(&pos, format!("Expected id, found {:?}", tok)),
        },
        Token::Asterisk => Ok(Expr::Dereference(
            pos, Box::new(parse_expr_unchained(c)?))),
        Token::PlusPlus => Ok(Expr::UnaryOperator(
            pos, UnaryOp::Increment, Box::new(parse_expr_unchained(c)?))),
        Token::MinusMinus => Ok(Expr::UnaryOperator(
            pos, UnaryOp::Decrement, Box::new(parse_expr_unchained(c)?))),
        Token::Minus => Ok(Expr::UnaryOperator(
            pos, UnaryOp::Negate, Box::new(parse_expr_unchained(c)?))),
        Token::Tilde => Ok(Expr::UnaryOperator(
            pos, UnaryOp::BitNot, Box::new(parse_expr_unchained(c)?))),
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

fn parse_content(
    file_id: usize, content: &String
) -> Result<(Vec<RootStatement>, Vec<Vec<char>>), CompErr> {
    let mut c = ParseContext {
        content: content.chars().collect(),
        offset: 0,
        file_id: file_id,
        strings: vec!(),
        tok_stack: vec!(),
    };

    let mut roots = vec!();
    loop {
        match parse_root_statement(&mut c)? {
            Some(statement) => roots.push(statement),
            None            => break,
        }
    }
    Ok((roots, c.strings))
}

fn as_relative_path(base: &PathBuf, imp: &PathBuf) -> PathBuf {
    if imp.is_absolute() {
        imp.clone()
    } else {
        match base.parent() {
            Some(parent) => parent.join(imp).canonicalize().unwrap(),
            None         => imp.clone(),
        }
    }
}

pub fn parse_files(paths: &Vec<String>) -> ParseResult {
    let mut result = ParseResult {
        file_contents: vec!(),
        root_statements: vec!(),
        strings: vec!(),
        error: None,
    };

    let mut parsed_set: HashSet<PathBuf> = HashSet::new();
    let mut parse_stack: Vec<PathBuf> = vec!();
    for path in paths {
        parse_stack.push(
            Path::new(path).canonicalize()
                .expect(format!("Invalid path: {}", path).as_str()));
    }

    let mut file_id = 0;
    while !parse_stack.is_empty() {
        let path = parse_stack.pop().unwrap();
        if parsed_set.contains(&path) {
            continue;
        }
        parsed_set.insert(path.clone());

        let path_str = path.to_str().unwrap().to_string();

        let content = std::fs::read_to_string(&path)
            .expect(format!("Failed reading {}", path_str).as_str());

        match parse_content(file_id, &content) {
            Ok((statements, strings)) => {
                for s in statements {
                    match s {
                        RootStatement::Import(_, imp) => {
                            let imp_path = Path::new(&imp).to_path_buf();
                            parse_stack.push(as_relative_path(&path, &imp_path));
                        },
                        s => result.root_statements.push(s)
                    };
                }

                result.file_contents.push((path_str, content));
                result.strings.push((file_id, strings));
            },
            Err(error) => {
                result.error = Some(error);
                result.file_contents.push((path_str, content));
                break;
            },
        }
        file_id += 1;
    }

    result
}
