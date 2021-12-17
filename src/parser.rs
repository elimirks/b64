use std::sync::Condvar;
use crate::util::logical_cpu_count;
use crate::ast::*;
use crate::tokenizer::*;

use std::collections::HashSet;
use std::ops::DerefMut;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::thread;

pub struct ParseResult {
    // Stored in order of file_id (in Pos)
    // Stores (file_name, file_contents)
    pub file_contents: Vec<(String, String)>,
    // Stores (file_id, strings_vec)
    pub strings: Vec<(usize, Vec<Vec<char>>)>,
    pub functions: Vec<RSFunction>,
    pub variables: Vec<RSVariable>,
    pub errors: Vec<CompErr>,
}

impl ParseResult {
    fn new() -> ParseResult {
        ParseResult {
            file_contents: vec!(),
            strings: vec!(),
            functions: vec!(),
            variables: vec!(),
            errors: vec!(),
        }
    }
}

struct ParseState {
    result: ParseResult,
    // Files waiting to be parsed
    parse_stack: Vec<PathBuf>,
    parsed_set: HashSet<PathBuf>,
    // Track number of running parsers so we know when to stop
    running_parsers: u16,
    file_id: usize,
}

impl ParseState {
    fn new() -> ParseState {
        ParseState {
            result: ParseResult::new(),
            parse_stack: vec!(),
            parsed_set: HashSet::new(),
            running_parsers: 0,
            file_id: 0,
        }
    }

    fn push_path_to_parse(&mut self, path: PathBuf) {
        if !self.parsed_set.contains(&path) {
            self.parsed_set.insert(path.clone());
            self.parse_stack.push(path);
        }
    }

    // Increments running parsers if result is nonempty
    // Returns (file_id, path)
    fn pop_path_to_parse(&mut self) -> Option<(usize, PathBuf)> {
        self.parse_stack.pop().map(|path| {
            self.running_parsers += 1;
            self.file_id += 1;
            (self.file_id, path)
        })
    }
}

pub struct ParseContext<'a> {
    pub content: &'a [u8],
    // Offset for use by the tokenizer
    pub offset: usize,
    pub file_id: usize,
    // Tracks the "floating" string literals that aren't assigned to vectors
    pub strings: Vec<Vec<char>>,
    // Used for the tokenizer stack
    pub tok_stack: Vec<(Pos, Token)>,
}

impl ParseContext<'_> {
    pub fn peek_char(&self) -> Option<char> {
        if self.offset < self.content.len() {
            Some(self.content[self.offset] as char)
        } else {
            None
        }
    }

    pub fn pos(&self) -> Pos {
        Pos::new(self.offset, self.file_id)
    }
}

fn parse_root_var(
    c: &mut ParseContext, name: String
) -> Result<RSVariable, CompErr> {
    let pos = c.pos();
    let var = parse_var_entry(c, name)?;
    parse_tok(c, Token::Semicolon)?;
    Ok(RSVariable{
        pos: pos,
        var: var,
    })
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
) -> Result<RSImport, CompErr> {
    match pop_tok(c)? {
        (pos, Token::Str(chars)) => {
            let path: String = chars.into_iter().collect();
            parse_tok(c, Token::Semicolon)?;
            Ok(RSImport {
                pos: pos,
                path: path,
            })
        },
        (pos, tok) => CompErr::err(&pos, format!(
            "String expected. {:?} given", tok)),
    }
}

// Parses everything after the name of a function
fn parse_fun(
    c: &mut ParseContext, pos: Pos, name: String
) -> Result<RSFunction, CompErr> {
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
    Ok(RSFunction {
        pos: pos,
        name: name,
        args: args,
        body: parse_statement(c)?,
    })
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
    parse_expr_prec(c, 0)
}

const COND_EXPR_PRECEDENCE: u8 = 2;
fn get_lr_op_precedence(op: &BinOp) -> u8 {
    match op {
        BinOp::Div       | BinOp::Mod | BinOp::Mul            => 10,
        BinOp::Add       | BinOp::Sub                         => 9,
        BinOp::ShiftLeft | BinOp::ShiftRight                  => 8,
        BinOp::Gt        | BinOp::Lt  | BinOp::Ge | BinOp::Le => 7,
        BinOp::Eq        | BinOp::Ne                          => 6,
        BinOp::And                                            => 5,
        BinOp::Xor                                            => 4,
        BinOp::Or                                             => 3,
        BinOp::Assign(_)                                      => 1,
    }
}

/*
 * Tries parsing operators until the precedence value doesn't meet requirement.
 * In other words, it recurses, but doesn't consume lower priority ops.
 */
fn parse_expr_prec(
    c: &mut ParseContext, precedence: u8
) -> Result<Expr, CompErr> {
    let mut expr = parse_expr_unchained(c)?;
    loop {
        match parse_op(c)? {
            (pos, tok, Some(binop)) => {
                let next_precedence = get_lr_op_precedence(&binop);
                if next_precedence >= precedence {
                    let rhs_expr = parse_expr_prec(c, next_precedence)?;
                    expr = join_exprs(binop, expr, rhs_expr)?;
                } else {
                    push_tok(c, (pos, tok));
                    return Ok(expr);
                }
            },
            (pos, Token::Question, _) if COND_EXPR_PRECEDENCE >= precedence => {
                let true_expr = parse_expr_prec(c, COND_EXPR_PRECEDENCE)?;
                parse_tok(c, Token::Colon)?;
                let false_expr = parse_expr_prec(c, COND_EXPR_PRECEDENCE)?;

                expr = Expr::Cond(
                    pos,
                    Box::new(expr),
                    Box::new(true_expr),
                    Box::new(false_expr)
                )
            },
            (pos, tok, _) => {
                push_tok(c, (pos, tok));
                return Ok(expr)
            },
        }
    }
}

fn join_assignment(
    post_op: Option<Box::<BinOp>>, lhs_expr: Expr, rhs_expr: Expr
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

fn join_exprs(
    op: BinOp, lhs: Expr, rhs: Expr
) -> Result<Expr, CompErr> {
    if let BinOp::Assign(post_op) = op {
        join_assignment(post_op, lhs, rhs)
    } else {
        Ok(Expr::BinOperator(
            lhs.pos(),
            op,
            Box::new(lhs),
            Box::new(rhs)
        ))
    }
}

// Always returns the next token
// Optionally returns a binop if appropriate
fn parse_op(
    c: &mut ParseContext
) -> Result<(Pos, Token, Option<BinOp>), CompErr> {
    let (pos, tok) = pop_tok(c)?;

    let binop = match tok {
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
        _                   => None,
    };
    Ok((pos, tok, binop))
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

pub fn print_comp_error(file_contents: &Vec<(String, String)>, err: &CompErr) {
    println!("Compile error: {}", err.message);
    match &err.pos {
        Some(pos) => {
            let (file_name, content) = &file_contents[pos.file_id];
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
) -> Result<(RootStatements, Vec<Vec<char>>), CompErr> {
    // TODO: Validate `content` is valid ASCII first
    let mut c = ParseContext {
        content: content.as_bytes(),
        offset: 0,
        file_id: file_id,
        strings: vec!(),
        tok_stack: vec!(),
    };

    let mut root_statements = RootStatements::new();
    loop {
        match pop_tok(&mut c)? {
            (_, Token::Id(id)) => {
                let (pos, tok) = pop_tok(&mut c)?;

                match tok {
                    Token::LParen => {
                        push_tok(&mut c, (pos.clone(), Token::LParen));
                        root_statements.functions.push(
                            parse_fun(&mut c, pos, id)?);
                    },
                    tok => {
                        push_tok(&mut c, (pos, tok));
                        root_statements.variables.push(
                            parse_root_var(&mut c, id)?);
                    },
                }
            },
            (_, Token::Import) =>
                root_statements.imports.push(parse_import(&mut c)?),
            (_, Token::Eof) => break,
            (pos, tok) => return CompErr::err(&pos, format!(
                "Expected id. {:?} found", tok)),
        }
    }
    Ok((root_statements, c.strings))
}

fn relative_to_canonical_path(base: &PathBuf, imp: &PathBuf) -> PathBuf {
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
    let mut parse_state = ParseState::new();
    for path in paths {
        parse_state.push_path_to_parse(
            Path::new(path).canonicalize()
                .expect(format!("Invalid path: {}", path).as_str()));
    }

    let parse_state_arc = Arc::new((
        Mutex::new(parse_state),
        Condvar::new()
    ));

    let thread_count = logical_cpu_count();
    let mut handles = Vec::with_capacity(thread_count);
    for _ in 0..thread_count {
        let th_parse_state_arc = parse_state_arc.clone();

        handles.push(thread::spawn(move || {
            parse_fiber(th_parse_state_arc);
        }))
    }
    for th in handles {
        th.join().unwrap();
    }

    let (mutex, _) = &*parse_state_arc;

    // Nasty hack becuase you can't move out of a mutex
    let mut final_result = ParseResult::new();
    std::mem::swap(
        &mut mutex.lock().unwrap().deref_mut().result,
        &mut final_result
    );
    final_result
}

fn parse_fiber(
    parse_state: Arc<(Mutex<ParseState>, Condvar)>
) {
    loop {
        match unpool_file_path(&parse_state) {
            Some((file_id, path_buf)) =>
                parse_file(file_id, path_buf, &parse_state),
            None => break,
        }
    }
}

fn unpool_file_path(
    parse_state: &Arc<(Mutex<ParseState>, Condvar)>
) -> Option<(usize, PathBuf)> {
    let (mutex, cvar) = parse_state.as_ref();
    let mut guard = mutex.lock().unwrap();

    loop {
        // There is nothing left to do!
        if guard.running_parsers == 0 && guard.parse_stack.is_empty() {
            return None;
        }
        match guard.pop_path_to_parse() {
            res @ Some(_) => return res,
            None => {},
        }
        guard = cvar.wait(guard).unwrap();
    }
}

// Parse a file, add to the parse state, and notify the cvar
fn parse_file(
    file_id: usize,
    path: PathBuf,
    parse_state: &Arc<(Mutex<ParseState>, Condvar)>
) {
    let path_str = path.to_str().unwrap().to_string();
    let content = std::fs::read_to_string(&path)
        .expect(format!("Failed reading {}", path_str).as_str());

    let (mutex, cvar) = parse_state.as_ref();

    let parsed_content = parse_content(file_id, &content);
    let mut guard = mutex.lock().unwrap();

    match parsed_content {
        Ok((mut statements, strings)) => {
            for import in statements.imports {
                let imp = import.path;
                let imp_path = Path::new(&imp).to_path_buf();
                guard.push_path_to_parse(relative_to_canonical_path(&path, &imp_path));
            }
            guard.result.functions.append(&mut statements.functions);
            guard.result.variables.append(&mut statements.variables);
            guard.result.file_contents.push((path_str, content));
            guard.result.strings.push((file_id, strings));
        },
        Err(error) => {
            guard.result.errors.push(error);
            guard.result.file_contents.push((path_str, content));
        },
    }
    guard.running_parsers -= 1;
    cvar.notify_all();
}
