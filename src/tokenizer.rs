use crate::parser::ParseContext;
use crate::ast::{Pos, CompErr};

#[derive(Debug, PartialEq)]
pub enum Token {
    Id(String),
    Label(String),
    Value(i64),
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
    LBracket,
    RBracket,
    Semicolon,
    Question,
    Colon,
    Comma,
    Plus,
    Minus,
    PlusPlus,
    MinusMinus,
    Asterisk,
    Ampersand,
    Bang,
    Tilde,
    Slash,
    Percent,
    ShiftRight,
    ShiftLeft,
    Gt,
    Lt,
    Le,
    Ge,
    Ne,
    Eq,
    EqEq,
}

// Returns false if it failed to parse the given token
pub fn parse_tok(c: &mut ParseContext, expected: Token) -> Result<(), CompErr> {
    let (pos, recieved) = pop_tok(c)?;
    if expected == recieved {
        Ok(())
    } else {
        CompErr::err(&pos, format!(
            "Expected {:?}, but {:?} was found", expected, recieved))
    }
}

// Returns None for invalid tokens
// Returns Token::Eof for Eof (considered a valid token)
pub fn pop_tok(c: &mut ParseContext) -> Result<(Pos, Token), CompErr> {
    if !c.next_tok.is_none() {
        let mut temp = None;
        std::mem::swap(&mut temp, &mut c.next_tok);
        return Ok(temp.unwrap());
    }

    // Seek past useless whitespace
    consume_ws(c);

    match c.peek_char() {
        Some(ch) => {
            if ch.is_alphabetic() {
                Ok(get_tok_word(c))
            } else if ch.is_numeric() {
                get_tok_int(c)
            } else if ch == '\'' {
                get_tok_char(c)
            } else {
                get_tok_symbol(c)
            }
        },
        None => Ok((c.pos(), Token::Eof)),
    }
}

pub fn push_tok(c: &mut ParseContext, tok: (Pos, Token)) {
    if !c.next_tok.is_none() {
        // This is fine. We shouldn't ever have to push more than one token
        // If more than one gets pushed, the parser is doing something silly
        panic!("Token stack is full");
    }
    c.next_tok = Some(tok);
}

// Generates a symbol tokenizer match statemnt for ambiguous multi-char tokens
macro_rules! multi_tok {
    ($context:expr, $pos:expr, $default:expr, $($extra:expr, $token:expr),*) => {
        match $context.peek_char() {
            $(
                Some($extra) => {
                    $context.offset += 1;
                    Ok(($pos, $token))
                },
            )*
            _ => Ok(($pos, $default)),
        }
    };
}

// Assumes c.content[c.offset] is in bounds
fn get_tok_symbol(c: &mut ParseContext) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    c.offset += 1;
    match c.content[c.offset - 1] as char {
        '+' => multi_tok!(c, pos, Token::Plus,
                          '+', Token::PlusPlus),
        '-' => multi_tok!(c, pos, Token::Minus,
                          '-', Token::MinusMinus),
        '=' => multi_tok!(c, pos, Token::Eq,
                          '=', Token::EqEq),
        '>' => multi_tok!(c, pos, Token::Gt,
                          '>', Token::ShiftRight,
                          '=', Token::Ge),
        '<' => multi_tok!(c, pos, Token::Lt,
                          '<', Token::ShiftLeft,
                          '=', Token::Le),
        '!' => multi_tok!(c, pos, Token::Bang,
                          '=', Token::Ne),
        '(' => Ok((pos, Token::LParen)),
        ')' => Ok((pos, Token::RParen)),
        '{' => Ok((pos, Token::LBrace)),
        '}' => Ok((pos, Token::RBrace)),
        '[' => Ok((pos, Token::LBracket)),
        ']' => Ok((pos, Token::RBracket)),
        ';' => Ok((pos, Token::Semicolon)),
        ':' => Ok((pos, Token::Colon)),
        '?' => Ok((pos, Token::Question)),
        ',' => Ok((pos, Token::Comma)),
        '*' => Ok((pos, Token::Asterisk)),
        '&' => Ok((pos, Token::Ampersand)),
        '~' => Ok((pos, Token::Tilde)),
        '/' => Ok((pos, Token::Slash)),
        '%' => Ok((pos, Token::Percent)),
        other => {
            c.offset -= 1;
            CompErr::err(&pos, format!("Invalid token: {}", other))
        }
    }
}

fn get_tok_int(c: &mut ParseContext) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    let current_word = alphanumeric_slice(&c.content, c.offset);
    let str_word: String = current_word.into_iter().collect();

    match str_word.parse::<i64>() {
        Ok(num) => {
            c.offset += current_word.len();
            Ok((pos, Token::Value(num)))
        },
        _ => CompErr::err(&pos, format!(
            "Invalid int literal: {}", str_word)),
    }
}

/**
 * Tokenizes a char literal into a value
 * Expects opening quote character to have been peeked at already
 */
fn get_tok_char(c: &mut ParseContext) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    // Start at the character right after the starting quote
    let mut i = c.offset + 1;
    // Index of the char literal, NOT of the content
    let mut index = 0;
    let mut value: i64 = 0;

    while i < c.content.len() && c.content[i] != '\'' {
        if index >= 8 {
            return CompErr::err(
                &pos, "A char can be at most 8 bytes".to_string());
        }

        let ichar = match c.content[i] {
            '*' => {
                i += 1;
                // Hit EOF while parsing char
                if i >= c.content.len() {
                    break;
                }

                match c.content[i] {
                    '*'  => '*' as i64,
                    'n'  => '\n' as i64,
                    '0'  => 0,
                    't'  => '\t' as i64,
                    '\'' => '\'' as i64,
                    '\"' => '\"' as i64,
                    other => return CompErr::err(&pos, format!(
                        "Unknown escape char: {}", other)),
                }
            },
            chr => {
                let ichar = chr as i64;

                if ichar >= 256 || ichar < 0 {
                    return CompErr::err(
                        &pos,
                        "b64 only supports ASCII chars".to_string());
                }

                ichar
            },
        };

        value += ichar << (index * 8);

        index += 1;
        i += 1;
    }

    if i >= c.content.len() {
        CompErr::err(&pos, "Hit EOF while parsing char".to_string())
    } else {
        c.offset = i + 1;
        Ok((pos, Token::Value(value)))
    }
}

// Parsed word-like tokens. Includes keywords and IDs
fn get_tok_word(c: &mut ParseContext) -> (Pos, Token) {
    let pos = c.pos();
    let current_word = alphanumeric_slice(&c.content, c.offset);
    c.offset += current_word.len();
    let str_word: String = current_word.into_iter().collect();

    let tok = match str_word.as_str() {
        "return" => Token::Return,
        "auto"   => Token::Auto,
        "extrn"  => Token::Extern,
        "eof"    => Token::Eof,
        "while"  => Token::While,
        "if"     => Token::If,
        "else"   => Token::Else,
        "goto"   => Token::Goto,
        "switch" => Token::Switch,
        "break"  => Token::Break,
        _        => {
            let name = str_word.to_string();

            match c.peek_char() {
                Some(':') => {
                    c.offset += 1;
                    Token::Label(name)
                },
                _ => Token::Id(name),
            }
        },
    };

    (pos, tok)
}

/**
 * Extract an alphanumeric slice at the given offset
 * @return An empty slice if the offset is out of bounds,
 *         or if there are no alphanumeric characters at that position
 */
fn alphanumeric_slice(slice: &[char], offset: usize) -> &[char] {
    let mut len = 0;
    while offset + len < slice.len() {
        if slice[offset + len].is_alphanumeric() {
            len += 1;
        } else {
            break;
        }
    }
    &slice[offset..offset + len]
}

// Parse any amount of whitespace, including comments
fn consume_ws(c: &mut ParseContext) {
    while !c.at_eof() {
        match c.peek_char() {
            Some(' ')  => c.offset += 1,
            Some('\n') => c.offset += 1,
            Some('/')  => {
                if !consume_comment(c) {
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
fn consume_comment(c: &mut ParseContext) -> bool {
    if c.offset + 1 >= c.content.len() {
        return false;
    }
    if c.content[c.offset + 1] as char != '*' {
        return false;
    }
    c.offset += 2;

    let mut one;
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
