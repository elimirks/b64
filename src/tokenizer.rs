use std::str;
use crate::parser::ParseContext;

#[derive(Debug, PartialEq)]
pub enum Token {
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
pub fn parse_tok(c: &mut ParseContext, expected: Token) -> bool {
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

// Returns None for invalid tokens
// Returns Token::Eof for Eof (considered a valid token)
pub fn get_tok(c: &mut ParseContext) -> Option<Token> {
    // Seek past useless whitespace
    consume_ws(c);

    match c.peek_char() {
        Some(ch) => {
            if ch.is_alphabetic() {
                get_tok_word(c)
            } else if ch.is_numeric() {
                get_tok_int(c)
            } else {
                get_tok_symbol(c)
            }
        },
        None => Some(Token::Eof),
    }
}

pub fn peek_tok(c: &mut ParseContext) -> Option<Token> {
    consume_ws(c); // It's ok to just jump back to right after the whitespace
    let initial = c.offset;
    let tok = get_tok(c);
    c.offset = initial;
    tok
}

// Generates a symbol tokenizer match statemnt for ambiguous multi-char tokens
macro_rules! multi_tok {
    ($context:expr, $default:expr, $($extra:expr, $token:expr),*) => {
        match $context.peek_char() {
            $(
                Some($extra) => {
                    $context.offset += 1;
                    Some($token)
                },
            )*
            _ => Some($default),
        }
    };
}

// Assumes c.content[c.offset] is in bounds
fn get_tok_symbol(c: &mut ParseContext) -> Option<Token> {
    c.offset += 1;
    match c.content[c.offset - 1] as char {
        '+' => multi_tok!(c, Token::Plus,
                          '+', Token::PlusPlus),
        '-' => multi_tok!(c, Token::Minus,
                          '-', Token::MinusMinus),
        '=' => multi_tok!(c, Token::Eq,
                          '=', Token::EqEq),
        '>' => multi_tok!(c, Token::Gt,
                          '>', Token::ShiftRight,
                          '=', Token::Ge),
        '<' => multi_tok!(c, Token::Lt,
                          '<', Token::ShiftLeft,
                          '=', Token::Le),
        '!' => multi_tok!(c, Token::Bang,
                          '=', Token::Ne),
        '(' => Some(Token::LParen),
        ')' => Some(Token::RParen),
        '{' => Some(Token::LBrace),
        '}' => Some(Token::RBrace),
        ';' => Some(Token::Semicolon),
        ':' => Some(Token::Colon),
        '?' => Some(Token::Question),
        ',' => Some(Token::Comma),
        '*' => Some(Token::Asterisk),
        '&' => Some(Token::Ampersand),
        '~' => Some(Token::Tilde),
        '/' => Some(Token::Slash),
        '%' => Some(Token::Percent),
        other => {
            c.offset -= 1;
            c.error = Some(format!("Invalid token: {}", other));
            None
        }
    }
}

fn get_tok_int(c: &mut ParseContext) -> Option<Token> {
    let current_word = alphanumeric_slice(c.content, c.offset);
    let str_word = str::from_utf8(current_word)
        .expect("Invalid UTF8 found in file");

    match str_word.parse::<i64>() {
        Ok(num) => {
            c.offset += current_word.len();
            Some(Token::Int(num))
        },
        _ => {
            c.error = Some(format!("Invalid int literal: {}", str_word));
            None
        },
    }
}

// Parsed word-like tokens. Includes keywords and IDs
fn get_tok_word(c: &mut ParseContext) -> Option<Token> {
    let current_word = alphanumeric_slice(c.content, c.offset);
    c.offset += current_word.len();

    let str_word = str::from_utf8(current_word)
        .expect("Invalid UTF8 found in file");

    let tok = match str_word {
        "return" => Token::Return,
        "auto"   => Token::Auto,
        "extern" => Token::Extern,
        "eof"    => Token::Eof,
        "while"  => Token::While,
        "if"     => Token::If,
        "else"   => Token::Else,
        "goto"   => Token::Goto,
        "switch" => Token::Switch,
        "break"  => Token::Break,
        _        => Token::Id(str_word.to_string()),
    };

    Some(tok)
}

/**
 * Extract an alphanumeric slice at the given offset
 * @return An empty slice if the offset is out of bounds,
 *         or if there are no alphanumeric characters at that position
 */
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
