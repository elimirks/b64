use crate::parser::ParseContext;
use crate::ast::{Pos, CompErr};

#[derive(Debug, PartialEq)]
pub enum Token {
    Id(String),
    Label(String),
    Str(Vec<char>),
    Char(Vec<char>),
    Int(i64),
    Import,
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
    Caret,
    Pipe,
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
    // For the slew of different assignment operators
    Eq         , EqEq       , EqEqEq      ,
    EqPlus     , EqMinus    , EqLe        ,
    EqLt       , EqGe       , EqGt        ,
    EqNe       , EqShiftLeft, EqShiftRight,
    EqAmpersand, EqPipe     , EqCaret     ,
    EqPercent  , EqSlash    , EqAsterisk  ,
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
    match c.tok_stack.pop() {
        None => {},
        Some(next) => return Ok(next),
    };

    // Seek past useless whitespace
    consume_ws(c);

    match c.peek_char() {
        Some('0') => {
            get_tok_int(c, 8)
        },
        Some(ch) => {
            if ch == '_' || ch.is_alphabetic() {
                Ok(get_tok_word(c))
            } else if ch.is_numeric() {
                get_tok_int(c, 10)
            } else if ch == '\'' {
                get_tok_char(c)
            } else if ch == '\"' {
                get_tok_str(c)
            } else if ch == '=' {
                // Handle '=' differently because of the chaining rule
                Ok(get_tok_equals(c))
            } else if ch == '@' {
                get_tok_meta(c)
            } else {
                get_tok_symbol(c)
            }
        },
        None => Ok((c.pos(), Token::Eof)),
    }
}

pub fn push_tok(c: &mut ParseContext, tok: (Pos, Token)) {
    c.tok_stack.push(tok);
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
        '|' => Ok((pos, Token::Pipe)),
        '^' => Ok((pos, Token::Caret)),
        '~' => Ok((pos, Token::Tilde)),
        '/' => Ok((pos, Token::Slash)),
        '%' => Ok((pos, Token::Percent)),
        other => {
            c.offset -= 1;
            CompErr::err(&pos, format!("Invalid token: {}", other))
        }
    }
}

// Assumes the @ token has been parsed
// Returns a metaprogramming token
fn get_tok_meta(c: &mut ParseContext) -> Result<(Pos, Token), CompErr> {
    let next_chars = alphanumeric_slice(&c.content, c.offset + 1);
    let next_word = std::str::from_utf8(next_chars).unwrap();
    let pos = c.pos();

    match next_word {
        "import" => {
            c.offset += 1 + next_chars.len();
            Ok((c.pos(), Token::Import))
        },
        other => {
            CompErr::err(&pos, format!("Invalid token: @{}", other))
        },
    }
}

// Assumes the character at the current point is =
fn get_tok_equals(c: &mut ParseContext) -> (Pos, Token) {
    // Peek at the next 2 chars
    let c1 = match c.content.get(c.offset + 1) {
        Some(value) => *value as char,
        None        => ' ',
    };
    let c2 = match c.content.get(c.offset + 2) {
        Some(value) => *value as char,
        None        => ' ',
    };

    let (len, tok) = match (c1, c2) {
        ('>', '>') => (3, Token::EqShiftRight),
        ('>', '=') => (3, Token::EqGe),
        ('<', '<') => (3, Token::EqShiftLeft),
        ('<', '=') => (3, Token::EqLe),
        ('!', '=') => (3, Token::EqNe),
        ('=', '=') => (3, Token::EqEqEq),
        ('=', _)   => (2, Token::EqEq),
        ('+', _)   => (2, Token::EqPlus),
        ('-', _)   => (2, Token::EqMinus),
        ('<', _)   => (2, Token::EqLt),
        ('>', _)   => (2, Token::EqGt),
        ('&', _)   => (2, Token::EqAmpersand),
        ('|', _)   => (2, Token::EqPipe),
        ('^', _)   => (2, Token::EqCaret),
        ('%', _)   => (2, Token::EqPercent),
        ('/', _)   => (2, Token::EqSlash),
        ('*', _)   => (2, Token::EqAsterisk),
        _          => (1, Token::Eq),
    };

    let pos = c.pos();
    c.offset += len;
    (pos, tok)
}

fn get_tok_int(
    c: &mut ParseContext, radix: u32
) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    let current_word = alphanumeric_slice(&c.content, c.offset);
    // TODO: No need to allocate a new string here. Reimplement from radix!
    let str_word: String = std::str::from_utf8(current_word).unwrap().to_string();

    match i64::from_str_radix(&str_word, radix) {
        Ok(num) => {
            c.offset += current_word.len();
            Ok((pos, Token::Int(num)))
        },
        _ => CompErr::err(&pos, format!(
            "Invalid int literal: {}", str_word)),
    }
}

fn get_tok_str(c: &mut ParseContext) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    c.offset += 1;
    let values = get_inside_quotes(c, '\"')?;
    c.offset += 1;
    Ok((pos, Token::Str(values)))
}

/**
 * Tokenizes a char literal into a value
 * Expects opening quote character to have been peeked at already
 */
fn get_tok_char(c: &mut ParseContext) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    c.offset += 1;
    let chars = get_inside_quotes(c, '\'')?;

    if chars.len() > 8 {
        CompErr::err(&pos, "A wide char may be at most 8 bytes".to_string())
    } else {
        c.offset += 1;
        Ok((pos, Token::Char(chars)))
    }
}

// Gets chars enclosed in the given terminal character
fn get_inside_quotes(
    c: &mut ParseContext, terminal: char
) -> Result<Vec<char>, CompErr> {
    let mut i = c.offset;
    let mut chars = vec!();

    while i < c.content.len() && c.content[i] as char != terminal {
        let chr = match c.content[i] as char {
            '*' => {
                i += 1;
                // Hit EOF while parsing char
                if i >= c.content.len() {
                    return CompErr::err(
                        &c.pos(), "Hit EOF while parsing char".to_string());
                }
                match c.content[i] as char {
                    '*'  => '*',
                    'n'  => '\n',
                    '0'  => '\0',
                    't'  => '\t',
                    '\'' => '\'',
                    '\"' => '\"',
                    // For compliance with the B manual
                    // These aren't ever necessary in code compiled with b64
                    '{'  => '{',
                    '}'  => '}',
                    other => return CompErr::err(&c.pos(), format!(
                        "Unknown escape char: {}", other)),
                }
            },
            chr => {
                let ichar = chr as i64;
                if ichar >= 256 || ichar < 0 {
                    return CompErr::err(
                        &c.pos(),
                        "b64 only supports ASCII chars".to_string());
                }
                chr
            },
        };
        i += 1;
        chars.push(chr);
    }

    c.offset = i;
    Ok(chars)
}

// Parsed word-like tokens. Includes keywords and IDs
fn get_tok_word(c: &mut ParseContext) -> (Pos, Token) {
    let pos = c.pos();
    let slice = alphanumeric_slice(&c.content, c.offset);
    c.offset += slice.len();

    // Safe to assume it's valid utf8 since we enforce ASCII
    let tok = match std::str::from_utf8(slice).unwrap() {
        "auto"   => Token::Auto,
        "break"  => Token::Break,
        "else"   => Token::Else,
        "extrn"  => Token::Extern,
        "goto"   => Token::Goto,
        "if"     => Token::If,
        "return" => Token::Return,
        "switch" => Token::Switch,
        "while"  => Token::While,
        word => {
            let name: String = word.to_string();

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
fn alphanumeric_slice(slice: &[u8], offset: usize) -> &[u8] {
    let mut len = 0;
    while offset + len < slice.len() {
        let c = slice[offset + len] as char;
        if c.is_alphanumeric() || c == '_' {
            len += 1;
        } else {
            break;
        }
    }
    &slice[offset..offset + len]
}

// Parse any amount of whitespace, including comments
fn consume_ws(c: &mut ParseContext) {
    while c.offset < c.content.len() {
        match c.content[c.offset] as char {
            ' '  => c.offset += 1,
            '\n' => c.offset += 1,
            '/' => if !consume_comment(c) {
                break
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
    } else if c.content[c.offset + 1] as char != '*' {
        return false;
    }
    c.offset += 2;

    let mut one;
    let mut two = 0;

    while c.offset < c.content.len() {
        one = two;
        two = c.content[c.offset];
        c.offset += 1;

        if one == '*' as u8 && two == '/' as u8 {
            break;
        }
    }
    true
}
