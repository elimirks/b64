use crate::parser::ParseContext;

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
pub fn parse_tok(c: &mut ParseContext, expected: Token) -> bool {
    match pop_tok(c) {
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
pub fn pop_tok(c: &mut ParseContext) -> Option<Token> {
    if !c.next_tok.is_none() {
        return std::mem::replace(&mut c.next_tok, None);
    }

    // Seek past useless whitespace
    consume_ws(c);

    match c.peek_char() {
        Some(ch) => {
            if ch.is_alphabetic() {
                get_tok_word(c)
            } else if ch.is_numeric() {
                get_tok_int(c)
            } else if ch == '\'' {
                get_tok_char(c)
            } else {
                get_tok_symbol(c)
            }
        },
        None => Some(Token::Eof),
    }
}

pub fn push_tok(c: &mut ParseContext, tok: Token) {
    if !c.next_tok.is_none() {
        // This is fine. We shouldn't ever have to push more than one token
        // If more than one gets pushed, the parser is doing something silly
        panic!("Token stack is full");
    }
    c.next_tok = Some(tok);
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
        '[' => Some(Token::LBracket),
        ']' => Some(Token::RBracket),
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
    let current_word = alphanumeric_slice(&c.content, c.offset);
    let str_word: String = current_word.into_iter().collect();

    match str_word.parse::<i64>() {
        Ok(num) => {
            c.offset += current_word.len();
            Some(Token::Value(num))
        },
        _ => {
            c.error = Some(format!("Invalid int literal: {}", str_word));
            None
        },
    }
}

/**
 * Tokenizes a char literal into a value
 * Expects opening quote character to have been peeked at already
 */
fn get_tok_char(c: &mut ParseContext) -> Option<Token> {
    // Start at the character right after the starting quote
    let mut i = c.offset + 1;
    // Index of the char literal, NOT of the content
    let mut index = 0;
    let mut value: i64 = 0;

    while i < c.content.len() && c.content[i] != '\'' {
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
                    other => {
                        c.error = Some(format!("Unknown escape char: {}", other));
                        return None;
                    },
                }
            },
            chr => {
                let ichar = chr as i64;

                if ichar >= 256 || ichar < 0 {
                    c.error = Some("b64 only supports ASCII chars".to_string());
                    return None;
                }

                ichar
            },
        };

        value += ichar << (index * 8);

        index += 1;
        i += 1;
    }

    if i >= c.content.len() {
        c.error = Some("Hit EOF while parsing char".to_string());
        None
    } else {
        c.offset = i + 1;
        Some(Token::Value(value))
    }
}

// Parsed word-like tokens. Includes keywords and IDs
fn get_tok_word(c: &mut ParseContext) -> Option<Token> {
    let current_word = alphanumeric_slice(&c.content, c.offset);
    c.offset += current_word.len();
    let str_word: String = current_word.into_iter().collect();

    let tok = match str_word.as_str() {
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

    Some(tok)
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
