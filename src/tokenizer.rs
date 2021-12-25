use crate::parser::ParseContext;
use crate::ast::{Pos, CompErr};
use crate::util::lsb_number;

#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
use std::arch::x86_64::*;

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

    if c.offset >= c.content.len() {
        return Ok((c.pos(), Token::Eof));
    }

    let ch = unsafe { *c.content.get_unchecked(c.offset) };

    match ch as char {
        '\''                        => get_tok_char(c),
        '\"'                        => get_tok_str(c),
        '#'                         => get_tok_meta(c),
        // Handle '=' differently because of the chaining rule
        '='                         => Ok(get_tok_equals(c)),
        '_' | 'a'..='z' | 'A'..='Z' => get_tok_word(c),
        '1'..='9'                   => get_tok_int_decimal(c),
        '0'                         => get_tok_int_octal(c),
        _                           => get_tok_symbol(c),
    }
}

pub fn push_tok(c: &mut ParseContext, tok: (Pos, Token)) {
    c.tok_stack.push(tok);
}

// Generates a symbol tokenizer match statemnt for ambiguous multi-char tokens
macro_rules! multi_tok {
    ($context:expr, $pos:expr, $default:expr, $($extra:expr, $token:expr),*) => {
        if $context.offset >= $context.content.len() {
            Ok(($pos, $default))
        } else {
            let ch = unsafe {
                *$context.content.get_unchecked($context.offset)
            };

            match ch as char {
                $(
                    $extra => {
                        $context.offset += 1;
                        Ok(($pos, $token))
                    },
                )*
                _ => Ok(($pos, $default)),
            }
        }
    };
}

// Assumes c.content[c.offset] is in bounds
fn get_tok_symbol(c: &mut ParseContext) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    c.offset += 1;
    match unsafe { *c.content.get_unchecked(c.offset - 1) } as char {
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

// Assumes the # token has been parsed
// Returns a metaprogramming token
fn get_tok_meta(c: &mut ParseContext) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    let next_word = id_slice(&pos, &c.content, c.offset + 1)?;

    match next_word {
        "import" => {
            c.offset += 1 + next_word.len();
            Ok((c.pos(), Token::Import))
        },
        other => {
            CompErr::err(&pos, format!("Invalid token: #{}", other))
        },
    }
}

// Assumes the character at the current point is =
fn get_tok_equals(c: &mut ParseContext) -> (Pos, Token) {
    // Peek at the next 2 chars
    let (c1, c2) = unsafe {
        if c.offset + 2 < c.content.len() {
            (*c.content.get_unchecked(c.offset + 1),
             *c.content.get_unchecked(c.offset + 2))
        } else if c.offset + 1 < c.content.len() {
            (*c.content.get_unchecked(c.offset + 1), 0)
        } else {
            (0, 0)
        }
    };

    let (len, tok) = match (c1 as char, c2 as char) {
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

fn get_tok_int_octal(
    c: &mut ParseContext
) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    let current_word = id_slice(&pos, &c.content, c.offset)?;

    let mut value = 0;
    let mut significance = 1;

    for c in current_word.bytes().rev() {
        if c > '7' as u8 || c < '0' as u8 {
            return CompErr::err(&pos, format!(
                "Invalid int literal: {}", current_word));
        }
        let x = c as i64 - '0' as i64;

        if value > i64::MAX - x * significance {
            return CompErr::err(&pos, format!(
                "Invalid int literal: {}", current_word));
        }
        value += x * significance;

        significance *= 8;
    }
    c.offset += current_word.len();
    Ok((pos, Token::Int(value)))
}

fn get_tok_int_decimal(
    c: &mut ParseContext
) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    let current_word = id_slice(&pos, &c.content, c.offset)?;

    let mut value = 0;
    let mut significance = 1;

    for c in current_word.bytes().rev() {
        if c > '9' as u8 || c < '0' as u8 {
            return CompErr::err(&pos, format!(
                "Invalid int literal: {}", current_word));
        }
        let x = c as i64 - '0' as i64;

        if value > i64::MAX - x * significance {
            return CompErr::err(&pos, format!(
                "Invalid int literal: {}", current_word));
        }
        value += x * significance;

        significance *= 10;
    }
    c.offset += current_word.len();
    Ok((pos, Token::Int(value)))
}

fn get_tok_str(c: &mut ParseContext) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    c.offset += 1;
    let values = unsafe {
        get_inside_quotes(c, '\"')?
    };
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
    let chars = unsafe {
        get_inside_quotes(c, '\'')?
    };

    if chars.len() > 8 {
        CompErr::err(&pos, "A wide char may be at most 8 bytes".to_string())
    } else {
        c.offset += 1;
        Ok((pos, Token::Char(chars)))
    }
}

// Gets chars enclosed in the given terminal character
unsafe fn get_inside_quotes(
    c: &mut ParseContext, terminal: char
) -> Result<Vec<char>, CompErr> {
    let mut i = c.offset;
    let mut chars = vec!();

    while i < c.content.len() && *c.content.get_unchecked(i) as char != terminal {
        let chr = match *c.content.get_unchecked(i) as char {
            '*' => {
                i += 1;
                // Hit EOF while parsing char
                if i >= c.content.len() {
                    return CompErr::err(
                        &c.pos(), "Hit EOF while parsing char".to_string());
                }
                match *c.content.get_unchecked(i) as char {
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
fn get_tok_word(c: &mut ParseContext) -> Result<(Pos, Token), CompErr> {
    let pos = c.pos();
    let slice = id_slice(&pos, &c.content, c.offset)?;
    c.offset += slice.len();

    // Safe to assume it's valid utf8 since we enforce ASCII
    let tok = match slice {
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

            if c.offset >= c.content.len() {
                Token::Id(name)
            } else {
                let ch = unsafe {
                    *c.content.get_unchecked(c.offset)
                };

                if ch == ':' as u8 {
                    c.offset += 1;
                    Token::Label(name)
                } else {
                    Token::Id(name)
                }
            }
        },
    };

    Ok((pos, tok))
}

/**
 * Extract an alphanumeric (and underscore) slice at the given offset
 * @return An empty slice if the offset is out of bounds,
 *         or if there are no alphanumeric characters at that position
 */
fn id_slice<'a>(
    pos: &Pos, slice: &'a [u8], offset: usize
) -> Result<&'a str, CompErr> {
    let len = id_len(slice, offset);

    if len == usize::MAX {
        return CompErr::err(pos, "Only ASCII is supported".to_string());
    }

    unsafe {
        Ok(std::str::from_utf8_unchecked(
            slice.get_unchecked(offset..offset + len)))
    }
}

/// Returns usize::MAX if there are invalid ASCII characters
fn id_len(
    slice: &[u8], offset: usize
) -> usize {
    unsafe {
        #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
        return simd_id_len( slice, offset);

        #[cfg(not(all(target_arch = "x86_64", target_feature = "avx2")))]
        return non_simd_id_len(slice, offset);
    }
}

unsafe fn non_simd_id_len(
    slice: &[u8], offset: usize
) -> usize {
    let mut len = 0;

    while offset + len < slice.len() {
        let c = *slice.get_unchecked(offset + len);

        if is_alphanum_underscore(c) {
            len += 1;
        } else if c > 0b01111111 {
            return usize::MAX;
        } else {
            break;
        }
    }
    len
}

fn is_alphanum_underscore(c: u8) -> bool {
    (c >= 97 && c <= 122) | (c >= 65 && c <= 90) | (c >= 48 && c <= 57) | (c == 95)
}

#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
#[allow(overflowing_literals)]
unsafe fn simd_id_len(
    slice: &[u8], offset: usize
) -> usize {
    let mut tail_offset = offset;

    let ascii_mask = _mm_set1_epi8(0b01111111);
    let alpha_min_bound = _mm_set1_epi8('a' as i8 - 1);
    let alpha_max_bound = _mm_set1_epi8('z' as i8 + 1);
    // ORing a letter with 0x20 will convert it to lowercase
    // So we won't have to do another check for A-Z
    let to_lower_bit_vec = _mm_set1_epi8(0x20);

    let num_min_bound = _mm_set1_epi8('0' as i8);
    let num_max_bound = _mm_set1_epi8('9' as i8);
    let underscore_vec = _mm_set1_epi8('_' as i8);

    while tail_offset + 16 < slice.len() {
        let mut values = _mm_loadu_si128(
            slice.get_unchecked(tail_offset) as *const u8 as *const _);

        let only_ascii = _mm_movemask_epi8(_mm_cmpgt_epi8(values, ascii_mask));
        if only_ascii != 0 {
            return usize::MAX;
        }

        /* Operations are inverted so we end up with a bitmask of
         * which characters are NOT alphanumeric / underscore
         */

        // Underscore & number check
        let mut result = _mm_andnot_si128(
            _mm_cmpeq_epi8(values, underscore_vec),
            _mm_or_si128(
                _mm_cmpgt_epi8(values, num_max_bound),
                _mm_cmpgt_epi8(num_min_bound, values),
            )
        );

        // Convert to lowercase
        values = _mm_or_si128(to_lower_bit_vec, values);

        // Alpha check
        result = _mm_andnot_si128(
            _mm_and_si128(
                _mm_cmpgt_epi8(values, alpha_min_bound),
                _mm_cmpgt_epi8(alpha_max_bound, values),
            ),
            result
        );

        // Compute bitmask of which values are 255
        // Mask is zeros going from from right to left
        let mask = _mm_movemask_epi8(result) as u32;

        // 16 valid characters
        if mask == 0 {
            tail_offset += 16;
        } else {
            let lsb = lsb_number(mask);
            return (tail_offset + lsb as usize) - offset;
        }
    }
    // Fallback to non-SIMD
    let eof_len = non_simd_id_len(slice, tail_offset);
    if eof_len == usize::MAX {
        eof_len
    } else {
        eof_len + (tail_offset - offset)
    }
}

#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
#[allow(overflowing_literals)]
unsafe fn simd_consume_ws(c: &mut ParseContext) {
    let space_vec = _mm_set1_epi8(' ' as i8);
    // Hack to reduce number of ops to find newlines & tabs
    let tab_nl_vec = _mm_set1_epi8(0b11111000);
    let tab_nl_stat_vec = _mm_set1_epi8(0b00001111);

    while c.offset + 16 < c.content.len() {
        let values = _mm_loadu_si128(
            c.content.get_unchecked(c.offset) as *const u8 as *const _);

        // Values will be 255 if they're whitespace
        // andnot(a, b) does ((NOT a) AND b)
        let result = _mm_andnot_si128(
            _mm_cmpeq_epi8(values, space_vec),
            // In this case, gt is the same as neq
            _mm_cmpgt_epi8(
                _mm_and_si128(values, tab_nl_vec),
                tab_nl_stat_vec
            )
        );

        // Compute bitmask of which values are 255
        // Mask is zeros going from from right to left
        let mask = _mm_movemask_epi8(result) as u32;

        if mask == 0 {
            c.offset += 16;
        } else {
            let lsb = lsb_number(mask);

            c.offset += lsb as usize;

            if !consume_comment(c) {
                return;
            }
        }
    }
    // If we're near the end of the file, fallback to classic mode
    non_simd_consume_ws(c);
}

unsafe fn non_simd_consume_ws(c: &mut ParseContext) {
    while c.offset < c.content.len() {
        match *c.content.get_unchecked(c.offset) as char {
            ' ' | '\n' | '\t'  => c.offset += 1,
            '/' => if !consume_comment(c) {
                break
            },
            _ => break,
        }
    }
}

// Parse any amount of whitespace, including comments
fn consume_ws(c: &mut ParseContext) {
    unsafe {
        #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
        simd_consume_ws(c);

        #[cfg(not(all(target_arch = "x86_64", target_feature = "avx2")))]
        non_simd_consume_ws(c);
    }
}

/**
 * @return true if it successfully parsed a comment
 */
fn consume_comment(c: &mut ParseContext) -> bool {
    if c.offset + 1 >= c.content.len() {
        return false;
    }
    unsafe {
        // Hacky way to compare for both /* at the same time with bounds check
        let x: *const u16 = c.content.as_ptr().add(c.offset) as *const u8 as *const _;
        // * first since we're on assuming little endian (x86 lyfe)
        if *x != ((('*' as u16) << 8) | ('/' as u16)) {
            return false;
        }
    }
    c.offset += 2;

    /* I found 256 vectors worked better than 128 bit for commments.
     * It's probably because comments are _usually_ longer than 16 characters.
     * But for whitespace, 16 characters in a row isn't as common.
     */
    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
    unsafe {
        let asterisk_vec = _mm256_set1_epi8('*' as i8);
        let slash_vec = _mm256_set1_epi8('/' as i8);
        while c.offset + 32 < c.content.len() {
            let values = _mm256_loadu_si256(
                c.content.get_unchecked(c.offset) as *const u8 as *const _);

            let asterisks = _mm256_cmpeq_epi8(values, asterisk_vec);
            let slashes   = _mm256_cmpeq_epi8(values, slash_vec);

            let asterisk_mask = _mm256_movemask_epi8(asterisks) as u32;
            let slash_mask    = _mm256_movemask_epi8(slashes) as u32;

            let mask = asterisk_mask & slash_mask.wrapping_shr(1);

            if mask == 0 {
                // Only + 31 in case the */ is at the end of the current vector
                c.offset += 31;
            } else {
                let lsb = lsb_number(mask);
                c.offset += lsb as usize + 2; // +2 for the */
                return true;
            }
        }
    }

    let mut one;
    let mut two = 0;

    while c.offset < c.content.len() {
        one = two;
        two = unsafe {
            *c.content.get_unchecked(c.offset)
        };
        c.offset += 1;

        if one == '*' as u8 && two == '/' as u8 {
            break;
        }
    }
    true
}
