use anyhow::{Result, bail};
use regex::Regex;
use std::rc::Rc;

#[allow(dead_code)]
pub struct Lexer;

impl Lexer {
    pub fn lex(stream: String) -> Result<Vec<Token>> {
        let mut line = 1;
        let mut character = 0;
        let mut tokens = vec![];
        let mut current_stream = stream.as_str();
        loop {
            match Token::consume(current_stream, &mut line, &mut character) {
                Ok((token, s)) if token != Token::Eof => {
                    tokens.push(token);
                    current_stream = s;
                }
                Err(_) => {
                    bail!(
                        "Invalid token encountered at line {}, character {} starting at:\n\"\"\"\n{}\n\"\"\"",
                        line,
                        character,
                        &current_stream,
                    );
                }
                _ => {
                    break;
                }
            }
        }
        Ok(tokens)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantSuffix {
    Long,
    Unsigned,
    UnsignedLong,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Ident(Rc<String>),
    Constant {
        text: Rc<String>,
        suffix: Option<ConstantSuffix>,
    },
    // Reserved
    Return,
    Typedef,
    SizeOf,
    Extern,
    Static,
    Auto,
    Register,
    Restrict,
    // Loop
    Case,
    Default,
    If,
    Else,
    Switch,
    While,
    Do,
    For,
    Goto,
    Continue,
    Break,
    // Storage/types
    Char,
    Short,
    Int,
    Long,
    Signed,
    Unsigned,
    Float,
    Double,
    Const,
    Volatile,
    Void,
    Struct,
    Union,
    Enum,
    // Symbols
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    Semi,
    Eof,
    Eq,
    GreatEq,
    LessEq,
    And,
    Or,
    Decrement,
    Increment,
    Ampersand,
    BitOr,
    Less,
    Great,
    Not,
    Assign,
    Plus,
    Minus,
    Star,
    Divide,
    LBracket,
    RBracket,
    Colon,
    Comma,
    Ellipsis,
    NotEq,
    AddAssign,
    SubAssign,
    MultAssign,
    DivAssign,
    OrAssign,
    AndAssign,
    XorAssign,
    ModAssign,
    LShiftAssign,
    RShiftAssign,
    LShift,
    RShift,
    BitXor,
    BitNot,
    Mod,
    Ternary,
    DoubleQuote,
    SingleQuote,
}

impl std::fmt::Display for ConstantSuffix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Long => write!(f, "l"),
            Self::Unsigned => write!(f, "u"),
            Self::UnsignedLong => write!(f, "ul"),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(s) => write!(f, "Identifer: \"{}\"", s),
            Self::Constant { text, suffix } => {
                write!(f, "Constant: \"{}\"", text)?;
                if let Some(suffix) = suffix {
                    write!(f, "{}", suffix)?;
                }
                Ok(())
            }
            Self::Return => write!(f, "Return"),
            Self::Typedef => write!(f, "Typedef"),
            Self::SizeOf => write!(f, "SizeOf"),
            Self::Extern => write!(f, "Extern"),
            Self::Static => write!(f, "Static"),
            Self::Auto => write!(f, "Auto"),
            Self::Register => write!(f, "Register"),
            Self::Restrict => write!(f, "Restrict"),
            Self::Case => write!(f, "Case"),
            Self::Default => write!(f, "Default"),
            Self::If => write!(f, "If"),
            Self::Else => write!(f, "Else"),
            Self::Switch => write!(f, "Switch"),
            Self::While => write!(f, "While"),
            Self::Do => write!(f, "Do"),
            Self::For => write!(f, "For"),
            Self::Goto => write!(f, "Goto"),
            Self::Continue => write!(f, "Continue"),
            Self::Break => write!(f, "Break"),
            Self::Char => write!(f, "Char"),
            Self::Short => write!(f, "Short"),
            Self::Int => write!(f, "Int"),
            Self::Long => write!(f, "Long"),
            Self::Signed => write!(f, "Signed"),
            Self::Unsigned => write!(f, "Unsigned"),
            Self::Float => write!(f, "Float"),
            Self::Double => write!(f, "Double"),
            Self::Const => write!(f, "Const"),
            Self::Volatile => write!(f, "Volatile"),
            Self::Void => write!(f, "Void"),
            Self::Struct => write!(f, "Struct"),
            Self::Union => write!(f, "Union"),
            Self::Enum => write!(f, "Enum"),
            Self::LParen => write!(f, "LParen"),
            Self::RParen => write!(f, "RParen"),
            Self::LSquirly => write!(f, "LSquirly"),
            Self::RSquirly => write!(f, "RSquirly"),
            Self::Semi => write!(f, "Semi"),
            Self::Eof => write!(f, "Eof"),
            Self::Eq => write!(f, "Eq"),
            Self::GreatEq => write!(f, "GreatEq"),
            Self::LessEq => write!(f, "LessEq"),
            Self::And => write!(f, "And"),
            Self::Or => write!(f, "Or"),
            Self::Decrement => write!(f, "Decrement"),
            Self::Increment => write!(f, "Increment"),
            Self::Ampersand => write!(f, "Ampersand"),
            Self::BitOr => write!(f, "BitOr"),
            Self::Less => write!(f, "Less"),
            Self::Great => write!(f, "Great"),
            Self::Not => write!(f, "Not"),
            Self::Assign => write!(f, "Assign"),
            Self::Plus => write!(f, "Plus"),
            Self::Minus => write!(f, "Minus"),
            Self::Star => write!(f, "Star"),
            Self::Divide => write!(f, "Divide"),
            Self::LBracket => write!(f, "LBracket"),
            Self::RBracket => write!(f, "RBracket"),
            Self::Colon => write!(f, "Colon"),
            Self::Comma => write!(f, "Comma"),
            Self::Ellipsis => write!(f, "Ellipsis"),
            Self::NotEq => write!(f, "NotEq"),
            Self::OrAssign => write!(f, "OrAssign"),
            Self::AndAssign => write!(f, "AndAssign"),
            Self::AddAssign => write!(f, "PlusAssign"),
            Self::SubAssign => write!(f, "MinusAssign"),
            Self::MultAssign => write!(f, "MultAssign"),
            Self::DivAssign => write!(f, "DivAssign"),
            Self::ModAssign => write!(f, "ModAssign"),
            Self::XorAssign => write!(f, "XorAssign"),
            Self::LShiftAssign => write!(f, "LShiftEq"),
            Self::RShiftAssign => write!(f, "RShiftEq"),
            Self::LShift => write!(f, "LShift"),
            Self::RShift => write!(f, "RShift"),
            Self::BitXor => write!(f, "BitXor"),
            Self::BitNot => write!(f, "BitNot"),
            Self::Mod => write!(f, "Mod"),
            Self::Ternary => write!(f, "Ternary"),
            Self::DoubleQuote => write!(f, "DoubleQuote"),
            Self::SingleQuote => write!(f, "SingleQuote"),
        }
    }
}

impl Token {
    const IDENT: &'static str = r"^[a-zA-Z_]\w*\b";
    const STRING: &'static str = r#""(?:[^"\\]|\\[\s\S])*""#;
    const CHAR: &'static str = r"'[^'\\]|\\[\s\S]'";
    const FLOAT: &'static str = r"^[0-9]+\.[0-9]+";
    const INT: &'static str = r"^[0-9]+(?:[uU]|[lL]|[uU][lL]|[lL][uU])?\b";

    const KEYWORDS: &'static [(&'static str, Token)] = &[
        ("return", Token::Return),
        ("typedef", Token::Typedef),
        ("sizeof", Token::SizeOf),
        ("extern", Token::Extern),
        ("static", Token::Static),
        ("auto", Token::Auto),
        ("register", Token::Register),
        ("restrict", Token::Restrict),
        ("case", Token::Case),
        ("default", Token::Default),
        ("if", Token::If),
        ("else", Token::Else),
        ("switch", Token::Switch),
        ("while", Token::While),
        ("do", Token::Do),
        ("for", Token::For),
        ("goto", Token::Goto),
        ("continue", Token::Continue),
        ("break", Token::Break),
        ("char", Token::Char),
        ("short", Token::Short),
        ("int", Token::Int),
        ("long", Token::Long),
        ("signed", Token::Signed),
        ("unsigned", Token::Unsigned),
        ("float", Token::Float),
        ("double", Token::Double),
        ("const", Token::Const),
        ("volatile", Token::Volatile),
        ("void", Token::Void),
        ("struct", Token::Struct),
        ("union", Token::Union),
        ("enum", Token::Enum),
    ];
    const SYMBOLS: &'static [(&'static str, Token)] = &[
        ("...", Token::Ellipsis),
        ("<<=", Token::LShiftAssign),
        (">>=", Token::RShiftAssign),
        ("==", Token::Eq),
        (">=", Token::GreatEq),
        ("<=", Token::LessEq),
        ("!=", Token::NotEq),
        ("&&", Token::And),
        ("||", Token::Or),
        ("|=", Token::OrAssign),
        ("&=", Token::AndAssign),
        ("^=", Token::XorAssign),
        ("+=", Token::AddAssign),
        ("-=", Token::SubAssign),
        ("*=", Token::MultAssign),
        ("/=", Token::DivAssign),
        ("%=", Token::ModAssign),
        ("--", Token::Decrement),
        ("++", Token::Increment),
        ("<<", Token::LShift),
        (">>", Token::RShift),
        ("^", Token::BitXor),
        ("~", Token::BitNot),
        ("&", Token::Ampersand),
        ("|", Token::BitOr),
        ("%", Token::Mod),
        ("<", Token::Less),
        (">", Token::Great),
        ("!", Token::Not),
        ("=", Token::Assign),
        ("+", Token::Plus),
        ("-", Token::Minus),
        ("*", Token::Star),
        ("/", Token::Divide),
        ("[", Token::LBracket),
        ("]", Token::RBracket),
        ("(", Token::LParen),
        (")", Token::RParen),
        ("{", Token::LSquirly),
        ("}", Token::RSquirly),
        (";", Token::Semi),
        (":", Token::Colon),
        (",", Token::Comma),
        ("?", Token::Ternary),
    ];
    pub fn consume<'a>(
        mut stream: &'a str,
        line: &mut usize,
        character: &mut usize,
    ) -> Result<(Token, &'a str)> {
        let mut chars_found = false;
        for (i, c) in stream.chars().enumerate() {
            match c {
                '\n' | '\r' => {
                    *character = 0;
                    *line += 1;
                }
                c if c.is_whitespace() => {
                    *character += 1;
                }
                _ => {
                    chars_found = true;
                    stream = &stream[i..];
                    break;
                }
            }
        }

        if !chars_found {
            return Ok((Token::Eof, stream));
        }

        if let Some((token, stream)) = Self::match_symbol(stream, line, character) {
            return Ok((token, stream));
        }

        if let Some((token, stream)) = Self::match_literal(stream, line, character) {
            Ok((token, stream))
        } else if let Some((token, stream)) = Self::match_ident(stream, line, character) {
            Ok((token, stream))
        } else {
            bail! {
                "Failed to parse token at line {}, character {}", line, character
            }
        }
    }

    fn match_regex<'a>(stream: &'a str, pattern: &'_ str) -> Result<&'a str> {
        let re = Regex::new(pattern)?;
        if let Some(capture) = re.captures(stream) {
            let (full, _) = capture.extract::<0>();
            Ok(full)
        } else {
            bail!("No match found in stream with pattern {}", pattern)
        }
    }

    fn match_literal<'a>(
        stream: &'a str,
        _line: &mut usize,
        character: &mut usize,
    ) -> Option<(Token, &'a str)> {
        fn make_literal(literal: &str) -> Token {
            let (text, suffix) = match literal.as_bytes() {
                &[.., c1, c2] => {
                    let c1 = c1.to_ascii_lowercase();
                    let c2 = c2.to_ascii_lowercase();
                    let (suffix_len, suffix) = match (c1, c2) {
                        (b'u', b'l') | (b'l', b'u') => (2, Some(ConstantSuffix::UnsignedLong)),
                        (_, b'l') => (1, Some(ConstantSuffix::Long)),
                        (_, b'u') => (1, Some(ConstantSuffix::Unsigned)),
                        _ => (0, None),
                    };

                    (
                        literal.chars().take(literal.len() - suffix_len).collect(),
                        suffix,
                    )
                }
                _ => (literal.to_string(), None),
            };
            Token::Constant {
                text: Rc::new(text),
                suffix,
            }
        }

        if let Some((token, len)) = {
            match stream.chars().next() {
                Some('\'') => Self::match_regex(stream, Self::CHAR).map_or(None, |s| {
                    Some((
                        Token::Constant {
                            text: Rc::new(s.to_string()),
                            suffix: None,
                        },
                        s.len(),
                    ))
                }),
                Some('"') => Self::match_regex(stream, Self::STRING).map_or(None, |s| {
                    Some((
                        Token::Constant {
                            text: Rc::new(s.to_string()),
                            suffix: None,
                        },
                        s.len(),
                    ))
                }),
                Some(c) if c.is_ascii_digit() => {
                    if let Ok(s) = Self::match_regex(stream, Self::FLOAT) {
                        Some((make_literal(s), s.len()))
                    } else if let Ok(s) = Self::match_regex(stream, Self::INT) {
                        Some((make_literal(s), s.len()))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        } {
            *character += len;
            Some((token, &stream[len..]))
        } else {
            None
        }
    }

    // Will match an ident, then look to see whether it matches a keyword
    fn match_ident<'a>(
        stream: &'a str,
        _line: &mut usize,
        character: &mut usize,
    ) -> Option<(Token, &'a str)> {
        if let Ok(ident) = Self::match_regex(stream, Self::IDENT) {
            match Self::KEYWORDS
                .iter()
                .find(|(keyword_str, _)| ident == *keyword_str)
            {
                Some((keyword_str, token)) => {
                    let len = keyword_str.len();
                    *character += len;
                    Some((token.clone(), &stream[len..]))
                }
                None => {
                    let len = ident.len();
                    *character += len;
                    Some((Token::Ident(Rc::new(ident.to_string())), &stream[len..]))
                }
            }
        } else {
            None
        }
    }

    fn match_symbol<'a>(
        stream: &'a str,
        _line: &mut usize,
        character: &mut usize,
    ) -> Option<(Token, &'a str)> {
        match Self::SYMBOLS
            .iter()
            .find(|(symbol_str, _)| stream.starts_with(symbol_str))
        {
            Some((symbol_str, token)) => {
                let len = symbol_str.len();
                *character += len;
                Some((token.clone(), &stream[len..]))
            }
            None => None,
        }
    }
}
