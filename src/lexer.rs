use anyhow::{bail, Result};
use regex::Regex;

#[allow(dead_code)]
pub struct Lexer;

impl Lexer {
    #[allow(dead_code)]
    pub fn lex(mut stream: &str) -> Result<Vec<Token>> {
        let mut line = 1;
        let mut character = 0;
        let mut tokens = vec![];
        loop {
            match Token::consume(stream, &mut line, &mut character) {
                Ok((token, s)) if token != Token::Eof => {
                    tokens.push(token);
                    stream = s;
                }
                Err(_) => {
                    bail!(
                        "Invalid token encountered at line {}, character {} starting at:\n\"\"\"\n{}\n\"\"\"",
                        line,
                        character,
                        &stream[..100],
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

#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Token<'a> {
    Ident(&'a str),
    Literal(&'a str),
    // Reserved
    Return,
    Typedef,
    SizeOf,
    Extern,
    Static,
    Auto,
    Register,
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
    MinusMinus,
    PlusPlus,
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
    LShiftEq,
    RShiftEq,
    NotEq,
    BitOrEq,
    BitAndEq,
    PlusAssign,
    MinusAssign,
    MultAssign,
    DivideAssign,
    ModAssign,
    LShift,
    RShift,
    BitXor,
    BitNot,
    Mod,
    Ternary,
    DoubleQuote,
    SingleQuote,
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(s) => write!(f, "Identifer: \"{}\"", s),
            Self::Literal(s) => write!(f, "Literal: \"{}\"", s),
            Self::Return => write!(f, "Return"),
            Self::Typedef => write!(f, "Typedef"),
            Self::SizeOf => write!(f, "SizeOf"),
            Self::Extern => write!(f, "Extern"),
            Self::Static => write!(f, "Static"),
            Self::Auto => write!(f, "Auto"),
            Self::Register => write!(f, "Register"),
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
            Self::MinusMinus => write!(f, "MinusMinus"),
            Self::PlusPlus => write!(f, "PlusPlus"),
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
            Self::LShiftEq => write!(f, "LShiftEq"),
            Self::RShiftEq => write!(f, "RShiftEq"),
            Self::NotEq => write!(f, "NotEq"),
            Self::BitOrEq => write!(f, "BitOrEq"),
            Self::BitAndEq => write!(f, "BitAndEq"),
            Self::PlusAssign => write!(f, "PlusAssign"),
            Self::MinusAssign => write!(f, "MinusAssign"),
            Self::MultAssign => write!(f, "MultAssign"),
            Self::DivideAssign => write!(f, "DivideAssign"),
            Self::ModAssign => write!(f, "ModAssign"),
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

impl Token<'_> {
    const IDENT: &'static str = r"^[a-zA-Z_]\w*\b";
    // TODO: Expand (e.g., Integer suffixes)
    const STRING: &'static str = r#""(?:[^"\\]|\\[\s\S])*""#;
    const CHAR: &'static str = r"'[^'\\]|\\[\s\S]'";
    const FLOAT: &'static str = r"^[0-9]+\.[0-9]+";
    const INT: &'static str = r"^[0-9]+\b";

    const KEYWORDS: &'static [(&'static str, Token<'static>)] = &[
        ("return", Token::Return),
        ("typedef", Token::Typedef),
        ("sizeof", Token::SizeOf),
        ("extern", Token::Extern),
        ("static", Token::Static),
        ("auto", Token::Auto),
        ("register", Token::Register),
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
    const SYMBOLS: &'static [(&'static str, Token<'static>)] = &[
        ("...", Token::Ellipsis),
        ("<<=", Token::LShiftEq),
        (">>=", Token::RShiftEq),
        ("==", Token::Eq),
        (">=", Token::GreatEq),
        ("<=", Token::LessEq),
        ("!=", Token::NotEq),
        ("&&", Token::And),
        ("||", Token::Or),
        ("|=", Token::BitOrEq),
        ("&=", Token::BitAndEq),
        ("+=", Token::PlusAssign),
        ("-=", Token::MinusAssign),
        ("*=", Token::MultAssign),
        ("/=", Token::DivideAssign),
        ("%=", Token::ModAssign),
        ("--", Token::MinusMinus),
        ("++", Token::PlusPlus),
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
    ) -> Result<(Token<'a>, &'a str)> {
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
    ) -> Option<(Token<'a>, &'a str)> {
        if let Some((token, len)) = {
            match stream.chars().next() {
                Some('\'') => Self::match_regex(stream, Self::CHAR)
                    .map_or(None, |s| Some((Token::Literal(s), s.len()))),
                Some('"') => Self::match_regex(stream, Self::STRING)
                    .map_or(None, |s| Some((Token::Literal(s), s.len()))),
                Some(c) if c.is_ascii_digit() => {
                    if let Ok(s) = Self::match_regex(stream, Self::FLOAT) {
                        Some((Token::Literal(s), s.len()))
                    } else if let Ok(s) = Self::match_regex(stream, Self::INT) {
                        Some((Token::Literal(s), s.len()))
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
    ) -> Option<(Token<'a>, &'a str)> {
        if let Ok(ident) = Self::match_regex(stream, Self::IDENT) {
            match Self::KEYWORDS
                .iter()
                .find(|(keyword_str, _)| ident == *keyword_str)
            {
                Some((keyword_str, token)) => {
                    let len = keyword_str.len();
                    *character += len;
                    Some((*token, &stream[len..]))
                }
                None => {
                    let len = ident.len();
                    *character += len;
                    Some((Token::Ident(ident), &stream[len..]))
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
    ) -> Option<(Token<'a>, &'a str)> {
        match Self::SYMBOLS
            .iter()
            .find(|(symbol_str, _)| stream.starts_with(symbol_str))
        {
            Some((symbol_str, token)) => {
                let len = symbol_str.len();
                *character += len;
                Some((*token, &stream[len..]))
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::{ensure, Context};
    use std::env;
    use std::path::Path;
    use std::process::Command;

    fn preprocess_file(file: &str) -> Result<String> {
        let preproccesed_file = format!("{}.i", file);

        let output = Command::new("gcc")
            .args(["-E", "-P", file, "-o", &preproccesed_file])
            .output()
            .context("Failed to run gcc from preprocessing")?;

        ensure!(
            output.status.success(),
            "gcc exited with error code {}",
            output.status,
        );

        // Compile the preprocessed source file
        let contents = std::fs::read_to_string(&preproccesed_file)
            .with_context(|| format!("Failed to read file: \"{}\"", preproccesed_file))?;

        // Cleanup
        std::fs::remove_file(&preproccesed_file)
            .with_context(|| format!("Failed to remove temp file \"{}\"", preproccesed_file))?;
        Ok(contents)
    }

    fn get_path(chapter: u8, file: &str) -> String {
        let proj_root =
            env::var("CARGO_MANIFEST_DIR").expect("Could not get project root directory.");
        let proj_root_path = Path::new(&proj_root);
        proj_root_path
            .join("tests")
            .join(format!("chapter{}", chapter))
            .join(file)
            .into_os_string()
            .into_string()
            .unwrap()
    }

    #[test]
    fn test_return2() {
        let file = get_path(1, "return_2.c");
        let contents = preprocess_file(&file).unwrap();
        let tokens = Lexer::lex(&contents).unwrap();
        let expected = vec![
            Token::Int,
            Token::Ident("main"),
            Token::LParen,
            Token::Void,
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::Literal("2"),
            Token::Semi,
            Token::RSquirly,
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_complex() {
        let file = get_path(1, "complex.c");
        let preprocessed_contents = preprocess_file(&file).unwrap();
        // Don't match on exact contents but make sure it can successfully
        // parse a fairly complex C file
        let _ = Lexer::lex(&preprocessed_contents).unwrap();
    }
}
