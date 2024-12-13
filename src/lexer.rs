use anyhow::{bail, Result};
use regex::Regex;

#[allow(dead_code)]
pub struct Lexer;

impl Lexer {
    #[allow(dead_code)]
    pub fn lex(mut stream: &str) -> Result<Vec<Token>> {
        let mut line = 0;
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
                        "Invalid token encountered at line {}, character {}",
                        line,
                        character
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

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Ident(&'a str),
    Constant(&'a str),
    Int,
    Void,
    Return,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    Semi,
    Eof,
}

impl Token<'_> {
    const IDENT: &'static str = r"^[a-zA-Z_]\w*\b";
    const CONSTANT: &'static str = r"^[0-9]+\b";
    const LPAREN: &'static str = r"^\(";
    const RPAREN: &'static str = r"^\)";
    const LSQUIRLY: &'static str = r"^\{";
    const RSQUIRLY: &'static str = r"^\}";
    const SEMI: &'static str = r"^;";
    const PATTERN_STRINGS: &'static [&'static str] = &[
        Self::IDENT,
        Self::CONSTANT,
        Self::LPAREN,
        Self::RPAREN,
        Self::LSQUIRLY,
        Self::RSQUIRLY,
        Self::SEMI,
    ];
    const KEYWORD_STRINGS: &'static [&'static str] = &["int", "void", "return"];
    pub fn consume<'a>(
        mut stream: &'a str,
        line: &mut u32,
        character: &mut u32,
    ) -> Result<(Token<'a>, &'a str)> {
        let mut longest_match_index = 0;
        let mut longest_match_length = 0;
        let mut chars_found = false;
        for (i, c) in stream.chars().enumerate() {
            match c {
                ' ' | '\t' => {
                    *character += 1;
                }
                '\n' => {
                    *character = 0;
                    *line += 1;
                }
                _ => {
                    chars_found = true;
                    stream = &stream[i..];
                    for (index, pattern) in Self::PATTERN_STRINGS.iter().enumerate() {
                        let re = Regex::new(pattern).unwrap();
                        if let Some(capture) = re.captures(stream) {
                            let (full, _) = capture.extract::<0>();
                            if full.len() > longest_match_length {
                                longest_match_length = full.len();
                                longest_match_index = index;
                            }
                        }
                    }
                    break;
                }
            }
        }

        if !chars_found {
            return Ok((Token::Eof, stream));
        }

        let re = Regex::new(Self::PATTERN_STRINGS[longest_match_index]).unwrap();
        if let Some(capture) = re.captures(stream) {
            let (full, _) = capture.extract::<0>();
            stream = &stream[full.len()..];
            let token = match longest_match_index {
                0 => {
                    let mut keyword_index = None;
                    for (index, keyword) in Self::KEYWORD_STRINGS.iter().enumerate() {
                        if full == *keyword {
                            keyword_index = Some(index);
                            break;
                        }
                    }

                    if let Some(keyword_index) = keyword_index {
                        match keyword_index {
                            0 => Token::Int,
                            1 => Token::Void,
                            2 => Token::Return,
                            _ => unreachable!(),
                        }
                    } else {
                        Token::Ident(full)
                    }
                }
                1 => Token::Constant(full),
                2 => Token::LParen,
                3 => Token::RParen,
                4 => Token::LSquirly,
                5 => Token::RSquirly,
                6 => Token::Semi,
                _ => unreachable!(),
            };
            Ok((token, stream))
        } else {
            bail! {
                "Failed to parse token at line {}, character {}", line, character
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_return2() {
        let return2 = "
        int main(void) {
            return 2;
        }  
        ";
        let tokens = Lexer::lex(return2).unwrap();
        let expected = vec![
            Token::Int,
            Token::Ident("main"),
            Token::LParen,
            Token::Void,
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::Constant("2"),
            Token::Semi,
            Token::RSquirly,
        ];
        assert_eq!(expected, tokens);
    }
}
