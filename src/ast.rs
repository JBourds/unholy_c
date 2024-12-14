use crate::lexer::Token;
use anyhow::{bail, Result};

pub fn parse<'a>(tokens: &'a [Token]) -> Result<Program<'a>> {
    let (program, tokens) = Program::consume(tokens)?;
    if !tokens.is_empty() {
        bail!("Found extra tokens after parsing program: {:?}", tokens)
    } else {
        Ok(program)
    }
}

pub trait AstNode<'a> {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Self, &'a [Token<'a>])>
    where
        Self: Sized;
}

#[derive(Debug, PartialEq)]
pub struct Program<'a> {
    function: Function<'a>,
}
impl<'a> AstNode<'a> for Program<'a> {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Program<'a>, &'a [Token<'a>])> {
        let (function, tokens) = Function::consume(tokens)?;
        if let Function {
            ret_t: Type::Int,
            name: Token::Ident("main"),
            ..
        } = function
        {
            Ok((Self { function }, tokens))
        } else {
            bail!("Error parsing main function.");
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    ret_t: Type,
    name: Token<'a>,
    body: Vec<Stmt<'a>>,
}
impl<'a> AstNode<'a> for Function<'a> {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Function<'a>, &'a [Token<'a>])> {
        let (ret_t, tokens) = Type::consume(tokens)?;
        if let [Token::Ident(s), Token::LParen, Token::Void, Token::RParen, Token::LSquirly, tokens @ ..] =
            tokens
        {
            let name = Token::Ident(s);
            let (stmt, tokens) = Stmt::consume(tokens)?;
            if let Some(Token::RSquirly) = tokens.first() {
                Ok((
                    Self {
                        ret_t,
                        name,
                        body: vec![stmt],
                    },
                    &tokens[1..],
                ))
            } else {
                bail!("Did not find closing brace for function.");
            }
        } else {
            bail!("Failed to parse function.")
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    Return(Expr<'a>),
    //If {
    //    condition: Expr<'a>,
    //    then_stmt: Box<Stmt<'a>>,
    //    else_stmt: Option<Box<Stmt<'a>>>,
    //},
}
impl<'a> AstNode<'a> for Stmt<'a> {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Stmt<'a>, &'a [Token<'a>])> {
        if let Some(Token::Return) = tokens.first() {
            match Expr::consume(&tokens[1..]) {
                Ok((expr, [Token::Semi, end @ ..])) => Ok((Self::Return(expr), end)),
                _ => bail!("Could not parse return statement."),
            }
        } else {
            bail!("Could not parse valid statement.");
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Literal(Literal<'a>),
}
impl<'a> AstNode<'a> for Expr<'a> {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Expr<'a>, &'a [Token<'a>])> {
        if let Ok((literal, tokens)) = Literal::consume(tokens) {
            Ok((Expr::Literal(literal), tokens))
        } else {
            bail!("Could not parse valid expression.");
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Void,
}
impl<'a> AstNode<'a> for Type {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Type, &'a [Token<'a>])> {
        if let Some(token) = tokens.first() {
            let t = match token {
                Token::Int => Type::Int,
                Token::Void => Type::Void,
                _ => bail!("Could not parse token into literal."),
            };
            Ok((t, &tokens[1..]))
        } else {
            bail!("No remaining tokens.")
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    Int(i32),
    #[allow(dead_code)]
    String(&'a str),
}
impl<'a> AstNode<'a> for Literal<'a> {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Literal, &'a [Token<'a>])> {
        if let Some(token) = tokens.first() {
            match token {
                Token::Literal(s) => {
                    if let Ok(int) = s.parse::<i32>() {
                        Ok((Self::Int(int), &tokens[1..]))
                    } else {
                        bail!("Could not parse token into literal.")
                    }
                }
                _ => bail!("Could not parse token into literal."),
            }
        } else {
            bail!("No remaining tokens.")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn return_2() {
        let tokens = &[
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
        let program = parse(tokens).unwrap();
        let expected = Program {
            function: Function {
                ret_t: Type::Int,
                name: Token::Ident("main"),
                body: vec![Stmt::Return(Expr::Literal(Literal::Int(2)))],
            },
        };
        assert_eq!(expected, program);
    }

    #[test]
    fn missing_return() {
        let tokens = &[
            Token::Int,
            Token::Ident("main"),
            Token::LParen,
            Token::Void,
            Token::RParen,
            Token::LSquirly,
            Token::RSquirly,
        ];
        let program = parse(tokens);
        assert!(program.is_err());
    }
}
