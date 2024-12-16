use crate::lexer::Token;
use anyhow::{bail, Context, Result};

pub fn parse<'a>(tokens: &'a [Token]) -> Result<Program<'a>> {
    let (program, tokens) = Program::consume(tokens)?;
    if !tokens.is_empty() {
        bail!("Found extra tokens when parsing main:\n{:#?}", tokens)
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
    pub function: Function<'a>,
}
impl<'a> AstNode<'a> for Program<'a> {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Program<'a>, &'a [Token<'a>])> {
        let (function, tokens) = Function::consume(tokens).context("Could not parse function.")?;
        if let Function {
            ret_t: Type::Int,
            name: "main",
            ..
        } = function
        {
            Ok((Self { function }, tokens))
        } else {
            bail!("Could not find a \"main\" function.");
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    pub ret_t: Type,
    pub name: &'a str,
    pub signature: Vec<(Type, Option<&'a str>)>,
    pub statements: Vec<Stmt>,
}

impl<'a> AstNode<'a> for Function<'a> {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Function<'a>, &'a [Token<'a>])> {
        let (ret_t, tokens) = Type::consume(tokens)
            .context("No return type indicated for function. Add a return type, or mark the function as returning \"void\" to signal that it returns nothing.")?;
        if let [Token::Ident(name), Token::LParen, tokens @ ..] = tokens {
            let mut signature = vec![];
            let mut remaining = match tokens {
                [Token::Void, Token::RParen, Token::LSquirly, tokens @ ..] => tokens,
                [Token::Void, t, ..] => {
                    bail!("Expected closing parentheses but found \"{}\"", t)
                }
                tokens => {
                    let mut remaining = tokens;
                    while let Ok((param_t, tokens)) = Type::consume(remaining) {
                        match tokens {
                            [Token::Ident(s), Token::Comma, tokens @ ..]
                            | [Token::Ident(s), tokens @ ..] => {
                                signature.push((param_t, Some(*s)));
                                remaining = tokens;
                            }
                            [Token::Comma, tokens @ ..] => {
                                signature.push((param_t, None));
                                remaining = tokens;
                            }
                            [t, ..] => {
                                bail!("Expected parameter name or comma but found : {}", t)
                            }
                            [] => {
                                bail!("Expected parameter name or comma but found no more tokens.")
                            }
                        }
                    }

                    match remaining {
                        [Token::RParen, Token::LSquirly, tokens @ ..] => tokens,
                        [t, ..] => bail!("Expected a closing parentheses but found {}", t),
                        [] => bail!("Expected a closing parentheses but found no more tokens."),
                    }
                }
            };

            let mut statements = vec![];
            while let Ok((stmt, tokens)) = Stmt::consume(remaining) {
                remaining = tokens;
                statements.push(stmt);
            }
            if let Some(Token::RSquirly) = remaining.first() {
                Ok((
                    Self {
                        ret_t,
                        name,
                        signature,
                        statements,
                    },
                    &remaining[1..],
                ))
            } else {
                bail!("No closing brace for the function. Add a \"}}\" after the statements in the function body. Found tokens:\n{:#?}", remaining)
            }
        } else {
            bail!("Failed to parse function.")
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Return(Option<Expr>),
    //If {
    //    condition: Expr<'a>,
    //    then_stmt: Box<Stmt<'a>>,
    //    else_stmt: Option<Box<Stmt<'a>>>,
    //},
}
impl<'a> AstNode<'a> for Stmt {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Stmt, &'a [Token<'a>])> {
        match tokens {
            [Token::Return, Token::Semi, ..] => Ok((Self::Return(None), tokens)),
            [Token::Return, tokens @ ..] => {
                let (expr, tokens) = Expr::consume(tokens).context(
                    "Expected return statement to return an expression but could not parse one.",
                )?;
                if let Some(Token::Semi) = tokens.first() {
                    Ok((Self::Return(Some(expr)), &tokens[1..]))
                } else {
                    bail!("Missing semicolon after return expression.")
                }
            }
            _ => bail!("Unable to parse semantically valid statement."),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Unary(Unary, Box<Expr>),
}
impl<'a> AstNode<'a> for Expr {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Expr, &'a [Token<'a>])> {
        if let Ok((literal, tokens)) = Literal::consume(tokens) {
            Ok((Expr::Literal(literal), tokens))
        } else if let Ok((unary, tokens)) = Unary::consume(tokens) {
            // We have to parse an Expr here
            let (expr, tokens) =
                Expr::consume(tokens).context("Parsing grammer rule: <unop> <exp> failed")?;
            Ok((Expr::Unary(unary, Box::new(expr)), tokens))
        } else {
            // We have 1 more rule to try to parse here
            // "(" <exp> ")"
            match tokens {
                [Token::LParen, tokens @ ..] => {
                    let (expr, tokens) = Expr::consume(tokens)
                        .context("Parsing grammer rule: \"(\" <exp> \")\" failed")?;
                    match tokens {
                        [Token::RParen, tokens @ ..] => Ok((expr, tokens)),
                        _ => bail!("Could not find matching right parenthesis"),
                    }
                }
                _ => bail!("Could not parse valid expression."),
            }
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
pub enum Literal {
    Int(i32),
}
impl<'a> AstNode<'a> for Literal {
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

#[derive(Debug, PartialEq)]
pub enum Unary {
    Complement,
    Negate,
}

impl<'a> AstNode<'a> for Unary {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Self, &'a [Token<'a>])>
    where
        Self: Sized,
    {
        if let Some(token) = tokens.first() {
            match token {
                Token::Minus => Ok((Self::Negate, &tokens[1..])),
                Token::BitNot => Ok((Self::Complement, &tokens[1..])),
                _ => bail!("Expected '-' or '~', found '{}'", token),
            }
        } else {
            bail!("No remaining tokens")
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
                name: "main",
                signature: vec![],
                statements: vec![Stmt::Return(Some(Expr::Literal(Literal::Int(2))))],
            },
        };
        assert_eq!(expected, program);
    }

    #[test]
    fn test_multiple_params() {
        let tokens = &[
            Token::Int,
            Token::Ident("multiple_args"),
            Token::LParen,
            Token::Int,
            Token::Ident("x"),
            Token::Comma,
            Token::Int,
            Token::Comma,
            Token::Int,
            Token::Ident("z"),
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::Literal("2"),
            Token::Semi,
            Token::RSquirly,
        ];
        let (function, _) = Function::consume(tokens).unwrap();
        let expected = Function {
            ret_t: Type::Int,
            name: "multiple_args",
            signature: vec![
                (Type::Int, Some("x")),
                (Type::Int, None),
                (Type::Int, Some("z")),
            ],
            statements: vec![Stmt::Return(Some(Expr::Literal(Literal::Int(2))))],
        };
        assert_eq!(expected, function);
    }

    #[test]
    fn rejects_trailing_comma() {
        let tokens = &[
            Token::Int,
            Token::Ident("trailing_comma"),
            Token::LParen,
            Token::Int,
            Token::Ident("x"),
            Token::Comma,
            Token::RParen,
            Token::LSquirly,
            Token::RSquirly,
        ];
        let program = parse(tokens);
        assert!(program.is_err());
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
        let program = parse(tokens).unwrap();
        let expected = Program {
            function: Function {
                name: "main",
                statements: vec![],
                signature: vec![],
                ret_t: Type::Int,
            },
        };
        assert_eq!(expected, program);
    }

    #[test]
    fn parse_unary_bitnot() {
        let tokens = &[Token::BitNot, Token::Literal("1")];

        let (expr, tokens) = Expr::consume(tokens).unwrap();

        assert!(tokens.is_empty());
        assert_eq!(
            expr,
            Expr::Unary(Unary::Complement, Box::new(Expr::Literal(Literal::Int(1))))
        );
    }

    #[test]
    fn parse_unary_negate() {
        let tokens = &[Token::Minus, Token::Literal("1")];

        let (expr, tokens) = Expr::consume(tokens).unwrap();

        assert!(tokens.is_empty());
        assert_eq!(
            expr,
            Expr::Unary(Unary::Negate, Box::new(Expr::Literal(Literal::Int(1))))
        );
    }

    #[test]
    fn parse_nested_expr() {
        let tokens = &[
            Token::LParen,
            Token::LParen,
            Token::LParen,
            Token::Literal("1"),
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ];

        let (expr, tokens) = Expr::consume(tokens).unwrap();

        assert!(tokens.is_empty());
        assert_eq!(expr, Expr::Literal(Literal::Int(1)));
    }

    #[test]
    fn parse_unary_complex() {
        let tokens = &[
            Token::LParen,
            Token::BitNot,
            Token::LParen,
            Token::Minus,
            Token::LParen,
            Token::Literal("2"),
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ];

        let (expr, tokens) = Expr::consume(tokens).unwrap();

        assert!(tokens.is_empty());

        assert_eq!(
            expr,
            Expr::Unary(
                Unary::Complement,
                Box::new(Expr::Unary(
                    Unary::Negate,
                    Box::new(Expr::Literal(Literal::Int(2)))
                ))
            )
        );
    }

    #[test]
    fn return_2_unary_edition() {
        let tokens = &[
            Token::Int,
            Token::Ident("main"),
            Token::LParen,
            Token::Void,
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::Minus,
            Token::LParen,
            Token::Minus,
            Token::Literal("2"),
            Token::RParen,
            Token::Semi,
            Token::RSquirly,
        ];
        let program = parse(tokens).unwrap();
        let expected = Program {
            function: Function {
                ret_t: Type::Int,
                name: "main",
                signature: vec![],
                statements: vec![Stmt::Return(Some(Expr::Unary(
                    Unary::Negate,
                    Box::new(Expr::Unary(
                        Unary::Negate,
                        Box::new(Expr::Literal(Literal::Int(2))),
                    )),
                )))],
            },
        };
        assert_eq!(expected, program);
    }

    #[test]
    fn parse_expr_thing() {
        let tokens = &[
            Token::Minus,
            Token::LParen,
            Token::Minus,
            Token::Literal("2"),
            Token::RParen,
            Token::Semi,
        ];

        let (expr, tokens) = Expr::consume(tokens).unwrap();

        assert_eq!(&tokens, &[Token::Semi]);

        assert_eq!(
            expr,
            Expr::Unary(
                Unary::Negate,
                Box::new(Expr::Unary(
                    Unary::Negate,
                    Box::new(Expr::Literal(Literal::Int(2))),
                ))
            )
        );
    }

    #[test]
    fn parse_expr_stmt() {
        let tokens = &[
            Token::Return,
            Token::Minus,
            Token::LParen,
            Token::Minus,
            Token::Literal("2"),
            Token::RParen,
            Token::Semi,
        ];

        let (_stmt, tokens) = Stmt::consume(tokens).unwrap();

        assert!(tokens.is_empty());

        // println!("{:#?}", stmt);
    }
}
