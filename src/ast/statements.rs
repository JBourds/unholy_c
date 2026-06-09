use anyhow::{Context, Result, bail};
use std::rc::Rc;

use super::{Block, Constant, Expr, ForInit};
use crate::{
    ast::token_stream::{eat_rparen, eat_semi, semi_terminated_expr},
    lexer::Token,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Compound(Block),
    Return(Option<Expr>),
    Expr(Expr),
    If {
        condition: Expr,
        then: Box<Stmt>,
        r#else: Option<Box<Stmt>>,
    },
    Break(Option<Rc<String>>),
    Continue(Option<Rc<String>>),
    While {
        condition: Expr,
        body: Box<Stmt>,
        label: Option<Rc<String>>,
    },
    DoWhile {
        body: Box<Stmt>,
        condition: Expr,
        label: Option<Rc<String>>,
    },
    For {
        init: Box<ForInit>,
        condition: Option<Expr>,
        post: Option<Expr>,
        body: Box<Stmt>,
        label: Option<Rc<String>>,
    },
    // In newer compilers these don't have to have a statement after them
    Label {
        name: Rc<String>,
        stmt: Box<Stmt>,
    },
    Case {
        value: Expr,
        stmt: Box<Stmt>,
        label: Option<Rc<String>>,
    },
    Default {
        label: Option<Rc<String>>,
        stmt: Box<Stmt>,
    },
    Switch {
        condition: Expr,
        body: Box<Stmt>,
        label: Option<Rc<String>>,
        cases: Option<Vec<(Constant, Rc<String>)>>,
        default: Option<Rc<String>>,
    },
    Goto(Rc<String>),
    Null,
}

impl Stmt {
    pub fn consume(tokens: &[Token]) -> Result<(Stmt, &[Token])> {
        match tokens {
            [Token::LSquirly, ..] => {
                let (block, tokens) = Block::consume(tokens)?;
                Ok((Self::Compound(block), tokens))
            }
            [Token::Semi, tokens @ ..] => Ok((Self::Null, tokens)),
            [Token::Goto, Token::Ident(name), Token::Semi, tokens @ ..] => {
                Ok((Self::Goto(Rc::new(name.to_string())), tokens))
            }
            [Token::Ident(name), Token::Colon, tokens @ ..] => {
                let (stmt, tokens) = Stmt::consume(tokens)?;
                Ok((
                    Self::Label {
                        name: Rc::new(name.to_string()),
                        stmt: Box::new(stmt),
                    },
                    tokens,
                ))
            }
            [Token::Break, Token::Semi, tokens @ ..] => Ok((Self::Break(None), tokens)),
            [Token::Continue, Token::Semi, tokens @ ..] => Ok((Self::Continue(None), tokens)),
            [Token::While, Token::LParen, tokens @ ..] => {
                let (condition, tokens) = Expr::parse(tokens, 0)
                    .context("Failed to parse expression for while statement conditional")?;
                let tokens =
                    eat_rparen(tokens).context("Expected \")\" to close while-loop conditional")?;
                let (body, tokens) = Stmt::consume(tokens).context("Failed to parse while body")?;

                Ok((
                    Self::While {
                        condition,
                        body: Box::new(body),
                        label: None,
                    },
                    tokens,
                ))
            }
            [Token::Do, tokens @ ..] => {
                let (body, tokens) = Stmt::consume(tokens).context("Failed to parse while body")?;
                match tokens {
                    [Token::While, Token::LParen, tokens @ ..] => {
                        let (condition, tokens) = Expr::parse(tokens, 0).context(
                            "Failed to parse expression for do while statement conditional",
                        )?;
                        let tokens = eat_rparen(tokens)
                            .context("Expected \")\" to end while-loop conditional")?;
                        let tokens = eat_semi(tokens)
                            .context("Expected \";\" to end while-loop statement")?;
                        Ok((
                            Self::DoWhile {
                                body: Box::new(body),
                                condition,
                                label: None,
                            },
                            tokens,
                        ))
                    }
                    _ => bail!("Failed to reach while part of do-while"),
                }
            }
            [Token::For, Token::LParen, tokens @ ..] => {
                let (init, tokens) =
                    ForInit::consume(tokens).context("Failed to parse ForInit for for-loop")?;
                let (condition, tokens) = match tokens {
                    [Token::Semi, tokens @ ..] => (None, tokens),
                    _ => semi_terminated_expr(tokens)
                        .map(|(expr, tokens)| (Some(expr), tokens))
                        .context("Invalid for-loop condition expression")?,
                };
                let (post, tokens) = if let Ok((expr, tokens)) = Expr::parse(tokens, 0) {
                    (Some(expr), tokens)
                } else {
                    (None, tokens)
                };
                let tokens = eat_rparen(tokens)
                    .context("Expected \")\" to close for-loop clauses")?;
                let (body, tokens) =
                    Stmt::consume(tokens).context("Failed to parse for-loop body")?;

                Ok((
                    Self::For {
                        init: Box::new(init),
                        condition,
                        post,
                        body: Box::new(body),
                        label: None,
                    },
                    tokens,
                ))
            }
            [Token::Case, tokens @ ..] => {
                let (expr, tokens) =
                    Expr::parse(tokens, 0).context("Case statement needs associated expression")?;

                let tokens = if let Some(Token::Colon) = tokens.first() {
                    &tokens[1..]
                } else {
                    bail!("Case statement should end with a colon");
                };
                let (stmt, tokens) = Stmt::consume(tokens).context("Failed to parse case body")?;

                Ok((
                    Self::Case {
                        value: expr,
                        stmt: Box::new(stmt),
                        label: None,
                    },
                    tokens,
                ))
            }
            [Token::Default, Token::Colon, tokens @ ..] => {
                let (stmt, tokens) =
                    Stmt::consume(tokens).context("Failed to parse default body")?;

                Ok((
                    Self::Default {
                        stmt: Box::new(stmt),
                        label: None,
                    },
                    tokens,
                ))
            }
            [Token::Switch, Token::LParen, tokens @ ..] => {
                let (condition, tokens) = Expr::parse(tokens, 0)
                    .context("Failed to parse expression for switch statement conditional")?;
                let tokens = eat_rparen(tokens)
                    .context("Expected \")\" to close switch statement conditional")?;
                let (body, tokens) =
                    Stmt::consume(tokens).context("Failed to parse body of switch statement")?;
                Ok((
                    Self::Switch {
                        condition,
                        body: Box::new(body),
                        label: None,
                        cases: None,
                        default: None,
                    },
                    tokens,
                ))
            }
            [Token::Return, Token::Semi, tokens @ ..] => Ok((Self::Return(None), tokens)),
            [Token::Return, tokens @ ..] => {
                let (expr, tokens) = semi_terminated_expr(tokens)?;
                Ok((Self::Return(Some(expr)), tokens))
            }
            [Token::If, Token::LParen, tokens @ ..] => {
                let (condition, tokens) = Expr::parse(tokens, 0)
                    .context("Failed to parse expression for if statement conditional")?;
                let tokens =
                    eat_rparen(tokens).context("Expected \")\" to close if statement conditional")?;
                let (then, tokens) =
                    Stmt::consume(tokens).context("Failed to parse then branch")?;

                let (r#else, tokens) = if let Some(Token::Else) = tokens.first() {
                    let (stmt, tokens) =
                        Stmt::consume(&tokens[1..]).context("Failed to parse else branch")?;
                    (Some(stmt), tokens)
                } else {
                    (None, tokens)
                };

                Ok((
                    Self::If {
                        condition,
                        then: Box::new(then),
                        r#else: r#else.map(Box::new),
                    },
                    tokens,
                ))
            }
            _ => {
                let (expr, tokens) = semi_terminated_expr(tokens)?;
                Ok((Self::Expr(expr), tokens))
            }
        }
    }
}
