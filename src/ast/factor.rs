use anyhow::{Context, Result, bail, ensure};
use std::rc::Rc;

use super::{AbstractDeclarator, Constant, Expr, TypeBuilder, UnaryOp};
use crate::{
    ast::token_stream::{eat_rbracket, eat_rparen},
    lexer::{ConstantFlag, Token},
};

pub struct Factor;

impl Factor {
    fn check_for_postfix(expr: Expr, tokens: &[Token]) -> Result<(Expr, &[Token])> {
        match tokens {
            [Token::LBracket, tokens @ ..] => {
                let (rhs, tokens) = Expr::parse(tokens, 0)?;
                let tokens = eat_rbracket(tokens)
                    .context("check_for_postfix(): Missing \"]\" to close subscript expression")?;
                Self::check_for_postfix(
                    Expr::Subscript {
                        expr: expr.into(),
                        index: rhs.into(),
                    },
                    &tokens[1..],
                )
            }
            _ => match UnaryOp::consume_postfix(tokens) {
                Ok((op, tokens)) => Self::check_for_postfix(
                    Expr::Unary {
                        op,
                        expr: Box::new(expr),
                    },
                    tokens,
                ),
                _ => Ok((expr, tokens)),
            },
        }
    }

    fn check_for_call(expr: Expr, tokens: &[Token]) -> Result<(Expr, &[Token])> {
        match (&expr, tokens.first()) {
            (Expr::Var(name), Some(Token::LParen)) => {
                let mut args = vec![];
                let mut remaining = &tokens[1..];
                if let Some(Token::RParen) = remaining.first() {
                    Ok((
                        Expr::FunCall {
                            name: Rc::clone(name),
                            args,
                        },
                        &remaining[1..],
                    ))
                } else {
                    let mut keep_going = true;
                    while keep_going {
                        let (arg, tokens) = Expr::parse(remaining, 0)?;
                        args.push(arg);
                        match tokens {
                            [Token::Comma, tokens @ ..] => {
                                remaining = tokens;
                            }
                            [Token::RParen, tokens @ ..] => {
                                keep_going = false;
                                remaining = tokens;
                            }
                            t => bail!(
                                "Expected a \",\" or \")\" in function parameter list but found {t:?}"
                            ),
                        }
                    }
                    Ok((
                        Expr::FunCall {
                            name: Rc::clone(name),
                            args,
                        },
                        remaining,
                    ))
                }
            }
            _ => Ok((expr, tokens)),
        }
    }

    pub fn parse(tokens: &[Token]) -> Result<(Expr, &[Token])> {
        match UnaryOp::consume_prefix(tokens) {
            Ok((op, tokens)) => {
                let (expr, tokens) = Factor::parse(tokens)?;
                Ok((
                    Expr::Unary {
                        op,
                        expr: Box::new(expr),
                    },
                    tokens,
                ))
            }
            _ => match tokens {
                [
                    Token::Constant {
                        text,
                        flag: Some(ConstantFlag::String),
                    },
                    tokens @ ..,
                ] => {
                    let text = super::constants::normalize_text(text);
                    if matches!(
                        tokens.first(),
                        Some(&Token::Constant {
                            flag: Some(ConstantFlag::String),
                            ..
                        })
                    ) {
                        match Self::parse(tokens)? {
                            (Expr::String { value }, tokens) => Ok((
                                Expr::String {
                                    value: Rc::new(format!("{text}{value}")),
                                },
                                tokens,
                            )),
                            (expr, _) => bail!(
                                "expected to concat a string to a string, found '{:#?}' instead",
                                expr
                            ),
                        }
                    } else {
                        Ok((
                            Expr::String {
                                value: Rc::new(text),
                            },
                            tokens,
                        ))
                    }
                }
                [Token::Constant { .. }, ..] => {
                    let (lit, tokens) = Constant::consume(tokens)?;
                    Ok((Expr::Constant(lit), tokens))
                }
                [Token::Ident(s), tokens @ ..] => {
                    Self::check_for_call(Expr::Var(Rc::clone(s)), tokens)
                }
                // Could be parentheses for a type cast or expression precedence
                [Token::LParen, tokens @ ..] => {
                    if let Ok((stream_offset, r#type, storage_class)) =
                        TypeBuilder::new(tokens).build_with_abstract_declarator()
                    {
                        ensure!(
                            storage_class.is_none(),
                            "Cannot have storage specifier in type cast."
                        );
                        let tokens = eat_rparen(&tokens[stream_offset..])
                            .context("Expected \")\" to close type cast")?;
                        let (expr, tokens) = Factor::parse(tokens)
                            .context("Parsing grammer rule: \"(\" <exp> \")\" failed")?;
                        Self::check_for_call(
                            Expr::Cast {
                                target: r#type,
                                exp: Box::new(expr),
                            },
                            tokens,
                        )
                    } else {
                        let (expr, tokens) = Expr::parse(tokens, 0)
                            .context("Parsing grammer rule: \"(\" <exp> \")\" failed")?;
                        match tokens {
                            [Token::RParen, tokens @ ..] => {
                                let (expr, tokens) = Self::check_for_postfix(expr, tokens)?;
                                Self::check_for_call(expr, tokens)
                            }
                            _ => bail!("Could not find matching right parenthesis"),
                        }
                    }
                }
                _ => bail!("Could not match valid grammar rule."),
            }
            .and_then(|(expr, tokens)| Self::check_for_postfix(expr, tokens)),
        }
    }
}
