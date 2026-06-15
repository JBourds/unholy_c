use std::rc::Rc;

use crate::{
    ast::{Constant, Expr, constants, token_stream::eat_rparen},
    lexer::{ConstantFlag, Token},
};
use anyhow::{Context, Result, bail};

/// <primary-exp> grammar rule
pub(in crate::ast) struct PrimaryExpr;

impl PrimaryExpr {
    pub(super) fn parse(tokens: &[Token]) -> Result<(Expr, &[Token])> {
        match tokens {
            // { <string> }+
            [
                Token::Constant {
                    text,
                    flag: Some(ConstantFlag::String),
                },
                tokens @ ..,
            ] => {
                let text = constants::normalize_text(text);
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
            // <const>
            [Token::Constant { .. }, ..] => {
                let (lit, tokens) = Constant::consume(tokens)?;
                Ok((Expr::Constant(lit), tokens))
            }
            // <identifier> (a following call is handled as a postfix op)
            [Token::Ident(s), tokens @ ..] => Ok((Expr::Var(Rc::clone(s)), tokens)),
            // "(" <exp> ")"
            [Token::LParen, tokens @ ..] => {
                let (expr, tokens) = Expr::parse(tokens, 0)
                    .context("Parsing grammer rule: \"(\" <exp> \")\" failed")?;
                let tokens = eat_rparen(tokens)
                    .context("Expected \")\" to close parenthesized expression")?;
                Ok((expr, tokens))
            }
            _ => bail!("Could not match valid grammar rule."),
        }
    }
    pub(super) fn check_for_call(expr: Expr, tokens: &[Token]) -> Result<(Expr, &[Token])> {
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
}
