use crate::{
    ast::{Expr, UnaryOp, primary_expr::PrimaryExpr, token_stream::eat_rbracket},
    lexer::Token,
};
use anyhow::{Context, Result};

/// <postfix-exp> grammar rule
pub(in crate::ast) struct PostfixExpr;

impl PostfixExpr {
    pub(super) fn parse(tokens: &[Token]) -> Result<(Expr, &[Token])> {
        PrimaryExpr::parse(tokens).and_then(|(expr, tokens)| Self::check_for_postfix(expr, tokens))
    }

    pub(super) fn check_for_postfix(expr: Expr, tokens: &[Token]) -> Result<(Expr, &[Token])> {
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
                    tokens,
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
}
