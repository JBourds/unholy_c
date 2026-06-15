use crate::{
    ast::{Expr, TypeBuilder, token_stream::eat_rparen, unary_expr::UnaryExpr},
    lexer::Token,
};
use anyhow::{Context, Result, ensure};

/// <cast-exp> grammar rule
pub(in crate::ast) struct CastExpr;

impl CastExpr {
    pub fn parse(tokens: &[Token]) -> Result<(Expr, &[Token])> {
        if let Some(Token::LParen) = tokens.first()
            && let Ok((stream_offset, r#type, storage_class)) =
                TypeBuilder::new(&tokens[1..]).build_with_abstract_declarator()
        {
            ensure!(
                storage_class.is_none(),
                "Cannot have storage specifier in type cast."
            );
            let tokens = eat_rparen(&tokens[stream_offset + 1..])
                .context("Expected \")\" to close type cast")?;
            let (expr, tokens) = CastExpr::parse(tokens)?;
            Ok((
                Expr::Cast {
                    target: r#type,
                    exp: Box::new(expr),
                },
                tokens,
            ))
        } else {
            UnaryExpr::parse(tokens)
        }
    }
}
