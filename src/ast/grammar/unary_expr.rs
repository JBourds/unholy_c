use crate::{
    ast::{
        Expr, TypeBuilder, UnaryOp,
        cast_expr::CastExpr,
        postfix_expr::PostfixExpr,
        token_stream::{eat_lparen, eat_rparen},
    },
    lexer::Token,
};
use anyhow::{Context, Result};

/// <unary-exp> grammar rule
pub(in crate::ast) struct UnaryExpr;

impl UnaryExpr {
    pub(super) fn parse(tokens: &[Token]) -> Result<(Expr, &[Token])> {
        if let Ok((op, tokens)) = UnaryOp::consume_prefix(tokens) {
            // <unop> <cast-exp>
            let (expr, tokens) = CastExpr::parse(tokens)?;
            Ok((
                Expr::Unary {
                    op,
                    expr: Box::new(expr),
                },
                tokens,
            ))
        } else if let Some(Token::SizeOf) = tokens.first() {
            let tokens = &tokens[1..];
            if let Ok((expr, tokens)) = UnaryExpr::parse(tokens) {
                // "sizeof" <unary-exp>
                Ok((Expr::SizeOf(Box::new(expr)), tokens))
            } else {
                // "sizeof" "(" <type-name> ")"
                let tokens =
                    eat_lparen(tokens).context("Expected \"(\" to open sizeof type-name")?;
                let (offset, ty, _) = TypeBuilder::new(tokens).build_with_abstract_declarator()?;
                let tokens = eat_rparen(&tokens[offset..])
                    .context("Expected \")\" to close sizeof type-name")?;
                Ok((Expr::SizeOfT(ty), tokens))
            }
        } else {
            // <postfix-exp>
            PostfixExpr::parse(tokens)
        }
    }
}
