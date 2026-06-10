use anyhow::Context;
/**
! Helper functions for dealing with token streams.
*/
use anyhow::Result;
use anyhow::ensure;

use crate::ast::Expr;
use crate::lexer::Token;

pub(super) fn semi_terminated_expr(stream: &[Token]) -> Result<(Expr, &[Token])> {
    let (expr, stream) = Expr::parse(stream, 0)
        .context("Expected return statement to return an expression but could not parse one.")?;
    let tokens = eat_semi(stream).context("Expected expression to be terminated by semicolon")?;
    Ok((expr, tokens))
}

pub(super) fn eat_rbracket(stream: &[Token]) -> Result<&[Token]> {
    eat_tok(stream, Token::RBracket)
}

pub(super) fn eat_rparen(stream: &[Token]) -> Result<&[Token]> {
    eat_tok(stream, Token::RParen)
}

pub(super) fn eat_lparen(stream: &[Token]) -> Result<&[Token]> {
    eat_tok(stream, Token::LParen)
}

pub(super) fn eat_semi(stream: &[Token]) -> Result<&[Token]> {
    eat_tok(stream, Token::Semi)
}

pub(super) fn eat_tok(stream: &[Token], expected: Token) -> Result<&[Token]> {
    let next = stream.first();
    ensure!(
        next.is_some_and(|t| *t == expected),
        format!("Expected token {expected} but got {next:?}")
    );
    Ok(&stream[1..])
}
