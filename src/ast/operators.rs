use anyhow::{Context, Result, bail};

use super::Expr;
use crate::lexer::Token;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitAnd,
    BitOr,
    Xor,
    LShift,
    RShift,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Assign,
    AddAssign,
    SubAssign,
    MultAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    LShiftAssign,
    RShiftAssign,
    Ternary,
}

impl BinaryOp {
    pub fn is_logical(&self) -> bool {
        matches!(*self, Self::And | Self::Or)
    }

    pub fn is_relational(&self) -> bool {
        matches!(
            *self,
            Self::Equal
                | Self::NotEqual
                | Self::LessThan
                | Self::LessOrEqual
                | Self::GreaterThan
                | Self::GreaterOrEqual
        )
    }

    pub fn is_add(&self) -> bool {
        matches!(*self, Self::Add | Self::AddAssign)
    }

    pub fn is_sub(&self) -> bool {
        matches!(*self, Self::Subtract | Self::SubAssign)
    }

    pub fn is_add_sub(&self) -> bool {
        matches!(
            *self,
            Self::Add | Self::AddAssign | Self::Subtract | Self::SubAssign
        )
    }

    pub fn is_bitwise(&self) -> bool {
        matches!(
            self,
            Self::BitAnd
                | Self::BitOr
                | Self::Xor
                | Self::LShift
                | Self::RShift
                | Self::AndAssign
                | Self::OrAssign
                | Self::XorAssign
                | Self::LShiftAssign
                | Self::RShiftAssign
        )
    }

    pub fn does_assignment(&self) -> bool {
        matches!(
            *self,
            Self::Assign
                | Self::AddAssign
                | Self::SubAssign
                | Self::MultAssign
                | Self::DivAssign
                | Self::ModAssign
                | Self::AndAssign
                | Self::OrAssign
                | Self::XorAssign
                | Self::LShiftAssign
                | Self::RShiftAssign
        )
    }

    pub fn compound_op(&self) -> Option<Self> {
        match self {
            Self::AddAssign => Some(Self::Add),
            Self::SubAssign => Some(Self::Subtract),
            Self::MultAssign => Some(Self::Multiply),
            Self::DivAssign => Some(Self::Divide),
            Self::ModAssign => Some(Self::Remainder),
            Self::AndAssign => Some(Self::BitAnd),
            Self::OrAssign => Some(Self::BitOr),
            Self::XorAssign => Some(Self::Xor),
            Self::LShiftAssign => Some(Self::LShift),
            Self::RShiftAssign => Some(Self::RShift),
            _ => None,
        }
    }

    pub fn precedence(&self) -> u32 {
        match *self {
            Self::Add => 45,
            Self::Subtract => 45,
            Self::Multiply => 50,
            Self::Divide => 50,
            Self::Remainder => 50,
            Self::BitAnd => 25,
            Self::BitOr => 15,
            Self::Xor => 20,
            Self::LShift => 40,
            Self::RShift => 40,
            Self::And => 10,
            Self::Or => 5,
            Self::Equal => 30,
            Self::NotEqual => 30,
            Self::LessThan => 35,
            Self::LessOrEqual => 35,
            Self::GreaterThan => 35,
            Self::GreaterOrEqual => 35,
            Self::Assign => 1,
            Self::AddAssign => 1,
            Self::SubAssign => 1,
            Self::MultAssign => 1,
            Self::DivAssign => 1,
            Self::ModAssign => 1,
            Self::AndAssign => 1,
            Self::OrAssign => 1,
            Self::XorAssign => 1,
            Self::LShiftAssign => 1,
            Self::RShiftAssign => 1,
            Self::Ternary => 3,
        }
    }

    pub fn parse(tokens: &[Token]) -> Result<Option<(BinaryOp, &[Token])>> {
        let token = tokens.first().context("No remaining tokens")?;
        let tokens = &tokens[1..];

        match token {
            Token::Plus => Ok(Some((BinaryOp::Add, tokens))),
            Token::Minus => Ok(Some((BinaryOp::Subtract, tokens))),
            Token::Star => Ok(Some((BinaryOp::Multiply, tokens))),
            Token::Divide => Ok(Some((BinaryOp::Divide, tokens))),
            Token::Mod => Ok(Some((BinaryOp::Remainder, tokens))),
            Token::Ampersand => Ok(Some((BinaryOp::BitAnd, tokens))),
            Token::BitOr => Ok(Some((BinaryOp::BitOr, tokens))),
            Token::BitXor => Ok(Some((BinaryOp::Xor, tokens))),
            Token::LShift => Ok(Some((BinaryOp::LShift, tokens))),
            Token::RShift => Ok(Some((BinaryOp::RShift, tokens))),
            Token::And => Ok(Some((BinaryOp::And, tokens))),
            Token::Or => Ok(Some((BinaryOp::Or, tokens))),
            Token::Eq => Ok(Some((BinaryOp::Equal, tokens))),
            Token::NotEq => Ok(Some((BinaryOp::NotEqual, tokens))),
            Token::Less => Ok(Some((BinaryOp::LessThan, tokens))),
            Token::LessEq => Ok(Some((BinaryOp::LessOrEqual, tokens))),
            Token::Great => Ok(Some((BinaryOp::GreaterThan, tokens))),
            Token::GreatEq => Ok(Some((BinaryOp::GreaterOrEqual, tokens))),
            Token::Assign => Ok(Some((BinaryOp::Assign, tokens))),
            Token::AddAssign => Ok(Some((BinaryOp::AddAssign, tokens))),
            Token::SubAssign => Ok(Some((BinaryOp::SubAssign, tokens))),
            Token::MultAssign => Ok(Some((BinaryOp::MultAssign, tokens))),
            Token::DivAssign => Ok(Some((BinaryOp::DivAssign, tokens))),
            Token::ModAssign => Ok(Some((BinaryOp::ModAssign, tokens))),
            Token::OrAssign => Ok(Some((BinaryOp::OrAssign, tokens))),
            Token::AndAssign => Ok(Some((BinaryOp::AndAssign, tokens))),
            Token::LShiftAssign => Ok(Some((BinaryOp::LShiftAssign, tokens))),
            Token::RShiftAssign => Ok(Some((BinaryOp::RShiftAssign, tokens))),
            Token::XorAssign => Ok(Some((BinaryOp::XorAssign, tokens))),
            Token::Ternary => Ok(Some((BinaryOp::Ternary, tokens))),
            _ => Ok(None),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    AddrOf,
    Deref,
}

impl UnaryOp {
    pub fn is_logical(&self) -> bool {
        *self == Self::Not
    }

    pub fn is_bitwise(&self) -> bool {
        matches!(self, Self::Complement)
    }

    pub fn does_assignment(&self) -> bool {
        matches!(
            self,
            UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec
        )
    }

    pub fn is_valid_for(&self, expr: &Expr) -> bool {
        !matches!(
            self,
            UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec
        ) || expr.is_lvalue()
    }

    pub fn consume_prefix(tokens: &[Token]) -> Result<(Self, &[Token])> {
        if let Some(token) = tokens.first() {
            match tokens {
                [Token::Ampersand, tokens @ ..] => Ok((Self::AddrOf, tokens)),
                [Token::Star, tokens @ ..] => Ok((Self::Deref, tokens)),
                [Token::Minus, tokens @ ..] => Ok((Self::Negate, tokens)),
                [Token::BitNot, tokens @ ..] => Ok((Self::Complement, tokens)),
                [Token::Not, tokens @ ..] => Ok((Self::Not, tokens)),
                [Token::Increment, tokens @ ..] => Ok((Self::PreInc, tokens)),
                [Token::Decrement, tokens @ ..] => Ok((Self::PreDec, tokens)),
                _ => bail!("Expected '-', '~', `++`, `--`, or '!', found '{}'", token),
            }
        } else {
            bail!("No remaining tokens")
        }
    }

    pub fn consume_postfix(tokens: &[Token]) -> Result<(Self, &[Token])> {
        if let Some(token) = tokens.first() {
            match tokens {
                [Token::Increment, tokens @ ..] => Ok((Self::PostInc, tokens)),
                [Token::Decrement, tokens @ ..] => Ok((Self::PostDec, tokens)),
                _ => bail!("Expected '-', '~', `++`, `--`, or '!', found '{}'", token),
            }
        } else {
            bail!("No remaining tokens")
        }
    }
}
