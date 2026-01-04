use anyhow::{Context, Result, bail};
use std::rc::Rc;

use super::{BinaryOp, Constant, Factor, Type, UnaryOp};
use crate::lexer::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Var(Rc<String>),
    Assignment {
        lvalue: Box<Expr>,
        rvalue: Box<Expr>,
    },
    Cast {
        target: Type,
        exp: Box<Expr>,
    },
    Constant(Constant),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Conditional {
        condition: Box<Expr>,
        then: Box<Expr>,
        r#else: Box<Expr>,
    },
    FunCall {
        name: Rc<String>,
        args: Vec<Expr>,
    },
    Subscript {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
}

impl Expr {
    pub fn is_modifiable_lvalue(&self, t: &Type) -> bool {
        let disallowed = [Type::is_array, Type::is_function];
        self.is_lvalue() && !disallowed.iter().any(|f| f(t))
    }

    pub fn is_lvalue(&self) -> bool {
        matches!(
            self,
            Self::Var(_)
                | Self::Unary {
                    op: UnaryOp::Deref,
                    ..
                }
                | Self::Subscript { .. }
        )
    }

    pub fn has_compound(&self) -> bool {
        match self {
            Expr::Cast { target: _, exp } => exp.has_compound(),
            Expr::Binary { op, .. } => op.compound_op().is_some(),
            _ => false,
        }
    }

    pub fn parse<'a>(tokens: &'a [Token], min_precedence: u32) -> Result<(Expr, &'a [Token])> {
        let (mut left, mut tokens) = Factor::parse(tokens)?;
        loop {
            let Some((operator, tokens_inner)) = BinaryOp::parse(tokens)? else {
                break;
            };

            if operator.precedence() < min_precedence {
                break;
            }

            if operator.does_assignment() {
                let (right, tokens_inner) = Expr::parse(tokens_inner, operator.precedence())?;
                if operator.compound_op().is_some() {
                    left = Expr::Binary {
                        op: operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                } else {
                    left = Expr::Assignment {
                        lvalue: Box::new(left),
                        rvalue: Box::new(right),
                    };
                }
                tokens = tokens_inner;
            } else if operator == BinaryOp::Ternary {
                let parse_conditional_middle =
                    |tokens: &'a [Token]| -> Result<(Expr, &'a [Token])> {
                        let (expr, tokens) = Expr::parse(tokens, 0)
                            .context("Failed to parse middle expression in ternary")?;

                        let tokens = match tokens.first() {
                            Some(Token::Colon) => &tokens[1..],
                            _ => bail!("Missing closing colon in ternary"),
                        };

                        Ok((expr, tokens))
                    };

                let (middle, tokens_inner) = parse_conditional_middle(tokens_inner)?;
                let (right, tokens_inner) = Expr::parse(tokens_inner, operator.precedence())?;

                left = Expr::Conditional {
                    condition: Box::new(left),
                    then: Box::new(middle),
                    r#else: Box::new(right),
                };

                tokens = tokens_inner;
            } else {
                let (right, tokens_inner) = Expr::parse(tokens_inner, operator.precedence() + 1)?;
                left = Expr::Binary {
                    op: operator,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                tokens = tokens_inner;
            }
        }
        Ok((left, tokens))
    }
}
