use crate::ast;

use anyhow::{bail, Context, Result};
use std::ops::{BitAnd, BitOr, BitXor};

pub fn eval(expr: ast::Expr) -> Result<ast::Literal> {
    match expr {
        ast::Expr::Var(_) => bail!("Variables cannot be constant evaluated"),
        ast::Expr::Assignment { .. } => {
            bail!("Assignment cannot be constant evaluated")
        }
        ast::Expr::Literal(literal) => Ok(literal),
        ast::Expr::Unary { op, expr } => eval_unary(op, *expr).context("UnaryOp const eval failed"),
        ast::Expr::Binary { op, left, right } => {
            eval_binary(op, *left, *right).context("BinaryOp const eval failed")
        }
        ast::Expr::Conditional {
            condition,
            then,
            r#else,
        } => {
            let condition = eval(*condition)?;
            let ast::Literal::Int(val) = condition;
            if val > 0 {
                eval(*then)
            } else {
                eval(*r#else)
            }
        }
        _ => unimplemented!(),
    }
}

fn eval_unary(op: ast::UnaryOp, expr: ast::Expr) -> Result<ast::Literal> {
    let literal = eval(expr)?;
    let ast::Literal::Int(val) = literal;
    let new_val = match op {
        ast::UnaryOp::Complement => !val,
        ast::UnaryOp::Negate => -val,
        ast::UnaryOp::Not => !val,
        ast::UnaryOp::PreInc
        | ast::UnaryOp::PreDec
        | ast::UnaryOp::PostInc
        | ast::UnaryOp::PostDec => bail!("{op:#?} is not allowed in const expresion"),
    };
    Ok(ast::Literal::Int(new_val))
}

fn eval_binary(op: ast::BinaryOp, left: ast::Expr, right: ast::Expr) -> Result<ast::Literal> {
    match op {
        ast::BinaryOp::And => {
            let left_val = get_value(eval(left)?);
            if left_val == 0 {
                return Ok(ast::Literal::Int(0));
            }
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(if right_val > 0 { 1 } else { 0 }))
        }
        ast::BinaryOp::Or => {
            let left_val = get_value(eval(left)?);
            if left_val > 0 {
                return Ok(ast::Literal::Int(1));
            }
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(if right_val > 0 { 1 } else { 0 }))
        }
        ast::BinaryOp::Add => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(left_val.wrapping_add(right_val)))
        }
        ast::BinaryOp::Subtract => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(left_val.wrapping_sub(right_val)))
        }
        ast::BinaryOp::Multiply => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(left_val.wrapping_mul(right_val)))
        }
        ast::BinaryOp::Divide => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(left_val.wrapping_div(right_val)))
        }
        ast::BinaryOp::Remainder => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(left_val.wrapping_rem(right_val)))
        }
        ast::BinaryOp::BitAnd => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(left_val.bitand(right_val)))
        }
        ast::BinaryOp::BitOr => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(left_val.bitor(right_val)))
        }
        ast::BinaryOp::Xor => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(left_val.bitxor(right_val)))
        }
        ast::BinaryOp::LShift => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(left_val << right_val))
        }
        ast::BinaryOp::RShift => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(left_val << right_val))
        }
        ast::BinaryOp::Equal => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(if left_val == right_val { 1 } else { 0 }))
        }
        ast::BinaryOp::NotEqual => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int(if left_val != right_val { 1 } else { 0 }))
        }
        ast::BinaryOp::LessThan => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int((left_val < right_val) as i32))
        }
        ast::BinaryOp::LessOrEqual => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int((left_val <= right_val) as i32))
        }
        ast::BinaryOp::GreaterThan => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int((left_val > right_val) as i32))
        }
        ast::BinaryOp::GreaterOrEqual => {
            let left_val = get_value(eval(left)?);
            let right_val = get_value(eval(right)?);
            Ok(ast::Literal::Int((left_val >= right_val) as i32))
        }
        ast::BinaryOp::Assign
        | ast::BinaryOp::AddAssign
        | ast::BinaryOp::SubAssign
        | ast::BinaryOp::MultAssign
        | ast::BinaryOp::DivAssign
        | ast::BinaryOp::ModAssign
        | ast::BinaryOp::AndAssign
        | ast::BinaryOp::OrAssign
        | ast::BinaryOp::XorAssign
        | ast::BinaryOp::LShiftAssign
        | ast::BinaryOp::RShiftAssign => bail!("Assignment cannot happen in const expression"),
        ast::BinaryOp::Ternary => {
            unreachable!("Ternary expressions are not true binary operands.")
        }
    }
}

fn get_value(lit: ast::Literal) -> i32 {
    match lit {
        ast::Literal::Int(v) => v,
    }
}
