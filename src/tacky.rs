use crate::ast;
use anyhow::{bail, Result};
use std::rc::Rc;

static mut TEMP_VAR_COUNTER: usize = 0;

#[allow(dead_code)]
fn reset_temp_counter() {
    unsafe {
        TEMP_VAR_COUNTER = 0;
    }
}

fn make_temp_var() -> Result<String> {
    unsafe {
        let n = TEMP_VAR_COUNTER;
        TEMP_VAR_COUNTER += 1;
        if TEMP_VAR_COUNTER < n {
            bail!("Integer overflow while creating temp variable name.")
        } else {
            Ok(format!("tmp.{}", n))
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    function: Function,
}

impl From<ast::Program<'_>> for Program {
    fn from(_node: ast::Program<'_>) -> Self {
        unimplemented!()
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    name: String,
    instructions: Vec<Instruction>,
}
impl From<ast::Function<'_>> for Function {
    fn from(_node: ast::Function<'_>) -> Self {
        unimplemented!()
    }
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Return(Option<Val>),
    Unary { op: UnaryOp, src: Val, dst: Val },
}
impl From<ast::Stmt> for Vec<Instruction> {
    fn from(node: ast::Stmt) -> Self {
        match node {
            ast::Stmt::Return(Some(expr)) => {
                let Expr {
                    mut instructions,
                    val,
                } = Expr::from(expr);
                instructions.push(Instruction::Return(Some(val)));
                instructions
            }
            ast::Stmt::Return(None) => {
                vec![Instruction::Return(None)]
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    instructions: Vec<Instruction>,
    val: Val,
}

impl From<ast::Expr> for Expr {
    fn from(node: ast::Expr) -> Self {
        match node {
            ast::Expr::Literal(v) => Self {
                instructions: vec![],
                val: Val::from(v),
            },
            ast::Expr::Unary(op, e) => {
                let Expr {
                    mut instructions,
                    val,
                } = Expr::from(*e);
                let dst = Val::Var(make_temp_var().unwrap().into());
                instructions.push(Instruction::Unary {
                    op: op.into(),
                    src: val,
                    dst: dst.clone(),
                });
                Expr {
                    instructions,
                    val: dst,
                }
            }
        }
    }
}

// TODO: Other types
#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Constant(i32),
    Var(Rc<String>),
}
impl From<ast::Literal> for Val {
    fn from(node: ast::Literal) -> Self {
        match node {
            ast::Literal::Int(i) => Self::Constant(i),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
}

// TODO: I created a separated version of this struct to prevent different
// modules from leaking into each other but these structs are identical
impl From<ast::Unary> for UnaryOp {
    fn from(node: ast::Unary) -> Self {
        match node {
            ast::Unary::Complement => Self::Complement,
            ast::Unary::Negate => Self::Negate,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_and_reset<F>(f: F)
    where
        F: Fn(),
    {
        f();
        reset_temp_counter();
    }

    #[test]
    fn test_return_literal() {
        test_and_reset(|| {
            let ast = ast::Stmt::Return(Some(ast::Expr::Literal(ast::Literal::Int(2))));
            let actual = Vec::<Instruction>::from(ast);
            let expected = vec![Instruction::Return(Some(Val::Constant(2)))];
            assert_eq!(actual, expected);
        })
    }

    #[test]
    fn test_return_unary() {
        test_and_reset(|| {
            let ast = ast::Stmt::Return(Some(ast::Expr::Unary(
                ast::Unary::Complement,
                Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
            )));
            let actual = Vec::<Instruction>::from(ast);
            let expected = vec![
                Instruction::Unary {
                    op: UnaryOp::Complement,
                    src: Val::Constant(2),
                    dst: Val::Var("tmp.0".to_string().into()),
                },
                Instruction::Return(Some(Val::Var("tmp.0".to_string().into()))),
            ];
            assert_eq!(actual, expected);
        });
    }
    #[test]
    fn test_return_nested_unary() {
        test_and_reset(|| {
            let ast = ast::Stmt::Return(Some(ast::Expr::Unary(
                ast::Unary::Negate,
                Box::new(ast::Expr::Unary(
                    ast::Unary::Complement,
                    Box::new(ast::Expr::Unary(
                        ast::Unary::Negate,
                        Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
                    )),
                )),
            )));
            let actual = Vec::<Instruction>::from(ast);
            let expected = vec![
                Instruction::Unary {
                    op: UnaryOp::Negate,
                    src: Val::Constant(2),
                    dst: Val::Var("tmp.0".to_string().into()),
                },
                Instruction::Unary {
                    op: UnaryOp::Complement,
                    src: Val::Var("tmp.0".to_string().into()),
                    dst: Val::Var("tmp.1".to_string().into()),
                },
                Instruction::Unary {
                    op: UnaryOp::Negate,
                    src: Val::Var("tmp.1".to_string().into()),
                    dst: Val::Var("tmp.2".to_string().into()),
                },
                Instruction::Return(Some(Val::Var("tmp.2".to_string().into()))),
            ];
            assert_eq!(actual, expected);
        });
    }
}
