use crate::ast;
use anyhow::{Context, Result};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub function: Function,
}

impl TryFrom<&ast::Program<'_>> for Program {
    type Error = anyhow::Error;
    fn try_from(node: &ast::Program<'_>) -> Result<Self> {
        Ok(Self {
            function: Function::try_from(&node.function)
                .context("Failed to parse \"main\" function into TACKY representation")?,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub instructions: Vec<Instruction>,
}

impl Function {
    fn make_temp_var(name: Rc<String>, counter: &'_ mut usize) -> impl FnMut() -> String + use<'_> {
        move || {
            let n = *counter;
            *counter += 1;
            format!("{name}.{n}")
        }
    }
}

impl TryFrom<&ast::Function<'_>> for Function {
    type Error = anyhow::Error;
    // TODO: Use the return type and arguments for something.
    //  This is a try_from even though it cannot fail right now, assuming that
    //  there wil actually be a possibility of failure once we use all of the
    //  function information.
    fn try_from(node: &ast::Function<'_>) -> Result<Self> {
        let ast::Function {
            name, statements, ..
        } = node;
        let mut temp_var_counter = 0;
        let mut make_temp_var =
            Function::make_temp_var(Rc::new(name.to_string()), &mut temp_var_counter);
        let instructions = statements
            .iter()
            .fold(Vec::new(), |mut instructions, stmt| {
                instructions.extend(Instruction::parse_with(stmt, &mut make_temp_var));
                instructions
            });
        Ok(Self {
            name: Rc::new(name.to_string()),
            instructions,
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Return(Option<Val>),
    Unary {
        op: UnaryOp,
        src: Val,
        dst: Val,
    },
    Binary {
        op: BinaryOp,
        src1: Val,
        src2: Val,
        dst: Val,
    },
}

impl Instruction {
    fn parse_with(node: &ast::Stmt, make_temp_var: &mut impl FnMut() -> String) -> Vec<Self> {
        match node {
            ast::Stmt::Return(Some(expr)) => {
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with(expr, make_temp_var);
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

impl Expr {
    fn parse_with(node: &ast::Expr, make_temp_var: &mut impl FnMut() -> String) -> Expr {
        match node {
            ast::Expr::Literal(v) => Self {
                instructions: vec![],
                val: Val::from(v),
            },
            ast::Expr::Unary { op, expr } => {
                let Self {
                    mut instructions,
                    val,
                } = Expr::parse_with(expr.as_ref(), make_temp_var);
                let dst = Val::Var(make_temp_var().into());
                instructions.push(Instruction::Unary {
                    op: UnaryOp::from(op),
                    src: val,
                    dst: dst.clone(),
                });
                Expr {
                    instructions,
                    val: dst,
                }
            }
            ast::Expr::Binary { op, left, right } => {
                let Self {
                    mut instructions,
                    val: left_val,
                } = Self::parse_with(left.as_ref(), make_temp_var);
                let Self {
                    instructions: right_instructions,
                    val: right_val,
                } = Self::parse_with(right.as_ref(), make_temp_var);
                instructions.extend(right_instructions);

                let dst = Val::Var(make_temp_var().into());

                instructions.push(Instruction::Binary {
                    op: op.into(),
                    src1: left_val,
                    src2: right_val,
                    dst: dst.clone(),
                });
                Self {
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
impl From<&ast::Literal> for Val {
    fn from(node: &ast::Literal) -> Self {
        match node {
            ast::Literal::Int(i) => Self::Constant(*i),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Negate,
    Complement,
}

impl From<&ast::UnaryOp> for UnaryOp {
    fn from(node: &ast::UnaryOp) -> Self {
        match node {
            ast::UnaryOp::Complement => Self::Complement,
            ast::UnaryOp::Negate => Self::Negate,
            ast::UnaryOp::Not => todo!(),
        }
    }
}

#[derive(Debug, PartialEq)]
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
}

impl From<&ast::BinaryOp> for BinaryOp {
    fn from(node: &ast::BinaryOp) -> Self {
        match node {
            ast::BinaryOp::Add => Self::Add,
            ast::BinaryOp::Subtract => Self::Subtract,
            ast::BinaryOp::Multiply => Self::Multiply,
            ast::BinaryOp::Divide => Self::Divide,
            ast::BinaryOp::Remainder => Self::Remainder,
            ast::BinaryOp::BitAnd => Self::BitAnd,
            ast::BinaryOp::BitOr => Self::BitOr,
            ast::BinaryOp::Xor => Self::Xor,
            ast::BinaryOp::LShift => Self::LShift,
            ast::BinaryOp::RShift => Self::RShift,
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_return_literal() {
        let ast = ast::Stmt::Return(Some(ast::Expr::Literal(ast::Literal::Int(2))));
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_with(&ast, &mut make_temp_var);
        let expected = vec![Instruction::Return(Some(Val::Constant(2)))];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_return_unary() {
        let ast = ast::Stmt::Return(Some(ast::Expr::Unary {
            op: ast::UnaryOp::Complement,
            expr: Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
        }));
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_with(&ast, &mut make_temp_var);
        let expected = vec![
            Instruction::Unary {
                op: UnaryOp::Complement,
                src: Val::Constant(2),
                dst: Val::Var("test.0".to_string().into()),
            },
            Instruction::Return(Some(Val::Var("test.0".to_string().into()))),
        ];
        assert_eq!(actual, expected);
    }
    #[test]
    fn test_return_nested_unary() {
        let ast = ast::Stmt::Return(Some(ast::Expr::Unary {
            op: ast::UnaryOp::Negate,
            expr: Box::new(ast::Expr::Unary {
                op: ast::UnaryOp::Complement,
                expr: Box::new(ast::Expr::Unary {
                    op: ast::UnaryOp::Negate,
                    expr: Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
                }),
            }),
        }));
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_with(&ast, &mut make_temp_var);
        let expected = vec![
            Instruction::Unary {
                op: UnaryOp::Negate,
                src: Val::Constant(2),
                dst: Val::Var("test.0".to_string().into()),
            },
            Instruction::Unary {
                op: UnaryOp::Complement,
                src: Val::Var("test.0".to_string().into()),
                dst: Val::Var("test.1".to_string().into()),
            },
            Instruction::Unary {
                op: UnaryOp::Negate,
                src: Val::Var("test.1".to_string().into()),
                dst: Val::Var("test.2".to_string().into()),
            },
            Instruction::Return(Some(Val::Var("test.2".to_string().into()))),
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_binary_expr() {
        let ast_binary_expr = ast::Expr::Binary {
            op: ast::BinaryOp::Subtract,
            left: Box::new(ast::Expr::Binary {
                op: ast::BinaryOp::Multiply,
                left: Box::new(ast::Expr::Literal(ast::Literal::Int(1))),
                right: Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
            }),
            right: Box::new(ast::Expr::Binary {
                op: ast::BinaryOp::Multiply,
                left: Box::new(ast::Expr::Literal(ast::Literal::Int(3))),
                right: Box::new(ast::Expr::Binary {
                    op: ast::BinaryOp::Add,
                    left: Box::new(ast::Expr::Literal(ast::Literal::Int(4))),
                    right: Box::new(ast::Expr::Literal(ast::Literal::Int(5))),
                }),
            }),
        };
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let tacky_expr = Expr::parse_with(&ast_binary_expr, &mut make_temp_var);
        let expected = Expr {
            instructions: vec![
                Instruction::Binary {
                    op: BinaryOp::Multiply,
                    src1: Val::Constant(1),
                    src2: Val::Constant(2),
                    dst: Val::Var(Rc::new("test.0".to_string())),
                },
                Instruction::Binary {
                    op: BinaryOp::Add,
                    src1: Val::Constant(4),
                    src2: Val::Constant(5),
                    dst: Val::Var(Rc::new("test.1".to_string())),
                },
                Instruction::Binary {
                    op: BinaryOp::Multiply,
                    src1: Val::Constant(3),
                    src2: Val::Var(Rc::new("test.1".to_string())),
                    dst: Val::Var(Rc::new("test.2".to_string())),
                },
                Instruction::Binary {
                    op: BinaryOp::Subtract,
                    src1: Val::Var(Rc::new("test.0".to_string())),
                    src2: Val::Var(Rc::new("test.2".to_string())),
                    dst: Val::Var(Rc::new("test.3".to_string())),
                },
            ],
            val: Val::Var(Rc::new("test.3".to_string())),
        };
        assert_eq!(expected, tacky_expr);
    }
}
