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
        let ast::Function { name, items, .. } = node;
        let mut temp_var_counter = 0;
        let mut make_temp_var =
            Function::make_temp_var(Rc::new(name.to_string()), &mut temp_var_counter);
        let mut instructions = items.iter().fold(Vec::new(), |mut instructions, item| {
            instructions.extend(Instruction::parse_with(item, &mut make_temp_var));
            instructions
        });
        // Temporary fix suggested by the book for the case where a function
        // is supposed to return something but does not.
        instructions.push(Instruction::Return(Some(Val::Constant(0))));
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
    Copy {
        src: Val,
        dst: Val,
    },
    Jump(Rc<String>),
    JumpIfZero {
        condition: Val,
        target: Rc<String>,
    },
    JumpIfNotZero {
        condition: Val,
        target: Rc<String>,
    },
    Label(Rc<String>),
}

impl Instruction {
    fn parse_with(node: &ast::BlockItem, make_temp_var: &mut impl FnMut() -> String) -> Vec<Self> {
        match node {
            ast::BlockItem::Decl(decl) => {
                if let Some(init) = &decl.init {
                    let Expr {
                        mut instructions,
                        val: src,
                    } = Expr::parse_with(init, make_temp_var);
                    let dst = Val::Var(Rc::clone(&decl.name));
                    instructions.push(Instruction::Copy {
                        src,
                        dst: dst.clone(),
                    });
                    instructions
                } else {
                    vec![]
                }
            }
            ast::BlockItem::Stmt(stmt) => match stmt {
                ast::Stmt::Null => vec![],
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
                ast::Stmt::If {
                    condition,
                    then,
                    r#else,
                } => {
                    let (else_label, end_label) = {
                        let label = make_temp_var();
                        // This isn't needed and can be simplified... To Bad!
                        let Some((name, count)) = label.as_str().split_once('.') else {
                            unreachable!("label should always be name.count");
                        };
                        let else_label = format!("{name}.{count}.else");
                        let end_label = format!("{name}.{count}.end");
                        (Rc::new(else_label), Rc::new(end_label))
                    };
                    let Expr {
                        mut instructions,
                        val,
                    } = Expr::parse_with(condition, make_temp_var);

                    instructions.push(Self::JumpIfZero {
                        condition: val,
                        target: Rc::clone(match r#else {
                            Some(_) => &else_label,
                            None => &end_label,
                        }),
                    });

                    instructions.extend(Instruction::parse_with(
                        &ast::BlockItem::Stmt((**then).clone()),
                        make_temp_var,
                    ));

                    if let Some(r#else) = r#else {
                        instructions.push(Instruction::Jump(Rc::clone(&end_label)));
                        instructions.push(Instruction::Label(Rc::clone(&else_label)));
                        instructions.extend(Instruction::parse_with(
                            &ast::BlockItem::Stmt((**r#else).clone()),
                            make_temp_var,
                        ));
                    }

                    instructions.push(Instruction::Label(Rc::clone(&end_label)));

                    instructions
                }
                ast::Stmt::Expr(expr) => {
                    let Expr { instructions, .. } = Expr::parse_with(expr, make_temp_var);
                    instructions
                }
                ast::Stmt::Goto(label) => vec![Instruction::Jump(Rc::clone(label))],
                ast::Stmt::Label(label) => vec![Instruction::Label(Rc::clone(label))],
            },
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
                let dst = match op {
                    ast::UnaryOp::PreInc => {
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Add,
                            src1: val.clone(),
                            src2: Val::Constant(1),
                            dst: val.clone(),
                        });
                        val.clone()
                    }
                    ast::UnaryOp::PostInc => {
                        let dst = Val::Var(make_temp_var().into());
                        instructions.push(Instruction::Copy {
                            src: val.clone(),
                            dst: dst.clone(),
                        });
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Add,
                            src1: val.clone(),
                            src2: Val::Constant(1),
                            dst: val.clone(),
                        });
                        dst
                    }
                    ast::UnaryOp::PreDec => {
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Subtract,
                            src1: val.clone(),
                            src2: Val::Constant(1),
                            dst: val.clone(),
                        });
                        val.clone()
                    }
                    ast::UnaryOp::PostDec => {
                        let dst = Val::Var(make_temp_var().into());
                        instructions.push(Instruction::Copy {
                            src: val.clone(),
                            dst: dst.clone(),
                        });
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Subtract,
                            src1: val.clone(),
                            src2: Val::Constant(1),
                            dst: val.clone(),
                        });
                        dst
                    }
                    // Other operations have tacky unary op equivalents
                    _ => {
                        let dst = Val::Var(make_temp_var().into());
                        instructions.push(Instruction::Unary {
                            op: UnaryOp::from(op),
                            src: val,
                            dst: dst.clone(),
                        });
                        dst
                    }
                };
                Expr {
                    instructions,
                    val: dst,
                }
            }
            ast::Expr::Binary { op, left, right } => {
                if op.is_logical() {
                    // <instructions for e1>
                    // v1 = <result of e1>
                    let Self {
                        mut instructions,
                        val: left_val,
                    } = Self::parse_with(left.as_ref(), make_temp_var);

                    let label = {
                        let mut label = make_temp_var();
                        // FIXME: make_temp_var() should support making label names
                        label.push_str(match op {
                            ast::BinaryOp::And => ".false_label",
                            ast::BinaryOp::Or => ".true_label",
                            _ => unreachable!(),
                        });
                        label.into()
                    };

                    let make_jmp_instruction = |val, target| match op {
                        ast::BinaryOp::And => Instruction::JumpIfZero {
                            condition: val,
                            target,
                        },
                        ast::BinaryOp::Or => Instruction::JumpIfNotZero {
                            condition: val,
                            target,
                        },
                        _ => unreachable!(),
                    };

                    // JumpIfZero(v1, label)
                    instructions.push(make_jmp_instruction(left_val.clone(), Rc::clone(&label)));

                    // <instructions for e2>
                    // v2 = <result of e2>
                    let Self {
                        instructions: right_instructions,
                        val: right_val,
                    } = Self::parse_with(right.as_ref(), make_temp_var);
                    instructions.extend(right_instructions);

                    let dst = Val::Var(make_temp_var().into());
                    let end = {
                        // FIXME: Support label use case
                        let mut end = make_temp_var();
                        end.push_str(".end");
                        end.into()
                    };

                    let (result_nojmp, result_jmp) = match op {
                        ast::BinaryOp::And => (1, 0),
                        ast::BinaryOp::Or => (0, 1),
                        _ => unreachable!(),
                    };

                    // JumpIfZero(v2, false_label)
                    instructions.push(make_jmp_instruction(right_val.clone(), Rc::clone(&label)));

                    // result = 1
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(result_nojmp),
                        dst: dst.clone(),
                    });

                    // Jump(end)
                    instructions.push(Instruction::Jump(Rc::clone(&end)));

                    // Label(false_label)
                    instructions.push(Instruction::Label(Rc::clone(&label)));

                    // result = 0
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(result_jmp),
                        dst: dst.clone(),
                    });

                    // Label(end)
                    instructions.push(Instruction::Label(end));

                    Self {
                        instructions,
                        val: dst,
                    }
                } else if let Some(op) = op.compound_op() {
                    if let ast::Expr::Var(dst) = left.as_ref() {
                        let binary = ast::Expr::Binary {
                            op,
                            left: left.clone(),
                            right: right.clone(),
                        };
                        let Self {
                            mut instructions,
                            val: src,
                        } = Self::parse_with(&binary, make_temp_var);

                        instructions.push(Instruction::Copy {
                            src,
                            dst: Val::Var(Rc::clone(dst)),
                        });
                        Self {
                            instructions,
                            val: Val::Var(Rc::clone(dst)),
                        }
                    } else {
                        panic!("Cannot use compound assignment on non-variable value.")
                    }
                } else {
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
            ast::Expr::Var(name) => Self {
                instructions: vec![],
                val: Val::Var(Rc::clone(name)),
            },
            ast::Expr::Assignment { lvalue, rvalue } => {
                if let ast::Expr::Var(name) = lvalue.as_ref() {
                    let Self {
                        mut instructions,
                        val: src,
                    } = Self::parse_with(rvalue, make_temp_var);
                    let dst = Val::Var(Rc::clone(name));
                    instructions.push(Instruction::Copy {
                        src,
                        dst: dst.clone(),
                    });
                    Self {
                        instructions,
                        val: dst,
                    }
                } else {
                    panic!("Error: Cannot assign to rvalue.")
                }
            }
            ast::Expr::Conditional {
                condition,
                then,
                r#else,
            } => {
                let (result, e2_label, end_label) = {
                    let label = make_temp_var();
                    // This isn't needed and can be simplified... To Bad!
                    let Some((name, count)) = label.as_str().split_once('.') else {
                        unreachable!("label should always be name.count");
                    };
                    let e2_label = format!("{name}.{count}.e2");
                    let end_label = format!("{name}.{count}.end");
                    (Rc::new(label), Rc::new(e2_label), Rc::new(end_label))
                };
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with(condition, make_temp_var);

                instructions.push(Instruction::JumpIfZero {
                    condition: val,
                    target: Rc::clone(&e2_label),
                });

                let Expr {
                    instructions: e1_instructions,
                    val: e1_val,
                } = Expr::parse_with(then, make_temp_var);

                instructions.extend(e1_instructions);
                instructions.push(Instruction::Copy {
                    src: e1_val,
                    dst: Val::Var(Rc::clone(&result)),
                });

                instructions.push(Instruction::Jump(Rc::clone(&end_label)));
                instructions.push(Instruction::Label(Rc::clone(&e2_label)));

                let Expr {
                    instructions: e2_instructions,
                    val: e2_val,
                } = Expr::parse_with(r#else, make_temp_var);

                instructions.extend(e2_instructions);

                instructions.push(Instruction::Copy {
                    src: e2_val,
                    dst: Val::Var(Rc::clone(&result)),
                });

                instructions.push(Instruction::Label(end_label));

                Self {
                    instructions,
                    val: Val::Var(Rc::clone(&result)),
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
    Not,
}

impl From<&ast::UnaryOp> for UnaryOp {
    fn from(node: &ast::UnaryOp) -> Self {
        match node {
            ast::UnaryOp::Complement => Self::Complement,
            ast::UnaryOp::Negate => Self::Negate,
            ast::UnaryOp::Not => Self::Not,
            _ => unreachable!(),
        }
    }
}

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
            ast::BinaryOp::And => unreachable!("Logical operands are not Binary operands"),
            ast::BinaryOp::Or => unreachable!("Logical operands are not Binary operands"),
            ast::BinaryOp::Equal => Self::Equal,
            ast::BinaryOp::NotEqual => Self::NotEqual,
            ast::BinaryOp::LessThan => Self::LessThan,
            ast::BinaryOp::LessOrEqual => Self::LessOrEqual,
            ast::BinaryOp::GreaterThan => Self::GreaterThan,
            ast::BinaryOp::GreaterOrEqual => Self::GreaterOrEqual,
            ast::BinaryOp::Assign => Self::Assign,
            ast::BinaryOp::AddAssign => Self::AddAssign,
            ast::BinaryOp::SubAssign => Self::SubAssign,
            ast::BinaryOp::MultAssign => Self::MultAssign,
            ast::BinaryOp::DivAssign => Self::DivAssign,
            ast::BinaryOp::ModAssign => Self::ModAssign,
            ast::BinaryOp::AndAssign => Self::AndAssign,
            ast::BinaryOp::OrAssign => Self::OrAssign,
            ast::BinaryOp::XorAssign => Self::XorAssign,
            ast::BinaryOp::LShiftAssign => Self::LShiftAssign,
            ast::BinaryOp::RShiftAssign => Self::RShiftAssign,
            ast::BinaryOp::Ternary => {
                unreachable!("Ternary expressions are not true Binary operands")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_return_literal() {
        let ast = ast::BlockItem::Stmt(ast::Stmt::Return(Some(ast::Expr::Literal(
            ast::Literal::Int(2),
        ))));
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_with(&ast, &mut make_temp_var);
        let expected = vec![Instruction::Return(Some(Val::Constant(2)))];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_return_unary() {
        let ast = ast::BlockItem::Stmt(ast::Stmt::Return(Some(ast::Expr::Unary {
            op: ast::UnaryOp::Complement,
            expr: Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
        })));
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
        let ast = ast::BlockItem::Stmt(ast::Stmt::Return(Some(ast::Expr::Unary {
            op: ast::UnaryOp::Negate,
            expr: Box::new(ast::Expr::Unary {
                op: ast::UnaryOp::Complement,
                expr: Box::new(ast::Expr::Unary {
                    op: ast::UnaryOp::Negate,
                    expr: Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
                }),
            }),
        })));
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
