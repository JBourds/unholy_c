use crate::ast;
use crate::tacky;
use anyhow::{ensure, Result};
use std::fmt;
use std::rc::Rc;

pub fn gen<'a>(program: &'a ast::Program<'a>) -> Result<Program<'a>> {
    let prog = Program::consume(program)?;
    assert!(prog.len() == 1);
    Ok(prog.into_iter().next().unwrap())
}

pub trait AsmNode<'a> {
    type T;

    fn consume(node: &'a Self::T) -> Result<Vec<Self>>
    where
        Self: Sized;
}

#[derive(Debug, PartialEq)]
pub struct Program<'a> {
    pub function: Function<'a>,
}

impl<'a> AsmNode<'a> for Program<'a> {
    type T = ast::Program<'a>;

    fn consume(node: &'a Self::T) -> Result<Vec<Self>>
    where
        Self: Sized,
    {
        let func = Function::consume(&node.function)?;

        assert!(func.len() == 1);

        Ok(vec![Program {
            function: func.into_iter().next().unwrap(),
        }])
    }
}

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    pub name: &'a str,
    pub instructions: Vec<Instruction>,
}

impl<'a> AsmNode<'a> for Function<'a> {
    type T = ast::Function<'a>;

    fn consume(node: &'a Self::T) -> Result<Vec<Self>>
    where
        Self: Sized,
    {
        let instructions: Result<Vec<Instruction>> = node
            .statements
            .iter()
            .map(Instruction::consume)
            .try_fold(vec![], |mut unrolled, result| {
                unrolled.extend(result?);
                Ok(unrolled)
            });

        Ok(vec![Function {
            name: node.name,
            instructions: instructions?,
        }])
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

impl From<&tacky::UnaryOp> for UnaryOp {
    fn from(node: &tacky::UnaryOp) -> Self {
        match node {
            tacky::UnaryOp::Complement => Self::Complement,
            tacky::UnaryOp::Negate => Self::Negate,
            tacky::UnaryOp::Not => Self::Not,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reg {
    Eax,
    R10,
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eax => write!(f, "eax"),
            Self::R10 => write!(f, "r10"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Unary { op: UnaryOp, dst: Operand },
    AllocStack(i32),
    Ret,
}

impl<'a> AsmNode<'a> for Instruction {
    type T = ast::Stmt;

    fn consume(node: &'a Self::T) -> Result<Vec<Self>>
    where
        Self: Sized,
    {
        match node {
            ast::Stmt::Return(expr) => {
                let mut vec = vec![];
                if let Some(e) = expr {
                    let ops = match e {
                        ast::Expr::Literal(e) => Operand::consume(e)?,
                        _ => todo!(),
                    };
                    ensure!(ops.len() == 1);
                    // TODO: Is this the right register?
                    vec.push(Instruction::Mov {
                        src: ops.into_iter().next().unwrap(),
                        dst: Operand::Reg(Reg::Eax),
                    });
                }
                vec.push(Instruction::Ret);
                Ok(vec)
            }
        }
    }
}

impl From<&tacky::Instruction> for Vec<Instruction> {
    fn from(instruction: &tacky::Instruction) -> Vec<Instruction> {
        match instruction {
            tacky::Instruction::Return(None) => vec![Instruction::Ret],
            tacky::Instruction::Return(Some(ref val)) => vec![
                Instruction::Mov {
                    src: Operand::from(val),
                    dst: Operand::Reg(Reg::Eax),
                },
                Instruction::Ret,
            ],
            tacky::Instruction::Unary { op, src, dst } => {
                vec![
                    Instruction::Mov {
                        src: src.into(),
                        dst: dst.into(),
                    },
                    Instruction::Unary {
                        op: op.into(),
                        dst: dst.into(),
                    },
                ]
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(Reg),
    Pseudo(Rc<String>),
    Stack(i32),
}

impl<'a> AsmNode<'a> for Operand {
    type T = ast::Literal;

    fn consume(node: &'a Self::T) -> Result<Vec<Self>>
    where
        Self: Sized,
    {
        match *node {
            ast::Literal::Int(i) => Ok(vec![Operand::Imm(i)]),
        }
    }
}

impl From<&tacky::Val> for Operand {
    fn from(val: &tacky::Val) -> Self {
        match val {
            tacky::Val::Constant(i) => Self::Imm(*i),
            tacky::Val::Var(r) => Self::Pseudo(r.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tacky_ret_to_asm() {
        let tacky = tacky::Instruction::Return(Some(tacky::Val::Constant(2)));
        let expected = vec![
            Instruction::Mov {
                src: Operand::Imm(2),
                dst: Operand::Reg(Reg::Eax),
            },
            Instruction::Ret,
        ];
        let actual = Vec::<Instruction>::from(&tacky);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tacky_unary_to_asm() {
        let pseudo = Rc::new("pseudoreg".to_string());
        let tacky = tacky::Instruction::Unary {
            op: tacky::UnaryOp::Not,
            src: tacky::Val::Constant(2),
            dst: tacky::Val::Var(Rc::clone(&pseudo)),
        };
        let expected = vec![
            Instruction::Mov {
                src: Operand::Imm(2),
                dst: Operand::Pseudo(Rc::clone(&pseudo)),
            },
            Instruction::Unary {
                op: UnaryOp::Not,
                dst: Operand::Pseudo(Rc::clone(&pseudo)),
            },
        ];
        let actual = Vec::<Instruction>::from(&tacky);
        assert_eq!(expected, actual);
    }
}
