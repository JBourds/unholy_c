use crate::ast;
use anyhow::{bail, ensure, Result};

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
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

impl<'a> AsmNode<'a> for Instruction {
    type T = ast::Stmt<'a>;

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
                    };
                    ensure!(ops.len() == 1);
                    vec.push(Instruction::Mov {
                        src: ops.into_iter().next().unwrap(),
                        dst: Operand::Register,
                    });
                }
                vec.push(Instruction::Ret);
                Ok(vec)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operand {
    Imm(i32),
    Register,
}

impl<'a> AsmNode<'a> for Operand {
    type T = ast::Literal<'a>;

    fn consume(node: &'a Self::T) -> Result<Vec<Self>>
    where
        Self: Sized,
    {
        match *node {
            ast::Literal::Int(i) => Ok(vec![Operand::Imm(i)]),
            ast::Literal::String(_) => bail!("Strings are unsupported"),
        }
    }
}
