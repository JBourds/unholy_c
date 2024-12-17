use crate::tacky;
use anyhow::{Context, Result};
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub function: Function,
}

impl TryFrom<&tacky::Program> for Program {
    type Error = anyhow::Error;
    fn try_from(node: &tacky::Program) -> Result<Self> {
        let function = Function::try_from(&node.function)
            .context("Failed to compile intermediate representation into assembly nodes.")?;
        Ok(Program { function })
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub instructions: Vec<Instruction>,
}

impl TryFrom<&tacky::Function> for Function {
    type Error = anyhow::Error;
    fn try_from(node: &tacky::Function) -> Result<Self> {
        // Mutable since we adjust the `AllocStack` instruction once we have
        // done compiler passes to determine actual required stack offset
        let instructions: Vec<Instruction> = node
            .instructions
            .iter()
            .map(Vec::<Instruction>::try_from)
            .try_fold(vec![Instruction::AllocStack(0)], |mut unrolled, result| {
                unrolled.extend(result.context(
                    "Failed to parse assembly function from intermediate representation.",
                )?);
                Ok::<Vec<Instruction>, Self::Error>(unrolled)
            })
            .context("Failed to generate function definition from statements.")?;

        // Get stack offsets for each pseudoregister
        //let mut offset = 0;
        //let stack_sizes = instructions
        //    .iter()
        //    .skip(1)
        //    .fold(HashMap::new(), |mut map, instr| {
        //        instr.pseudoregisters().iter().for_each(|op| {
        //            if let Some(Operand::Pseudo { name, size }) = op {
        //                if let Entry::Vacant(e) = map.entry(&name) {
        //                    offset -= size;
        //                    e.insert(offset);
        //                }
        //            }
        //        });
        //        map
        //    });

        // TODO: Fixup instructions

        // TODO: Unhardcode the assumption this is 4 size

        Ok(Function {
            name: Rc::clone(&node.name),
            instructions,
        })
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

impl Instruction {
    // TODO: Is 3 the max number of args for an instruction?
    #[allow(dead_code)]
    fn pseudoregisters(&self) -> [Option<Operand>; 3] {
        match self {
            Instruction::Mov { src, dst } => match (src, dst) {
                (
                    Operand::Pseudo { name: r1, size: s1 },
                    Operand::Pseudo { name: r2, size: s2 },
                ) => [
                    Some(Operand::Pseudo {
                        name: Rc::clone(r1),
                        size: *s1,
                    }),
                    Some(Operand::Pseudo {
                        name: Rc::clone(r2),
                        size: *s2,
                    }),
                    None,
                ],
                _ => [None, None, None],
            },
            Instruction::Unary { op: _, dst } => {
                if let Operand::Pseudo { name, size } = dst {
                    [
                        Some(Operand::Pseudo {
                            name: Rc::clone(name),
                            size: *size,
                        }),
                        None,
                        None,
                    ]
                } else {
                    [None, None, None]
                }
            }
            _ => [None, None, None],
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
    Pseudo { name: Rc<String>, size: usize },
    Stack(i32),
}

// TODO: Unhardcode size of 4
impl From<&tacky::Val> for Operand {
    fn from(val: &tacky::Val) -> Self {
        match val {
            tacky::Val::Constant(i) => Self::Imm(*i),
            tacky::Val::Var(r) => Self::Pseudo {
                name: Rc::clone(r),
                size: 4,
            },
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
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
            Instruction::Unary {
                op: UnaryOp::Not,
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
        ];
        let actual = Vec::<Instruction>::from(&tacky);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_stack_pass() {
        let pseudo = Rc::new("pseudoreg".to_string());
        let tacky = tacky::Instruction::Unary {
            op: tacky::UnaryOp::Not,
            src: tacky::Val::Constant(2),
            dst: tacky::Val::Var(Rc::clone(&pseudo)),
        };
        let expected = vec![
            Instruction::Mov {
                src: Operand::Imm(2),
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
            Instruction::Unary {
                op: UnaryOp::Not,
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
        ];
        let actual = Vec::<Instruction>::from(&tacky);

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_instruction_fixup() {
        let pseudo = Rc::new("pseudoreg".to_string());
        let tacky = tacky::Instruction::Unary {
            op: tacky::UnaryOp::Not,
            src: tacky::Val::Constant(2),
            dst: tacky::Val::Var(Rc::clone(&pseudo)),
        };
        let expected = vec![
            Instruction::Mov {
                src: Operand::Imm(2),
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
            Instruction::Unary {
                op: UnaryOp::Not,
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
        ];
        let actual = Vec::<Instruction>::from(&tacky);
        assert_eq!(expected, actual);
    }
}
