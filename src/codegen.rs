use crate::tacky;
use anyhow::{Context, Result};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
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
        let mut instructions: Vec<Instruction> = node
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

        // Get stack offsets for each pseudoregister as we fix them up
        let mut stack_offsets = HashMap::new();
        let mut stack_bound = 0;
        let mut fixed_instructions: Vec<FixedInstruction> = instructions
            .drain(..)
            .map(|instr| FixedInstruction::new(instr, &mut stack_offsets, &mut stack_bound))
            .collect();
        fixed_instructions[0].0 = Instruction::AllocStack(stack_bound);

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
    AllocStack(usize),
    Ret,
}

pub struct FixedInstruction(pub Instruction);
impl FixedInstruction {
    fn new(
        instruction: Instruction,
        stack_offsets: &mut HashMap<Rc<String>, usize>,
        stack_bound: &mut usize,
    ) -> Self {
        let mut convert_operand_offset = |op| {
            if let Operand::Pseudo { ref name, size } = op {
                if let Entry::Vacant(e) = stack_offsets.entry(Rc::clone(name)) {
                    *stack_bound += size;
                    e.insert(*stack_bound);
                }
                // SAFETY: We just checked this condition
                unsafe { Operand::StackOffset(*stack_offsets.get(name).unwrap_unchecked()) }
            } else {
                op
            }
        };

        FixedInstruction(match instruction {
            Instruction::Mov { src, dst } => Instruction::Mov {
                src: convert_operand_offset(src),
                dst: convert_operand_offset(dst),
            },
            Instruction::Unary { op, dst } => Instruction::Unary {
                op,
                dst: convert_operand_offset(dst),
            },
            instr => instr,
        })
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
            tacky::Instruction::Unary { op, src, dst } => vec![
                Instruction::Mov {
                    src: src.into(),
                    dst: dst.into(),
                },
                Instruction::Unary {
                    op: op.into(),
                    dst: dst.into(),
                },
            ],
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(Reg),
    Pseudo { name: Rc<String>, size: usize },
    StackOffset(usize),
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
