use crate::codegen;
use anyhow::Result;
use std::fmt::Write;

pub trait AsmGen<W> {
    fn gen(writer: W, program: &codegen::Program) -> Result<()>
    where
        W: Write;
}

pub mod x64 {
    use super::AsmGen;
    use crate::codegen::{self, Operand};
    use anyhow::Result;
    use std::fmt::Write;

    pub struct Generator;

    impl<W> AsmGen<W> for Generator {
        fn gen(mut writer: W, program: &crate::codegen::Program) -> Result<()>
        where
            W: std::fmt::Write,
        {
            gen_program(&mut writer, program)?;
            Ok(())
        }
    }

    fn gen_program(w: &mut impl Write, program: &codegen::Program) -> Result<()> {
        w.write_str("\t.intel_syntax noprefix\n\n")?;

        gen_function(w, &program.function)?;

        w.write_str("\n\t.section .note.GNU-stack,\"\",@progbits\n")?;
        Ok(())
    }

    fn gen_function(w: &mut impl Write, function: &codegen::Function) -> Result<()> {
        w.write_fmt(format_args!("\t.globl {}\n", function.name))?;
        w.write_fmt(format_args!("{}:\n", function.name))?;

        for instr in function.instructions.iter() {
            gen_instruction(w, &instr.op)?;
        }

        Ok(())
    }

    fn gen_instruction(w: &mut impl Write, instr: &codegen::InstructionType) -> Result<()> {
        match instr {
            codegen::InstructionType::Mov { src, dst } => {
                let specifier = match dst {
                    Operand::StackOffset(offset) => match offset {
                        2 => "word ptr ",
                        4 => "dword ptr ",
                        8 => "qword ptr ",
                        _ => "",
                    },
                    _ => "",
                };
                let src = gen_operand(src);
                let dst = gen_operand(dst);
                w.write_fmt(format_args!("\tmov {specifier}{dst}, {src}\n"))?;
            }
            codegen::InstructionType::Ret => {
                w.write_str("\tmovq rsp, rbp\n")?;
                w.write_str("\tpopq rbp\n")?;
                w.write_str("\tret\n")?;
            }
            // TODO: Unhardcode size
            codegen::InstructionType::Unary { op, dst } => match op {
                codegen::UnaryOp::Complement => {
                    w.write_fmt(format_args!("\tnot dword ptr {}\n", gen_operand(dst)))?;
                }
                codegen::UnaryOp::Negate => {
                    w.write_fmt(format_args!("\tneg dword ptr {}\n", gen_operand(dst)))?;
                }
            },
            codegen::InstructionType::AllocStack(size) => {
                w.write_str("\tpushq rbp\n")?;
                w.write_str("\tmovq rbp, rsp\n")?;
                w.write_fmt(format_args!("\tsub rsp, {}\n", size))?;
            }
            // TODO: Unhardcode sizes
            codegen::InstructionType::Binary { op, src1, src2 } => {
                let op_str = match op {
                    codegen::BinaryOp::Add => "addl",
                    codegen::BinaryOp::Subtract => "subl",
                    codegen::BinaryOp::Multiply => "imull",
                    codegen::BinaryOp::BitAnd => todo!(),
                    codegen::BinaryOp::BitOr => todo!(),
                    codegen::BinaryOp::Xor => todo!(),
                    codegen::BinaryOp::LShift => todo!(),
                    codegen::BinaryOp::RShift => todo!(),
                    _ => unreachable!(),
                };
                w.write_fmt(format_args!(
                    "{op_str} {}, {}",
                    gen_operand(src1),
                    gen_operand(src2)
                ))?
            }
            codegen::InstructionType::Cdq => {
                w.write_str("\tcdq\n")?;
            }
            codegen::InstructionType::Idiv(operand) => match operand {
                codegen::Operand::Reg(r) => w.write_fmt(format_args!("\tidiv {r}"))?,
                codegen::Operand::StackOffset(offset) => {
                    w.write_fmt(format_args!("\tidiv {offset}"))?
                }
                _ => unreachable!(),
            },
        }
        Ok(())
    }

    fn gen_operand(operand: &codegen::Operand) -> String {
        match operand {
            codegen::Operand::Imm(i) => format!("{i}"),
            codegen::Operand::Reg(r) => format!("{r}"),
            codegen::Operand::StackOffset(offset) => format!("[rbp-{}]", offset),
            _ => todo!(),
        }
    }
}
