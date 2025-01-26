use crate::codegen;
use anyhow::Result;
use std::fmt::Write;

pub trait AsmGen<W> {
    fn gen(writer: W, program: codegen::Program) -> Result<()>
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
        fn gen(mut writer: W, program: crate::codegen::Program) -> Result<()>
        where
            W: std::fmt::Write,
        {
            gen_program(&mut writer, program)?;
            Ok(())
        }
    }

    fn gen_program(w: &mut impl Write, program: codegen::Program) -> Result<()> {
        w.write_str("\t.intel_syntax noprefix\n\n")?;

        for f in program.functions.iter() {
            gen_function(w, f)?;
        }

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
        let get_specifier = |src: Option<&Operand>, dst: &Operand| {
            let size = match (src, dst) {
                (None, Operand::StackOffset { size, .. })
                | (Some(Operand::Imm(_)), Operand::StackOffset { size, .. }) => Some(*size),
                (Some(Operand::StackOffset { size, .. }), Operand::Imm(_)) => Some(*size),
                _ => None,
            };
            match size {
                None => "",
                Some(1) => "byte ptr ",
                Some(2) => "word ptr ",
                Some(4) => "dword ptr ",
                Some(8) => "qword ptr ",
                _ => unreachable!("Cannot have a destination size other than 1, 2, 4, or 8."),
            }
        };

        match instr {
            codegen::InstructionType::Mov { src, dst } => {
                w.write_fmt(format_args!(
                    "\tmov {}{dst}, {src}\n",
                    get_specifier(Some(src), dst)
                ))?;
            }
            codegen::InstructionType::Ret => {
                w.write_str("\tmov rsp, rbp\n")?;
                w.write_str("\tpop rbp\n")?;
                w.write_str("\tret\n")?;
            }
            codegen::InstructionType::Unary { op, dst } => {
                w.write_fmt(format_args!("\t{op} {}{dst}\n", get_specifier(None, dst)))?
            }
            codegen::InstructionType::AllocStack(size) => {
                w.write_str("\tpush rbp\n")?;
                w.write_str("\tmov rbp, rsp\n")?;
                w.write_fmt(format_args!("\tsub rsp, {}\n", size))?;
            }
            codegen::InstructionType::Binary {
                op,
                src1: src,
                src2: dst,
            } => {
                // Special case- if we are bitshifting then the "cl" register
                // can be the src2 operand but says nothing about the size of
                // the data it points to
                let specifier = match op {
                    codegen::BinaryOp::LShift | codegen::BinaryOp::RShift => {
                        get_specifier(None, dst)
                    }
                    _ => get_specifier(Some(src), dst),
                };
                w.write_fmt(format_args!("\t{op} {specifier}{dst}, {src}\n",))?
            }
            codegen::InstructionType::Cdq => {
                w.write_str("\tcdq\n")?;
            }
            codegen::InstructionType::Idiv(operand) => w.write_fmt(format_args!(
                "\tidiv {}{operand}\n",
                get_specifier(None, operand)
            ))?,
            codegen::InstructionType::Cmp { src, dst } => {
                w.write_fmt(format_args!(
                    "\tcmp {}{dst}, {src}\n",
                    get_specifier(Some(src), dst)
                ))?;
            }
            codegen::InstructionType::Jmp(label) => {
                w.write_fmt(format_args!("\tjmp .L{label}\n",))?;
            }
            codegen::InstructionType::JmpCC {
                cond_code,
                identifier,
            } => {
                w.write_fmt(format_args!("\tj{cond_code} .L{identifier}\n",))?;
            }
            codegen::InstructionType::SetCC { cond_code, dst } => {
                w.write_fmt(format_args!(
                    "\tset{cond_code} {}{dst}\n",
                    get_specifier(None, dst)
                ))?;
            }
            codegen::InstructionType::Label(label) => {
                w.write_fmt(format_args!(".L{label}:\n",))?;
            }
        }
        Ok(())
    }
}
