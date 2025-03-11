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
    use anyhow::{bail, Result};
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

        for entry in program.top_level.into_iter() {
            match entry {
                codegen::TopLevel::Fun(f) => gen_function(w, f)?,
                codegen::TopLevel::Static(v) => gen_static(w, v)?,
            }
        }

        w.write_str("\n\t.section .note.GNU-stack,\"\",@progbits\n")?;
        Ok(())
    }

    fn gen_static(w: &mut impl Write, var: codegen::StaticVariable) -> Result<()> {
        if var.global {
            w.write_fmt(format_args!("\t.globl {}\n", var.identifier))?;
        }
        let init_value = var.init.unwrap_or(0);
        if init_value == 0 {
            w.write_fmt(format_args!("\t.bss\n"))?;
        } else {
            w.write_fmt(format_args!("\t.data\n"))?;
        }
        // TODO: Unhardcode 4 here - take symbol info into this pass and calculate alignment
        w.write_fmt(format_args!("\t.align 4\n"))?;
        w.write_fmt(format_args!("{}:\n", var.identifier))?;
        w.write_fmt(format_args!("\t.long {}:\n", init_value))?;
        w.write_char('\n')?;

        Ok(())
    }

    fn gen_function(w: &mut impl Write, function: codegen::Function) -> Result<()> {
        if function.global {
            w.write_fmt(format_args!("\t.globl {}\n", function.name))?;
        }
        w.write_fmt(format_args!("\t.text\n"))?;
        w.write_fmt(format_args!("{}:\n", function.name))?;

        for instr in function.instructions.into_iter() {
            gen_instruction(w, instr.op)?;
        }
        w.write_char('\n')?;

        Ok(())
    }

    fn gen_instruction(w: &mut impl Write, instr: codegen::InstructionType) -> Result<()> {
        let get_specifier = |src: Option<&Operand>, dst: &Operand| {
            let size = match (src, dst) {
                (None, Operand::StackOffset { size, .. } | Operand::Data { size, .. })
                | (
                    Some(Operand::Imm(_)),
                    Operand::StackOffset { size, .. } | Operand::Data { size, .. },
                ) => Some(*size),
                (
                    Some(Operand::StackOffset { size, .. } | Operand::Data { size, .. }),
                    Operand::Imm(_),
                ) => Some(*size),
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
                    get_specifier(Some(&src), &dst)
                ))?;
            }
            codegen::InstructionType::Ret => {
                w.write_str("\tmov rsp, rbp\n")?;
                w.write_str("\tpop rbp\n")?;
                w.write_str("\tret\n")?;
            }
            codegen::InstructionType::Unary { op, dst } => {
                w.write_fmt(format_args!("\t{op} {}{dst}\n", get_specifier(None, &dst)))?
            }
            codegen::InstructionType::AllocStack(size) => {
                w.write_fmt(format_args!("\tsub rsp, {size}\n"))?;
            }
            codegen::InstructionType::DeAllocStack(size) => {
                w.write_fmt(format_args!("\tadd rsp, {size}\n"))?;
            }
            codegen::InstructionType::Binary {
                op,
                src,
                dst,
            } => {
                // Special case- if we are bitshifting then the "cl" register
                // can be the src2 operand but says nothing about the size of
                // the data it points to
                let specifier = match op {
                    codegen::BinaryOp::LShift | codegen::BinaryOp::RShift => {
                        get_specifier(None, &dst)
                    }
                    _ => get_specifier(Some(&src), &dst),
                };
                w.write_fmt(format_args!("\t{op} {specifier}{dst}, {src}\n",))?
            }
            codegen::InstructionType::Cdq => {
                w.write_str("\tcdq\n")?;
            }
            codegen::InstructionType::Idiv(operand) => w.write_fmt(format_args!(
                "\tidiv {}{operand}\n",
                get_specifier(None, &operand)
            ))?,
            codegen::InstructionType::Cmp { src, dst } => {
                w.write_fmt(format_args!(
                    "\tcmp {}{dst}, {src}\n",
                    get_specifier(Some(&src), &dst)
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
                let dst = match dst {
                    codegen::Operand::Reg(r) => codegen::Operand::Reg(r.as_section(codegen::RegSection::LowByte)),
                    codegen::Operand::StackOffset { offset, .. } => codegen::Operand::StackOffset { offset, size: 1 },
                    _ => dst
                };
                w.write_fmt(format_args!(
                    "\tset{cond_code} {}{dst}\n",
                    get_specifier(None, &dst)
                ))?;
            }
            codegen::InstructionType::Label(label) => {
                w.write_fmt(format_args!(".L{label}:\n",))?;
            }
            // Push and pop instruction works with 64-bit arguments. This means:
            //  - Constants will be pushed as 8-bytes (Ok)
            //  - Memory offsets < 8-bytes will include garbage after (Not great, but alright)
            //  - Registers have to be rewritten as their 64-bit equivalents here (Annoying)
            // Clone since we do have to mutate register section
            codegen::InstructionType::Push(op) => {
                let op = if let codegen::Operand::Reg(r) = op {
                    codegen::Operand::Reg(r.as_section(codegen::RegSection::Qword))
                } else {
                    op
                };
                w.write_fmt(format_args!("\tpush {}{op}\n", get_specifier(None, &op)))?;
            }
            codegen::InstructionType::Pop(op) => {
                match op {
                    codegen::Operand::Reg(_) | codegen::Operand::StackOffset { .. } => {
                        let op = if let codegen::Operand::Reg(r) = op {
                            codegen::Operand::Reg(r.as_section(codegen::RegSection::Qword))
                        } else {
                            op
                        };
                        w.write_fmt(format_args!("\tpop {}{op}\n", get_specifier(None, &op)))?;
                    }
                    _ => bail!("Cannot push stack to argument {} which is neither a register nor a memory location.", op)
                }
            }
            codegen::InstructionType::Call(name) => {
                w.write_fmt(format_args!("\tcall {name}@PLT\n"))?;
            }
        }
        Ok(())
    }
}
