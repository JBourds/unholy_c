use crate::codegen;
use anyhow::Result;
use std::fmt::Write;

pub trait AsmGen<W> {
    fn r#gen(writer: W, program: codegen::Program) -> Result<()>
    where
        W: Write;
}

pub mod x64 {
    use super::AsmGen;
    use crate::{
        codegen::{self, Operand},
        tacky,
    };
    use anyhow::{Result, bail};
    use std::fmt::Write;

    pub struct Generator;

    impl<W> AsmGen<W> for Generator {
        fn r#gen(mut writer: W, program: crate::codegen::Program) -> Result<()>
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
                codegen::TopLevel::StaticVariable(v) => gen_static_var(w, v, &program.symbols)?,
                codegen::TopLevel::StaticConstant(v) => gen_static_const(w, v)?,
            }
        }

        w.write_str("\n\t.section .note.GNU-stack,\"\",@progbits\n")?;
        Ok(())
    }

    fn gen_static_const(w: &mut impl Write, constant: codegen::StaticConstant) -> Result<()> {
        w.write_str("\t.section .rodata\n")?;
        w.write_fmt(format_args!("\t.align {}\n", constant.alignment))?;
        w.write_fmt(format_args!("\".L_{}\":\n", constant.id))?;
        w.write_fmt(format_args!("\t.double {}\n\n", constant.id))?;
        Ok(())
    }

    fn gen_static_var(
        w: &mut impl Write,
        var: codegen::StaticVariable,
        symbols: &tacky::SymbolTable,
    ) -> Result<()> {
        let symbol = symbols.get(&var.identifier).unwrap();
        if var.global {
            w.write_fmt(format_args!("\t.globl {}\n", var.identifier))?;
        }
        let init_value = var.init.expect("all statics have some init after tacky?");
        let in_bss = init_value.iter().all(|x| *x == 0);
        if in_bss {
            w.write_fmt(format_args!("\t.bss\n"))?;
        } else {
            w.write_fmt(format_args!("\t.data\n"))?;
        }
        w.write_fmt(format_args!("\t.align {}\n", symbol.r#type.alignment))?;
        w.write_fmt(format_args!("{}:\n", var.identifier))?;
        if in_bss {
            w.write_fmt(format_args!("\t.zero {}\n", symbol.r#type.size_of()))?;
        } else {
            // FIXME: This is not how this should be done
            let nbytes = symbol.r#type.size_of();
            match nbytes {
                8 => w.write_fmt(format_args!(
                    "\t.quad {}\n",
                    i64::from_le_bytes(init_value[0..nbytes].try_into().unwrap())
                ))?,
                4 => w.write_fmt(format_args!(
                    "\t.long {}\n",
                    i32::from_le_bytes(init_value[0..nbytes].try_into().unwrap())
                ))?,
                _ => unreachable!(),
            }
        }
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

    fn instr_suffix(r#type: codegen::AssemblyType) -> String {
        match r#type {
            codegen::AssemblyType::Float => "ss",
            codegen::AssemblyType::Double => "sd",
            _ => "",
        }
        .to_string()
    }

    fn get_specifier<'a>(src: Option<&Operand>, dst: &Operand) -> &'a str {
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
    }

    fn gen_instruction(w: &mut impl Write, instr: codegen::InstructionType) -> Result<()> {
        match instr {
            codegen::InstructionType::Mov { src, dst } => {
                let suffix = instr_suffix(codegen::AssemblyType::from(&dst));
                let specifier = get_specifier(Some(&src), &dst);
                w.write_fmt(format_args!("\tmov{suffix} {specifier}{dst}, {src}\n",))?;
            }
            codegen::InstructionType::Movsx { src, dst } => {
                w.write_fmt(format_args!(
                    "\tmovslq {}{dst}, {src}\n",
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
            codegen::InstructionType::Binary { op, src, dst } => {
                // Special case- if we are bitshifting then the "cl" register
                // can be the src2 operand but says nothing about the size of
                // the data it points to
                let specifier = match op {
                    codegen::BinaryOp::LShift | codegen::BinaryOp::Sar | codegen::BinaryOp::Shr => {
                        get_specifier(None, &dst)
                    }
                    _ => get_specifier(Some(&src), &dst),
                };
                let suffix = match (codegen::AssemblyType::from(&dst), &op) {
                    (codegen::AssemblyType::Double, codegen::BinaryOp::Xor) => "pd".to_string(),
                    (codegen::AssemblyType::Float, codegen::BinaryOp::Xor) => "ps".to_string(),
                    (asm_type, _) => instr_suffix(asm_type),
                };
                w.write_fmt(format_args!("\t{op}{suffix} {specifier}{dst}, {src}\n",))?
            }
            codegen::InstructionType::Cdq(section) => match section {
                codegen::RegSection::Dword => w.write_str("\tcdq\n")?,
                codegen::RegSection::Qword => w.write_str("\tcqo\n")?,
                _ => unreachable!(),
            },
            codegen::InstructionType::Idiv(operand) => {
                let specifier = get_specifier(None, &operand);
                w.write_fmt(format_args!("\tidiv {specifier}{operand}\n",))?
            }
            codegen::InstructionType::Div(operand) => {
                let specifier = get_specifier(None, &operand);
                w.write_fmt(format_args!("\tdiv {specifier}{operand}\n",))?
            }
            codegen::InstructionType::Cmp { src, dst } => {
                let specifier = get_specifier(Some(&src), &dst);
                let asm_type = codegen::AssemblyType::from(&dst);
                let suffix = instr_suffix(asm_type.clone());
                if asm_type.uses_xmm_regs() {
                    w.write_fmt(format_args!("\tcomi{suffix} {specifier}{dst}, {src}\n",))?;
                } else {
                    w.write_fmt(format_args!("\tcmp{suffix} {specifier}{dst}, {src}\n",))?;
                }
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
                    codegen::Operand::Reg(r) => {
                        codegen::Operand::Reg(r.as_section(codegen::RegSection::LowByte))
                    }
                    codegen::Operand::StackOffset { offset, r#type, .. } => {
                        codegen::Operand::StackOffset {
                            offset,
                            size: 1,
                            r#type,
                        }
                    }
                    _ => dst,
                };
                let specifier = get_specifier(None, &dst);
                w.write_fmt(format_args!("\tset{cond_code} {specifier}{dst}\n",))?;
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
                let specifier = get_specifier(None, &op);
                w.write_fmt(format_args!("\tpush {specifier}{op}\n",))?;
            }
            codegen::InstructionType::Pop(op) => match op {
                codegen::Operand::Reg(_) | codegen::Operand::StackOffset { .. } => {
                    let op = if let codegen::Operand::Reg(r) = op {
                        codegen::Operand::Reg(r.as_section(codegen::RegSection::Qword))
                    } else {
                        op
                    };
                    let specifier = get_specifier(None, &op);
                    w.write_fmt(format_args!("\tpop {specifier}{op}\n",))?;
                }
                _ => bail!(
                    "Cannot push stack to argument {} which is neither a register nor a memory location.",
                    op
                ),
            },
            codegen::InstructionType::Call(name) => {
                w.write_fmt(format_args!("\tcall \"{name}\"@PLT\n"))?;
            }
            codegen::InstructionType::Cvttsd2si { src, dst } => {
                let specifier = get_specifier(None, &src);
                w.write_fmt(format_args!("\tcvttsd2si {dst}, {specifier}{src}\n"))?;
            }
            codegen::InstructionType::Cvtsi2sd { src, dst } => {
                let specifier = get_specifier(None, &src);
                w.write_fmt(format_args!("\tcvtsi2sd {dst}, {specifier}{src}\n"))?;
            }
            codegen::InstructionType::DivDouble { src, dst } => {
                let specifier = get_specifier(None, &src);
                let suffix = instr_suffix(codegen::AssemblyType::from(&dst));
                w.write_fmt(format_args!("\tdiv{suffix} {specifier}{dst}, {src}\n"))?;
            }
            codegen::InstructionType::MovZeroExtend { .. } => unreachable!(),
        }
        Ok(())
    }
}
