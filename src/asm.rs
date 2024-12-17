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
    use crate::codegen;
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
        w.write_str("\t.intel_syntax noprefix\n")?;

        gen_function(w, &program.function)?;

        w.write_str("\t.section .note.GNU-stack,\"\",@progbits\n")?;
        Ok(())
    }

    fn gen_function(w: &mut impl Write, function: &codegen::Function) -> Result<()> {
        w.write_fmt(format_args!("\t.globl {}\n", function.name))?;
        w.write_fmt(format_args!("{}:\n", function.name))?;

        for instr in function.instructions.iter() {
            w.write_char('\t')?;
            gen_instruction(w, instr)?;
        }

        Ok(())
    }

    fn gen_instruction(w: &mut impl Write, instr: &codegen::Instruction) -> Result<()> {
        match instr {
            codegen::Instruction::Mov { src, dst } => {
                let src = gen_operand(src);
                let dst = gen_operand(dst);
                w.write_fmt(format_args!("mov {dst}, {src}\n"))?;
            }
            codegen::Instruction::Ret => w.write_str("ret\n")?,
            _ => todo!(),
        }
        Ok(())
    }

    fn gen_operand(operand: &codegen::Operand) -> String {
        match operand {
            codegen::Operand::Imm(i) => format!("{i}"),
            codegen::Operand::Reg(r) => format!("{r}"),
            _ => todo!(),
        }
    }
}
