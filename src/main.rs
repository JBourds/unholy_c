mod asm;
mod ast;
mod codegen;
mod lexer;
mod sema;
mod tacky;
use std::{ffi::OsStr, io::Write, process::Command};

use anyhow::{anyhow, bail, ensure, Context, Result};
use clap::Parser;

use asm::AsmGen;

#[derive(Parser, Debug)]
struct Args {
    file: String,

    #[arg(long)]
    preprocess: bool,

    #[arg(long)]
    lex: bool,

    #[arg(long)]
    parse: bool,

    #[arg(long)]
    validate: bool,

    #[arg(long)]
    codegen: bool,

    #[arg(long)]
    tacky: bool,

    #[arg(long)]
    asm: bool,

    #[arg(short = 'c', long, default_value_t = false)]
    c: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    macro_rules! early_exit_call {
        ($func:ident, $($args:expr),*) => {{
            let v = $func($($args),*)?;
            if let Some(v) = v {
                v
            } else {
                return Ok(());
            }
        }};
    }

    let contents = early_exit_call!(preprocess, &args);
    let tokens = early_exit_call!(tokenize, &args, contents);
    let ast = early_exit_call!(parse_ast, &args, tokens);
    let ast_valid = early_exit_call!(validate_ast, &args, ast);
    let ir = early_exit_call!(generate_ir, &args, ast_valid);
    let codegen = early_exit_call!(generate_code, &args, ir);
    let asm = early_exit_call!(generate_asm, &args, codegen);
    assemble_and_link(&args, asm)
}

fn preprocess(args: &Args) -> Result<Option<String>> {
    if !std::fs::exists(&args.file)
        .with_context(|| format!("Checking file \"{}\" failed", args.file))?
    {
        bail!("File \"{}\" does not exist", args.file);
    }

    let preproccesed_file = format!("{}.i", args.file);

    let output = Command::new("gcc")
        .args(["-E", "-P", &args.file, "-o", &preproccesed_file])
        .output()
        .context("Failed to run gcc for preprocessing")?;

    ensure!(
        output.status.success(),
        "gcc exited with error code {}",
        output.status,
    );

    let contents = std::fs::read_to_string(&preproccesed_file)
        .with_context(|| format!("Failed to read file: \"{}\"", preproccesed_file))?;

    // Cleanup
    std::fs::remove_file(&preproccesed_file)
        .with_context(|| format!("Failed to remove temp file \"{}\"", preproccesed_file))?;

    if args.preprocess {
        println!("Preprocessed file:\n{}", contents);
        Ok(None)
    } else {
        Ok(Some(contents))
    }
}

fn tokenize(args: &Args, contents: String) -> Result<Option<Vec<lexer::Token>>> {
    let tokens = lexer::Lexer::lex(contents)?;
    if args.lex {
        println!("Lexed tokens:\n{:#?}", tokens);
        Ok(None)
    } else {
        Ok(Some(tokens))
    }
}

fn parse_ast(args: &Args, tokens: Vec<lexer::Token>) -> Result<Option<ast::Program>> {
    let ast = ast::parse(&tokens)?;
    if args.parse {
        println!("Parsed AST:\n{:#?}", ast);
        Ok(None)
    } else {
        Ok(Some(ast))
    }
}

fn validate_ast(args: &Args, ast: ast::Program) -> Result<Option<sema::SemaStage<sema::Final>>> {
    let ast_valid = sema::validate(ast)?;
    if args.validate {
        println!("Validated AST:\n{:#?}", ast_valid.program);
        Ok(None)
    } else {
        Ok(Some(ast_valid))
    }
}

fn generate_ir(args: &Args, stage: sema::SemaStage<sema::Final>) -> Result<Option<tacky::Program>> {
    let tacky = tacky::Program::from(stage);
    if args.tacky {
        println!("Generated Intermediate Representation:\n{:#?}", tacky);
        Ok(None)
    } else {
        Ok(Some(tacky))
    }
}

fn generate_code(args: &Args, tacky: tacky::Program) -> Result<Option<codegen::Program>> {
    let codegen = codegen::Program::from(tacky);
    if args.codegen {
        println!("Codegen:\n{:#?}", codegen);
        Ok(None)
    } else {
        Ok(Some(codegen))
    }
}

fn generate_asm(args: &Args, codegen: codegen::Program) -> Result<Option<String>> {
    let mut asm_text = String::new();
    asm::x64::Generator::gen(&mut asm_text, codegen)?;
    if args.asm {
        println!("Assembly:\n{}", asm_text);
        Ok(None)
    } else {
        Ok(Some(asm_text))
    }
}

fn assemble_and_link(args: &Args, asm: String) -> Result<()> {
    let asm_path = std::path::Path::new(&args.file).with_extension("s");
    // Ensure that the file is closed
    {
        let mut asm_file = std::fs::File::create(&asm_path).with_context(|| {
            format!(
                "Could not create assembly file: \"{}\" ",
                asm_path.to_string_lossy()
            )
        })?;

        asm_file.write(asm.as_bytes()).with_context(|| {
            format!(
                "Failed to write assembly to \"{}\"",
                asm_path.to_string_lossy()
            )
        })?;
    }

    let output_name =
        std::path::Path::new(&args.file).with_extension(if args.c { "o" } else { "" });
    let mut cmd = Command::new("gcc");
    cmd.args([
        asm_path.as_os_str(),
        OsStr::new("-o"),
        output_name.as_os_str(),
    ]);
    let output = if args.c {
        cmd.arg(OsStr::new("-c"));
        cmd.output().context("Failed to run gcc for assembly")?
    } else {
        cmd.output()
            .context("Failed to run gcc for assembly and linking")?
    };

    if !output.status.success() {
        return Err(anyhow!("gcc exited with error code: {}", output.status)).with_context(|| {
            format!(
                "gcc stdout output: \n{}",
                String::from_utf8_lossy(&output.stderr)
            )
        });
    }

    std::fs::remove_file(&asm_path).with_context(|| {
        format!(
            "Failed to remove temp file \"{}\"",
            asm_path.to_string_lossy()
        )
    })?;

    Ok(())
}
