mod asm;
mod ast;
mod codegen;
mod lexer;
mod tacky;

use std::{ffi::OsStr, io::Write, process::Command};

use anyhow::{anyhow, bail, ensure, Context, Result};
use clap::Parser;

use asm::AsmGen;

#[derive(Parser, Debug)]
struct Args {
    file: String,

    #[arg(long)]
    lex: bool,

    #[arg(long)]
    parse: bool,

    #[arg(long)]
    codegen: bool,

    #[arg(long)]
    tacky: bool,

    #[arg(long)]
    asm: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Step 1: Run the preprocesser on the source file
    let contents = preprocess(&args)?;

    // Step 2: Compile the preprocessed source file
    let Some((_, _, asm_nodes, _)) = lex_parse_codegen_tacky(&args, contents)? else {
        return Ok(());
    };

    // Step 2.5: Turn AsmNodes to asm text
    let Some(asm) = gen_asm::<std::string::String, asm::x64::Generator>(&args, asm_nodes)? else {
        return Ok(());
    };

    // Step 3: Assemble and link the file
    assemble_and_link(&args, &asm)?;

    Ok(())
}

fn preprocess(args: &Args) -> Result<&'static str> {
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

    Ok(contents.leak())
}

type LexTokens = &'static [lexer::Token<'static>];
type AstNodes = &'static ast::Program<'static>;
type AsmNodes = &'static codegen::Program;
type TackyNodes = &'static tacky::Program;

fn lex_parse_codegen_tacky(
    args: &Args,
    contents: &'static str,
) -> Result<Option<(LexTokens, AstNodes, AsmNodes, TackyNodes)>> {
    // Lex
    let tokens: &'static [lexer::Token<'static>] =
        Box::leak(Box::new(lexer::Lexer::lex(contents)?));

    if args.lex {
        println!("Lexed tokens:\n{:#?}", tokens);
        return Ok(None);
    }

    // Parse
    let ast: &'static ast::Program<'static> = Box::leak(Box::new(ast::parse(tokens)?));
    if args.parse {
        println!("Parsed AST:\n{:#?}", ast);
        return Ok(None);
    }

    // Tacky
    let tacky: &'static tacky::Program = Box::leak(Box::new(tacky::Program::try_from(ast)?));
    if args.tacky {
        println!("Tacky Generation:\n{:#?}", tacky);
        return Ok(None);
    }

    // Codegen
    let asm: &'static codegen::Program = Box::leak(Box::new(codegen::Program::try_from(tacky)?));
    if args.codegen {
        println!("Generated asm nodes:\n{:#?}", asm);
        return Ok(None);
    }

    Ok(Some((tokens, ast, asm, tacky)))
}

fn gen_asm<W: std::fmt::Write, T: AsmGen<W>>(
    args: &Args,
    asm: &'static codegen::Program,
) -> Result<Option<String>> {
    let mut asm_txt = String::new();

    asm::x64::Generator::gen(&mut asm_txt, asm)?;

    if args.asm {
        println!("Generated asm:\n{asm_txt}");
        return Ok(None);
    }

    Ok(Some(asm_txt))
}

fn assemble_and_link(args: &Args, asm: &str) -> Result<()> {
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

    let output_name = std::path::Path::new(&args.file).with_extension("");

    let output = Command::new("gcc")
        .args([
            asm_path.as_os_str(),
            OsStr::new("-o"),
            output_name.as_os_str(),
        ])
        .output()
        .context("Failed to run gcc for assembly and linking")?;

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
