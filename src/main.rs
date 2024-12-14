mod ast;
mod lexer;

use std::process::Command;

use anyhow::{bail, ensure, Context, Result};
use clap::Parser;

#[derive(Parser, Debug)]
struct Args {
    file: String,

    #[arg(long)]
    lex: bool,

    #[arg(long)]
    parse: bool,

    #[arg(long)]
    codegen: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let file = args.file.clone();

    if !std::fs::exists(&file).with_context(|| format!("Cehcking file \"{}\" failed", file))? {
        bail!("File \"{}\" does not exist", file);
    }

    // Step 1: Run the preprocesser on the source file
    let preproccesed_file = format!("{}.i", file);

    let output = Command::new("gcc")
        .args(["-E", "-P", &file, "-o", &preproccesed_file])
        .output()
        .context("Failed to run gcc from preprocessing")?;

    ensure!(
        output.status.success(),
        "gcc exited with error code {}",
        output.status,
    );

    // Step 2: Compile the preprocessed source file

    let contents = std::fs::read_to_string(&preproccesed_file)
        .with_context(|| format!("Failed to read file: \"{}\"", preproccesed_file))?;

    // Cleanup
    std::fs::remove_file(&preproccesed_file)
        .with_context(|| format!("Failed to remove temp file \"{}\"", preproccesed_file))?;

    // Lex
    let tokens = lexer::Lexer::lex(&contents)?;

    if args.lex {
        println!("Lexed tokens:\n{:#?}", tokens);
        return Ok(());
    }

    // Parse
    let ast = ast::parse(&tokens)?;
    if args.parse {
        println!("Parsed AST:\n{:#?}", ast);
        return Ok(());
    }

    // Codegen

    // Step 3: Assemble and link the file

    Ok(())
}
