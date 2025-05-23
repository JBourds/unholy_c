mod const_eval;
mod gotos;
mod identifiers;
mod loops;
mod switch;
mod typechecking;

use std::{collections::HashMap, marker::PhantomData, rc::Rc};

use crate::ast;
use anyhow::{Error, Result, bail, ensure};

// Module internals
#[derive(Debug)]
enum Initial {}
#[derive(Debug)]
enum IdentResolution {}
#[derive(Debug)]
enum TypeChecking {}
#[derive(Debug)]
enum GotoValidation {}
#[derive(Debug)]
enum LoopLabelling {}
#[derive(Debug)]
enum SwitchLabelling {}

#[derive(Debug)]
struct SemaStage<T> {
    program: ast::Program,
    symbols: Option<typechecking::SymbolTable>,
    stage: PhantomData<T>,
}

// Public re-exports
pub mod tc {
    pub use super::typechecking::{Attribute, InitialValue, SymbolEntry, SymbolTable};
}

pub struct ValidAst {
    pub program: ast::Program,
    pub symbols: typechecking::SymbolTable,
}

pub fn validate(program: ast::Program) -> Result<ValidAst> {
    let stage = SemaStage {
        program,
        symbols: None,
        stage: PhantomData::<Initial>,
    };
    let stage = identifiers::validate(stage)?;
    let stage = switch::validate(stage)?;
    let stage = typechecking::validate(stage)?;
    let stage = gotos::validate(stage)?;
    let stage = loops::validate(stage)?;

    let SemaStage {
        program, symbols, ..
    } = stage;

    Ok(ValidAst {
        program,
        symbols: symbols.expect("Invalid symbol table."),
    })
}
