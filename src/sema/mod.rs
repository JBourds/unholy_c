mod const_eval;
mod gotos;
mod identifiers;
mod loops;
mod switch;
pub mod typechecking;

use std::{collections::HashMap, marker::PhantomData, rc::Rc};

use crate::ast;
use anyhow::{bail, ensure, Error, Result};

enum Initial {}
enum IdentResolution {}
enum TypeChecking {}
enum GotoValidation {}
enum LoopLabelling {}
enum SwitchLabelling {}
pub enum Final {}

pub struct SemaStage<T> {
    pub program: ast::Program,
    pub symbols: Option<typechecking::SymbolTable>,
    stage: PhantomData<T>,
}

pub fn validate(program: ast::Program) -> Result<SemaStage<Final>> {
    let stage = SemaStage {
        program,
        symbols: None,
        stage: PhantomData::<Initial>,
    };
    let stage = identifiers::validate(stage)?;
    let stage = typechecking::validate(stage)?;
    let stage = gotos::validate(stage)?;
    let stage = switch::validate(stage)?;
    let stage = loops::validate(stage)?;

    Ok(SemaStage {
        program: stage.program,
        symbols: stage.symbols,
        stage: PhantomData::<Final>,
    })
}
