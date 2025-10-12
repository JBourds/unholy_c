use std::collections::HashSet;

use super::{Function, TopLevel, make_temp};
use crate::tacky;

#[derive(Debug)]
pub struct Program {
    pub top_level: Vec<TopLevel>,
    pub symbols: tacky::SymbolTable,
}

impl From<tacky::Program> for Program {
    fn from(prog: tacky::Program) -> Self {
        let mut top_level = vec![];
        let mut constants = HashSet::new();
        let mut counter = 0;
        let mut make_label = make_temp(&mut counter);
        for item in prog.top_level.into_iter() {
            match item {
                tacky::TopLevel::Fun(f) => {
                    let fun = Function::from_with_storage(
                        f,
                        &prog.symbols,
                        &mut constants,
                        &mut make_label,
                    );
                    top_level.push(TopLevel::Fun(fun));
                }
                tacky::TopLevel::Static(s) => top_level.push(TopLevel::StaticVariable(s.into())),
            }
        }
        top_level.extend(
            constants
                .into_iter()
                .map(TopLevel::StaticConstant)
                .collect::<Vec<_>>(),
        );
        Program {
            top_level,
            symbols: prog.symbols,
        }
    }
}
