use super::*;

#[derive(Debug)]
pub struct Program {
    pub top_level: Vec<TopLevel>,
    pub symbols: SymbolTable,
}

impl From<sema::ValidAst> for Program {
    fn from(ast: sema::ValidAst) -> Self {
        let sema::ValidAst { program, symbols } = ast;
        let mut statics = vec![];
        for (name, symbol) in symbols.global.iter() {
            if let Some(r#static) = StaticVariable::from_symbol_with_name(Rc::clone(name), symbol) {
                statics.push(r#static);
            }
        }
        let mut symbols = SymbolTable::from(symbols);
        let mut valid_functions = vec![];
        for decl in program.declarations.into_iter() {
            match decl {
                // Only declarations with bodies will be returned here.
                // We need to do some fixup so that if the definition for a
                // function was not marked static but the first declaration was
                // that the function gets defined as static.
                ast::Declaration::FunDecl(f) => {
                    if let Some(f) = Function::from_symbol(f, &mut symbols) {
                        valid_functions.push(f);
                    }
                }
                ast::Declaration::VarDecl(_) => {}
            };
        }
        let top_level = valid_functions
            .into_iter()
            .map(TopLevel::Fun)
            .chain(statics.into_iter().map(TopLevel::Static))
            .collect::<Vec<TopLevel>>();
        Self { top_level, symbols }
    }
}
