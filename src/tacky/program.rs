use super::*;

#[derive(Debug)]
pub struct Program {
    pub top_level: Vec<TopLevel>,
    pub symbols: SymbolTable,
}

impl From<sema::ValidAst> for Program {
    fn from(ast: sema::ValidAst) -> Self {
        let sema::ValidAst { program, symbols } = ast;
        let mut top_level = vec![];
        for (name, symbol) in symbols.global.iter() {
            if let Some(r#static) = StaticVariable::from_symbol_with_name(Rc::clone(name), symbol) {
                top_level.push(TopLevel::StaticVariable(r#static));
            } else if let Some(constant) =
                StaticConstant::from_symbol_with_name(Rc::clone(name), symbol)
            {
                top_level.push(TopLevel::StaticConstant(constant));
            }
        }
        let mut symbols = SymbolTable::from(symbols);
        for decl in program.declarations.into_iter() {
            match decl {
                // Only declarations with bodies will be returned here.
                // We need to do some fixup so that if the definition for a
                // function was not marked static but the first declaration was
                // that the function gets defined as static.
                ast::Declaration::FunDecl(f) => {
                    if let Some(f) = Function::from_symbol(f, &mut symbols) {
                        top_level.push(TopLevel::Fun(f));
                    }
                }
                ast::Declaration::VarDecl(_) => {}
                ast::Declaration::StructDecl(..) => todo!(),
            };
        }
        Self { top_level, symbols }
    }
}
