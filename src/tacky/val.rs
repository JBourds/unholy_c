use super::*;
#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Constant(ast::Constant),
    Var(Rc<String>),
}

impl Val {
    pub fn get_type(&self, symbols: &SymbolTable) -> ast::Type {
        match self {
            Self::Constant(c) => c.get_type(),
            Self::Var(name) => {
                let Some(entry) = symbols.get(name) else {
                    unreachable!("Variable name '{name}' not found in symbol table");
                };
                entry.r#type.clone()
            }
        }
    }
}

impl From<ast::Constant> for Val {
    fn from(node: ast::Constant) -> Self {
        Self::Constant(node)
    }
}
