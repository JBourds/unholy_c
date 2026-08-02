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
                    // Hack to make conditionals not freak out when dealing with void types
                    if name.as_str() == "DUMMY" {
                        return ast::Type::VOID;
                    }
                    unreachable!("Variable name '{name}' not found in symbol table");
                };
                entry.r#type.clone()
            }
        }
    }

    pub fn assert_var_get_name(&self) -> Rc<String> {
        match self {
            Val::Var(name) => name.clone(),
            _ => panic!("expected Val to be a var"),
        }
    }

    pub fn dummy() -> Self {
        Val::Var(Rc::new("DUMMY".to_string()))
    }
}

impl From<ast::Constant> for Val {
    fn from(node: ast::Constant) -> Self {
        Self::Constant(node)
    }
}
