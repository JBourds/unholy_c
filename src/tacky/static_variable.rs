use super::*;

#[derive(Debug, PartialEq)]
pub struct StaticVariable {
    pub identifier: Rc<String>,
    pub global: bool,
    pub init: Vec<StaticInit>,
}

impl StaticVariable {
    pub(crate) fn from_symbol_with_name(
        name: Rc<String>,
        symbol: &sema::tc::SymbolEntry,
    ) -> Option<Self> {
        match &symbol.attribute {
            sema::tc::Attribute::Fun { .. } => None,
            sema::tc::Attribute::Static {
                initial_value,
                external_linkage,
            } => match initial_value {
                sema::tc::InitialValue::Initial(i) => Some(StaticVariable {
                    identifier: name,
                    global: *external_linkage,
                    init: i.clone(),
                }),
                sema::tc::InitialValue::Tentative => Some(StaticVariable {
                    identifier: name,
                    global: *external_linkage,
                    init: vec![StaticInit::Zero(symbol.r#type.base.nbytes())],
                }),
                sema::tc::InitialValue::None => None,
            },
            sema::tc::Attribute::Local => None,
            // Constants are `StaticConstant` not variable
            sema::tc::Attribute::Constant { .. } => None,
        }
    }
}
