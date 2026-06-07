use super::*;

#[derive(Debug, PartialEq)]
pub struct StaticVariable {
    pub identifier: Rc<String>,
    pub global: bool,
    pub init: Vec<Rc<[u8]>>,
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
                    init: i.unwrap_bytes().to_vec(),
                }),
                sema::tc::InitialValue::Tentative => Some(StaticVariable {
                    identifier: name,
                    global: *external_linkage,
                    init: vec![vec![0; symbol.r#type.base.nbytes()].into()],
                }),
                sema::tc::InitialValue::None => None,
            },
            sema::tc::Attribute::Local => None,
            // Constants are `StaticConstant` not variable
            sema::tc::Attribute::Constant { .. } => None,
        }
    }
}
