use super::*;

#[derive(Debug, PartialEq)]
pub struct StaticVariable {
    pub identifier: Rc<String>,
    pub external_linkage: bool,
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
                    external_linkage: *external_linkage,
                    init: i.to_vec(),
                }),
                sema::tc::InitialValue::Tentative => Some(StaticVariable {
                    identifier: name,
                    external_linkage: *external_linkage,
                    init: vec![vec![0; symbol.r#type.base.nbytes()].into()],
                }),
                sema::tc::InitialValue::None => None,
            },
            sema::tc::Attribute::Local => None,
        }
    }
}
