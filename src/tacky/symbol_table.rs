use super::*;
#[derive(Debug, Default)]
pub struct SymbolTable {
    pub table: HashMap<Rc<String>, SymbolEntry>,
}

#[derive(Clone, Debug)]
pub struct SymbolEntry {
    pub r#type: ast::Type,
    pub attribute: sema::tc::Attribute,
}

impl From<sema::tc::SymbolTable> for SymbolTable {
    fn from(value: sema::tc::SymbolTable) -> Self {
        Self {
            table: value
                .global
                .into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        SymbolEntry {
                            r#type: v.r#type,
                            attribute: v.attribute,
                        },
                    )
                })
                .collect(),
        }
    }
}

impl SymbolTable {
    pub fn get(&self, key: &Rc<String>) -> Option<&SymbolEntry> {
        self.table.get(key)
    }

    pub fn new_entry(&mut self, key: Rc<String>, r#type: ast::Type) {
        let old_key = self.table.insert(
            Rc::clone(&key),
            SymbolEntry {
                r#type,
                attribute: sema::tc::Attribute::Local,
            },
        );

        assert!(
            old_key.is_none(),
            "Every new entry into SymbolTable should have a unique name!, but {key} did not!"
        );
    }
}
