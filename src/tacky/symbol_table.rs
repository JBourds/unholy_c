use super::*;
#[derive(Debug, Default)]
pub struct SymbolTable {
    pub table: HashMap<Rc<String>, SymbolEntry>,

    string_pool: HashMap<Rc<String>, Rc<String>>,
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
            string_pool: value.string_pool,
        }
    }
}

impl SymbolTable {
    pub fn get_string(&self, key: &Rc<String>) -> Option<&Rc<String>> {
        self.string_pool.get(key)
    }

    pub fn get(&self, key: &Rc<String>) -> Option<&SymbolEntry> {
        self.table.get(key)
    }

    #[allow(dead_code)]
    pub fn get_mut(&mut self, key: &Rc<String>) -> Option<&mut SymbolEntry> {
        self.table.get_mut(key)
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
