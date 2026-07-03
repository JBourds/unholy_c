use super::symbols::Scope;
use super::*;

#[derive(Debug)]
pub struct TypeTable {
    pub global: HashMap<Rc<String>, StructEntry>,
    scopes: Vec<HashMap<Rc<String>, StructEntry>>,
}

impl TypeTable {
    pub fn new_table() -> Self {
        Self {
            global: HashMap::new(),
            scopes: vec![],
        }
    }

    pub fn get(&self, key: &Rc<String>) -> Option<&StructEntry> {
        Self::get_local(&self.scopes, key).or(Self::get_global(&self.global, key))
    }

    pub fn get_mut(&mut self, key: &Rc<String>) -> Option<&mut StructEntry> {
        Self::get_mut_local(&mut self.scopes, key).or(Self::get_mut_global(&mut self.global, key))
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> Result<HashMap<Rc<String>, StructEntry>> {
        if self.scopes.is_empty() {
            bail!("Already in global scope, cannot pop symbol table")
        } else {
            Ok(self.scopes.pop().unwrap())
        }
    }

    pub fn declare_in_scope(
        &mut self,
        r#type: ast::Type,
        member_entries: Vec<MemberEntry>,
        scope: Scope,
    ) -> Result<()> {
        let (name, tag_type) = match &r#type {
            ast::Type {
                base: ast::BaseType::Struct { tag, .. },
                ..
            } => (Rc::clone(tag), StructOrUnion::Struct),
            ast::Type {
                base: ast::BaseType::Union { tag, .. },
                ..
            } => (Rc::clone(tag), StructOrUnion::Union),
            _ => unreachable!(),
        };

        if let Some(entry) = self.get_mut(&name) {
            // previous entry
            if !scope.shadows(&entry.scope) {
                ensure!(
                    tag_type == entry.tag_type,
                    "redefining {name} as {tag_type:?} when it was previously defined as {:?}",
                    entry.tag_type,
                );
                ensure!(
                    entry.members.is_empty() || member_entries.is_empty(),
                    "cannot define {name} with members twice in one scope"
                );
                if !member_entries.is_empty() {
                    entry.members = member_entries;
                }
                if r#type.alignment != NonZeroUsize::new(1).unwrap() && r#type.base.nbytes() != 0 {
                    entry.alignment = r#type.alignment.into();
                    entry.size = r#type.base.nbytes();
                }
            } else {
                // we do shadow
                self.insert_scope(
                    name,
                    StructEntry {
                        alignment: r#type.alignment.into(),
                        size: r#type.base.nbytes(),
                        members: member_entries,
                        tag_type,
                        scope: self.scope(),
                    },
                );
            }
        } else {
            self.insert_scope(
                name,
                StructEntry {
                    alignment: r#type.alignment.into(),
                    size: r#type.base.nbytes(),
                    members: member_entries,
                    tag_type,
                    scope: self.scope(),
                },
            );
        }

        Ok(())
    }

    pub fn insert_scope(&mut self, key: Rc<String>, entry: StructEntry) -> Option<StructEntry> {
        match entry.scope {
            Scope::Global => self.global.insert(key, entry),
            Scope::Local(frame) => self.scopes[frame].insert(key, entry),
        }
    }

    pub fn scope(&self) -> Scope {
        match self.scopes.len() {
            0 => Scope::Global,
            n => Scope::Local(n - 1),
        }
    }

    fn get_local<'a>(
        scopes: &'a Vec<HashMap<Rc<String>, StructEntry>>,
        key: &Rc<String>,
    ) -> Option<&'a StructEntry> {
        for scope in scopes.iter().rev() {
            if let Some(entry) = scope.get(key) {
                return Some(entry);
            }
        }
        None
    }

    fn get_global<'a>(
        globals: &'a HashMap<Rc<String>, StructEntry>,
        key: &Rc<String>,
    ) -> Option<&'a StructEntry> {
        globals.get(key)
    }

    fn get_mut_local<'a>(
        scopes: &'a mut Vec<HashMap<Rc<String>, StructEntry>>,
        key: &Rc<String>,
    ) -> Option<&'a mut StructEntry> {
        for scope in scopes.iter_mut().rev() {
            if let Some(entry) = scope.get_mut(key) {
                return Some(entry);
            }
        }
        None
    }

    fn get_mut_global<'a>(
        globals: &'a mut HashMap<Rc<String>, StructEntry>,
        key: &Rc<String>,
    ) -> Option<&'a mut StructEntry> {
        globals.get_mut(key)
    }
}

#[derive(Debug)]
pub struct StructEntry {
    pub alignment: usize,
    pub size: usize,
    pub members: Vec<MemberEntry>,
    pub tag_type: StructOrUnion,
    pub scope: Scope,
}

impl StructEntry {
    pub fn get_member(&self, name: &Rc<String>) -> Option<&MemberEntry> {
        self.members.iter().find(|member| member.name == *name)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum StructOrUnion {
    Struct,
    Union,
}

#[derive(Debug)]
pub struct MemberEntry {
    pub name: Rc<String>,
    pub r#type: ast::Type,
    pub offset: usize,
}
