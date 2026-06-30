use super::*;

use super::symbols::Scope;

#[derive(Debug)]
pub struct TypeTable {
    pub global: HashMap<Rc<String>, StructEntry>,
    scopes: Vec<HashMap<Rc<String>, StructEntry>>,
}

impl TypeTable {
    pub fn get(&self, key: &Rc<String>) -> Option<&StructEntry> {
        Self::get_local(&self.scopes, key).or(Self::get_global(&self.global, key))
    }

    pub fn declare_struct(&mut self, decl: &ast::StructDecl) -> Result<StructEntry> {
        todo!()
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
        decl: &ast::Declaration,
        scope: Scope,
    ) -> Result<StructEntry> {
        todo!()
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
}

#[derive(Debug)]
pub struct StructEntry {
    pub alignment: usize,
    pub size: usize,
    pub members: Vec<MemberEntry>,
    pub tag_type: StructOrUnion,
}

impl StructEntry {
    pub fn get_member(&self, name: Rc<String>) -> Option<&MemberEntry> {
        self.members.iter().find(|member| member.name == name)
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
