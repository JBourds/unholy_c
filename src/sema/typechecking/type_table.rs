use super::*;

#[derive(Debug)]
pub struct TypeTable {
    pub global: HashMap<Rc<String>, StructEntry>,
    scopes: Vec<HashMap<Rc<String>, StructEntry>>,
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
