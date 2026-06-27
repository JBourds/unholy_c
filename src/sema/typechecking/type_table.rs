use super::*;

#[derive(Debug)]
pub struct TypeTable {
    pub global: HashMap<Rc<String>, StructEntry>,
    scopes: Vec<HashMap<Rc<String>, StructEntry>>,
}

#[derive(Debug)]
pub struct StructEntry {
    alignment: usize,
    size: usize,
    members: Vec<MemberEntry>,
}

impl StructEntry {
    pub fn get_member(&self, name: Rc<String>) -> Option<&MemberEntry> {
        self.members.iter().find(|member| member.name == name)
    }
}

#[derive(Debug)]
pub struct MemberEntry {
    name: Rc<String>,
    r#type: ast::Type,
    offset: usize,
}
