use std::rc::Rc;
use std::{collections::HashSet, num::NonZeroUsize};

use crate::{ast, sema::typechecking::MemberEntry};

use super::{SymbolTable, TypeTable, fixup_type};

use anyhow::{Context, Result, ensure};

pub fn typecheck_struct_decl(
    decl: ast::StructDecl,
    structs: &mut TypeTable,
) -> Result<ast::StructDecl> {
    let mut member_set = HashSet::new();
    for member in decl.members.iter() {
        ensure!(
            !member_set.contains(&member.name),
            "members cannot have the same name, member: {} struct: {}",
            member.name,
            decl.tag
        );
        ensure!(
            member.r#type.is_complete() && member.r#type.is_array_complete(),
            "member {} cannot have incomplete type {}",
            member.name,
            member.r#type
        );
        member_set.insert(Rc::clone(&member.name));
    }

    let mut members = vec![];
    let mut member_entries = vec![];
    let mut struct_size = 0;
    let mut struct_alignment = 1;
    for mut member in decl.members.into_iter() {
        member.r#type = fixup_type(member.r#type, structs);
        let member_alignment = member.r#type.alignment;
        let member_size = member.r#type.base.nbytes();
        let member_offset = round_up(struct_size, member_alignment.into());
        member_entries.push(MemberEntry {
            name: Rc::clone(&member.name),
            r#type: member.r#type.clone(),
            offset: member_offset,
        });

        struct_alignment = struct_alignment.max(member_alignment.into());
        struct_size = member_offset + member_size;

        members.push(member);
    }
    struct_size = round_up(struct_size, struct_alignment);

    let r#type = ast::Type {
        base: ast::BaseType::Struct {
            tag: Rc::clone(&decl.tag),
            size: struct_size,
        },
        alignment: NonZeroUsize::new(struct_alignment).unwrap(),
        is_const: false,
    };

    structs
        .declare_in_scope(r#type, member_entries.to_vec(), structs.scope())
        .context("failed to declare struct in scope")?;

    // fixup the types... AGAIN!
    for entry in member_entries.iter_mut() {
        entry.r#type = fixup_type(entry.r#type.clone(), structs);
    }
    if let Some(entry) = structs.get_mut(&decl.tag) {
        entry.members = member_entries;
    }
    Ok(ast::StructDecl {
        tag: decl.tag,
        members,
    })
}

pub fn typecheck_union_decl(
    decl: ast::UnionDecl,
    structs: &mut TypeTable,
) -> Result<ast::StructDecl> {
    todo!()
}

fn round_up(size: usize, alignment: usize) -> usize {
    assert!(alignment != 0);
    size.checked_add(alignment - 1)
        .unwrap()
        .checked_mul(alignment)
        .unwrap()
        .checked_div(alignment)
        .unwrap()
}
