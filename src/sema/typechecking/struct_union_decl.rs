use std::collections::HashSet;
use std::rc::Rc;

use crate::{ast, sema::typechecking::validate_type_specifier};

use super::{StructOrUnion, SymbolTable, TypeTable, typecheck_block_item};

use anyhow::{Context, Result, ensure};

pub fn typecheck_struct_decl(
    decl: ast::StructDecl,
    symbols: &mut SymbolTable,
    structs: &mut TypeTable,
) -> Result<ast::StructDecl> {
    if decl.members.is_empty() {
        if let Some(entry) = structs.get(&decl.tag) {
            ensure!(
                entry.tag_type == StructOrUnion::Struct,
                "struct decl {} redefined with different tag type",
                decl.tag
            );
        }
        return Ok(decl);
    }

    // validating struct definition
    ensure!(
        structs.get(&decl.tag).is_none(),
        "redefining already defined struct {}",
        decl.tag
    );

    let mut member_entries = HashSet::new();
    for member in decl.members {
        ensure!(
            !member_entries.contains(&member.name),
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
        member_entries.insert(Rc::clone(&member.name));
    }

    todo!()
}

pub fn typecheck_union_decl(
    decl: ast::StructDecl,
    symbols: &mut SymbolTable,
    structs: &mut TypeTable,
) -> Result<ast::StructDecl> {
    todo!()
}
