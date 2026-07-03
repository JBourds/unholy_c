use crate::ast;

use super::{
    SymbolTable, TypeTable, typecheck_fun_decl, typecheck_global_var_decl, typecheck_struct_decl,
    typecheck_union_decl,
};

use anyhow::{Context, Result};

use std::rc::Rc;

pub fn typecheck_program(
    program: ast::Program,
    symbols: &mut SymbolTable,
    structs: &mut TypeTable,
) -> Result<ast::Program> {
    let mut declarations = vec![];
    for decl in program.declarations.into_iter() {
        match decl {
            ast::Declaration::FunDecl(f) => {
                let name = Rc::clone(&f.name);
                declarations.push(ast::Declaration::FunDecl(
                    typecheck_fun_decl(f, symbols, structs).context(format!(
                        "Unable to typecheck function declaration for {name}"
                    ))?,
                ));
            }
            ast::Declaration::VarDecl(v) => {
                let name = Rc::clone(&v.name);
                declarations.push(ast::Declaration::VarDecl(
                    typecheck_global_var_decl(v, symbols, structs).context(format!(
                        "Unable to typecheck variable declaration for {name}"
                    ))?,
                ));
            }
            ast::Declaration::StructDecl(s) => {
                let name = Rc::clone(&s.tag);
                declarations.push(ast::Declaration::StructDecl(
                    typecheck_struct_decl(s, structs)
                        .context(format!("Unable to typecheck struct declaration for {name}"))?,
                ));
            }
            ast::Declaration::UnionDecl(u) => {
                let name = Rc::clone(&u.tag);
                declarations.push(ast::Declaration::StructDecl(
                    typecheck_union_decl(u, structs)
                        .context(format!("Unable to typecheck union declaration for {name}"))?,
                ));
            }
        }
    }
    Ok(ast::Program { declarations })
}
