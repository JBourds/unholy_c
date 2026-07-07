use crate::ast;
use crate::ast::Type;

use super::{SymbolTable, TypeTable, TypedExpr, convert_by_assignment, typecheck_expr_and_convert};

use anyhow::{Context, Result, bail, ensure};

use std::rc::Rc;

pub fn typecheck_init(
    target: &Type,
    init: ast::Initializer,
    symbols: &mut SymbolTable,
    structs: &TypeTable,
    name: &Rc<String>,
) -> Result<ast::Initializer> {
    match (target, init) {
        (
            ast::Type {
                base: ast::BaseType::Array { element, size },
                ..
            },
            ast::Initializer::SingleInit(expr),
        ) => match *expr {
            ast::Expr::String { value } => {
                if !element.is_char() {
                    bail!("Can't initialize a non-character type with a string literal");
                }
                if value.len() > *size {
                    bail!(
                        "String literal is too large for array, string {} > array {}",
                        value.len(),
                        size
                    );
                }
                Ok(ast::Initializer::SingleInit(Box::new(ast::Expr::String {
                    value,
                })))
            }
            _ => bail!(
                "Arrays cannot be initialized with a `SingleInit` that aren't string literals"
            ),
        },
        (
            ast::Type {
                base: ast::BaseType::Struct { tag, .. },
                ..
            },
            ast::Initializer::CompoundInit(init),
        ) => {
            let Some(entry) = structs.get(tag) else {
                bail!("cannot initialize struct type thats not been defined");
            };
            ensure!(
                init.len() <= entry.members.len(),
                "too many element in initializer list"
            );
            let mut inits = init
                .into_iter()
                .zip(entry.members.iter())
                .map(|(i, member_entry)| {
                    typecheck_init(&member_entry.r#type, i, symbols, structs, name)
                })
                .collect::<Result<Vec<ast::Initializer>>>()?;
            if inits.len() < entry.members.len() {
                for member_entry in &entry.members[entry.members.len() - inits.len()..] {
                    inits.push(ast::Initializer::zero_initializer(
                        &member_entry.r#type,
                        structs,
                    )?);
                }
            }

            Ok(ast::Initializer::CompoundInit(inits))
        }
        (_, ast::Initializer::SingleInit(expr)) => {
            let TypedExpr { expr, r#type } = typecheck_expr_and_convert(&expr, symbols, structs)
                .context("failed to typecheck expression and convert")?;
            Ok(ast::Initializer::SingleInit(
                convert_by_assignment(expr, &r#type, target)
                    .context(format!(
                        "Failed to typecheck initialization for variable \"{}\"",
                        name,
                    ))?
                    .into(),
            ))
        }

        (
            ast::Type {
                base: ast::BaseType::Array { .. },
                ..
            },
            init @ ast::Initializer::CompoundInit(_),
        ) => pad_compound_init(target, init, symbols, structs, name),
        _ => bail!("Cannot assign compound initializer to non array var decl"),
    }
}

pub fn pad_compound_init(
    target: &Type,
    init: ast::Initializer,
    symbols: &mut SymbolTable,
    structs: &TypeTable,
    name: &Rc<String>,
) -> Result<ast::Initializer> {
    if let ast::Type {
        base: ast::BaseType::Array { element, size },
        ..
    } = target
        && let ast::Initializer::CompoundInit(inits) = init
    {
        if inits.len() > *size {
            bail!("Initializer {inits:#?} has to many elements for array of len {size}");
        }
        let mut inits = inits
            .into_iter()
            .map(|i| typecheck_init(element, i, symbols, structs, name))
            .collect::<Result<Vec<ast::Initializer>>>()?;
        while inits.len() < *size {
            inits.push(ast::Initializer::zero_initializer(element, structs)?);
        }
        Ok(ast::Initializer::CompoundInit(inits))
    } else {
        Ok(init)
    }
}
