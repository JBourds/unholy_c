use crate::ast;
use crate::ast::Type;

use super::{SymbolTable, TypedExpr, convert_by_assignment, typecheck_expr_and_convert};

use anyhow::{Context, Result, bail};

use std::rc::Rc;

pub fn typecheck_init(
    target: &Type,
    init: ast::Initializer,
    symbols: &mut SymbolTable,
    name: &Rc<String>,
) -> Result<ast::Initializer> {
    match (target, init) {
        (target, ast::Initializer::SingleInit(..)) if target.is_array() => {
            bail!("Arrays cannot be initialized with a `SingleInit`")
        }
        (_, ast::Initializer::SingleInit(expr)) => {
            let TypedExpr { expr, r#type } = typecheck_expr_and_convert(&expr, symbols)
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
                base: ast::BaseType::Array { element, size },
                ..
            },
            ast::Initializer::CompundInit(inits),
        ) => {
            if inits.len() > *size {
                bail!("Initializer {inits:#?} has to many elements for array of len {size}");
            }
            let mut inits = inits
                .into_iter()
                .map(|i| typecheck_init(element, i, symbols, name))
                .collect::<Result<Vec<ast::Initializer>>>()?;
            while inits.len() < *size {
                inits.push(ast::Initializer::zero_initializer(element)?);
            }

            Ok(ast::Initializer::CompundInit(inits))
        }
        _ => bail!("Cannot assign compound initializer to non array var decl"),
    }
}
