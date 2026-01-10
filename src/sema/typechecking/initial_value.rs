use super::{
    Attribute, Scope, SymbolTable, TypedExpr, const_eval, convert_by_assignment,
    is_null_pointer_constant, typecheck_expr_and_convert,
};
use anyhow::{Context, Result, bail, ensure};

use std::cmp;
use std::rc::Rc;

use crate::ast;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InitialValue {
    Initial(Vec<Rc<[u8]>>),
    Tentative,
    None,
}

impl Ord for InitialValue {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (Self::Initial(_), Self::Initial(_)) => cmp::Ordering::Equal,
            (Self::None, Self::None) => cmp::Ordering::Equal,
            (Self::Tentative, Self::Tentative) => cmp::Ordering::Equal,
            (_, Self::None) => cmp::Ordering::Greater,
            (Self::Initial(_), Self::Tentative) => cmp::Ordering::Greater,
            (Self::None, _) => cmp::Ordering::Less,
            (Self::Tentative, Self::Initial(_)) => cmp::Ordering::Less,
        }
    }
}

impl PartialOrd for InitialValue {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl InitialValue {
    pub fn from_initializer(
        r#type: &ast::Type,
        init: &ast::Initializer,
        symbols: &mut SymbolTable,
    ) -> Result<Self> {
        match (r#type, init) {
            (_, ast::Initializer::SingleInit(init)) => Self::from_expr(r#type, init, symbols),
            (
                ast::Type {
                    base: ast::BaseType::Array { element, size },
                    ..
                },
                ast::Initializer::CompundInit(inits),
            ) => {
                ensure!(inits.len() <= *size, "Too many initializers in static init");
                let mut new_inits = vec![];
                for init in inits.iter() {
                    match Self::from_initializer(element, init, symbols)? {
                        Self::Initial(initial) => {
                            new_inits.extend(initial);
                        }
                        _ => unreachable!(),
                    }
                }

                Ok(Self::Initial(new_inits))
            }
            _ => bail!("Cannot static init non-array with compound initializer"),
        }
    }
    // TODO: Make this not dependent on host computer byte ordering
    fn from_expr(target: &ast::Type, expr: &ast::Expr, symbols: &mut SymbolTable) -> Result<Self> {
        let TypedExpr { expr, r#type } = typecheck_expr_and_convert(expr, symbols)
            .context("failed to typecheck expression and convert")?;
        let expr = convert_by_assignment(expr, &r#type, target).context(
            "Failed to perform implicit casting when constructing initial value for declaration",
        )?;
        if is_null_pointer_constant(&expr) && target.is_pointer() {
            return Ok(InitialValue::Initial(vec![
                0usize.to_ne_bytes().to_vec().into(),
            ]));
        }
        let val = const_eval::eval(expr.clone()).context("Failed to const eval expression")?;
        match val {
            ast::Constant::I8(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
            ast::Constant::I16(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
            ast::Constant::I32(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
            ast::Constant::I64(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
            ast::Constant::U8(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
            ast::Constant::U16(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
            ast::Constant::U32(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
            ast::Constant::U64(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
            ast::Constant::F32(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
            ast::Constant::F64(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
            // FIXME: We may need to truncate these to fit in i8/u8
            ast::Constant::ICHAR(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
            ast::Constant::UCHAR(val) => Ok(InitialValue::Initial(vec![
                val.to_ne_bytes().to_vec().into(),
            ])),
        }
    }

    pub fn from_var_with_scope(
        var: &ast::VarDecl,
        scope: Scope,
        symbols: &mut SymbolTable,
    ) -> Result<Option<Self>> {
        match (scope, var.init.as_ref()) {
            (Scope::Global, Some(init)) => {
                let init = Self::from_initializer(&var.r#type, init, symbols)
                    .context(format!("Evaluating expression for \"{}\" failed", var.name))?;
                Ok(Some(init))
            }
            (Scope::Local(..), Some(init)) => match var.storage_class {
                Some(ast::StorageClass::Static) => Ok(Some(
                    Self::from_initializer(&var.r#type, init, symbols)
                        .context(format!("Evaluating expression for \"{}\" failed", var.name))?,
                )),
                None => Ok(None), // Locals technically dont have initial values
                Some(ast::StorageClass::Extern) => unreachable!(),
                _ => unreachable!(
                    "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                ),
            },
            (Scope::Global, None) => match var.storage_class {
                Some(ast::StorageClass::Static) | None => Ok(Some(InitialValue::Tentative)), // Global non-externals with no initilizer are marked as tentative
                Some(ast::StorageClass::Extern) => Ok(Some(InitialValue::None)),
                _ => unreachable!(
                    "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                ),
            },
            (Scope::Local(..), None) => match var.storage_class {
                // Local Statics with no initilizer get defaulted to zero
                Some(ast::StorageClass::Static) => Ok(Some(InitialValue::Initial(vec![
                    vec![0; var.r#type.base.nbytes()].into(),
                ]))),
                // Resolve any existing declaration's initial value
                Some(ast::StorageClass::Extern) => {
                    if let Some(entry) = symbols.get(&var.name) {
                        match &entry.attribute {
                            Attribute::Static { initial_value, .. } => {
                                Ok(Some(initial_value.clone()))
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        Ok(Some(InitialValue::None))
                    }
                }
                None => Ok(Some(InitialValue::None)),
                _ => unreachable!(
                    "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                ),
            },
        }
    }
}
