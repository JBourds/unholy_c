use super::{
    Attribute, Scope, SymbolTable, TypedExpr, const_eval, convert_by_assignment,
    is_null_pointer_constant, typecheck_expr_and_convert,
};
use anyhow::{Context, Result, bail, ensure};

use std::cmp;
use std::rc::Rc;

use crate::ast;
use crate::tacky::StaticInit;
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InitialValue {
    Initial(Vec<StaticInit>),
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
            (
                ast::Type {
                    base: ast::BaseType::Array { element, size },
                    ..
                },
                ast::Initializer::SingleInit(expr),
            ) => match &**expr {
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
                    let null_terminated = *size > value.len();
                    let mut inits = vec![StaticInit::String {
                        data: value.as_bytes().to_vec().into(),
                        null_terminated,
                    }];
                    if null_terminated {
                        let remaining = *size - value.len() - 1;
                        if remaining > 0 {
                            inits.push(StaticInit::Zero(remaining));
                        }
                    }
                    Ok(Self::Initial(inits))
                }
                _ => bail!("Arrays cannot be initialized with a `SingleInit`"),
            },
            (
                r#type @ ast::Type {
                    base: ast::BaseType::Ptr { to, .. },
                    ..
                },
                ast::Initializer::SingleInit(expr),
            ) => match &**expr {
                ast::Expr::String { value } => {
                    if !to.base.is_plain_char() {
                        bail!(
                            "Cannot assign string literal to non char pointer, including signed and unsigned char pointers"
                        );
                    }
                    let label = symbols.get_or_make_string(Rc::clone(value));
                    Ok(Self::Initial(vec![StaticInit::Pointer(label)]))
                }
                _ => Self::from_expr(r#type, expr, symbols),
            },
            (_, ast::Initializer::SingleInit(init)) => Self::from_expr(r#type, init, symbols),
            (
                ast::Type {
                    base: ast::BaseType::Array { element, size },
                    ..
                },
                ast::Initializer::CompoundInit(inits),
            ) => {
                ensure!(inits.len() <= *size, "Too many initializers in static init");
                let mut new_inits = vec![];
                for init in inits.iter() {
                    match Self::from_initializer(element, init, symbols)? {
                        Self::Initial(init) => {
                            new_inits.extend(init);
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
            return Ok(InitialValue::Initial(vec![StaticInit::Zero(
                core::mem::size_of::<usize>(),
            )]));
        }
        let val = const_eval::eval(expr.clone()).context("Failed to const eval expression")?;
        Ok(InitialValue::Initial(vec![val.into()]))
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
                Some(ast::StorageClass::Static) => {
                    Ok(Some(InitialValue::Initial(vec![StaticInit::Zero(
                        var.r#type.base.nbytes(),
                    )])))
                }
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
