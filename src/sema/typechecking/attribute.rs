use super::{InitialData, InitialValue, Scope, SymbolTable};
use anyhow::{Context, Result, ensure};

use crate::{ast, tacky::StaticInit};

#[derive(Clone, Debug)]
pub enum Attribute {
    Fun {
        external_linkage: bool,
    },
    Static {
        initial_value: InitialValue,
        external_linkage: bool,
    },
    Constant(StaticInit),
    Local,
}

impl Attribute {
    pub fn from_var_with_scope(
        var: &ast::VarDecl,
        scope: Scope,
        symbols: &mut SymbolTable,
    ) -> Result<Self> {
        if matches!(scope, Scope::Local(..)) && var.storage_class == Some(ast::StorageClass::Extern)
        {
            ensure!(
                var.init.is_none(),
                "Local var \"{}\" is extern but has an initial value",
                var.name
            );
        }
        let initial_value = if let Some(init_val) =
            InitialValue::from_var_with_scope(var, scope, symbols)?
        {
            init_val
        } else {
            match scope {
                Scope::Global => match var.storage_class {
                    Some(ast::StorageClass::Static) | None => InitialValue::Tentative, // Global non-externals with no initilizer are marked as tentative
                    Some(ast::StorageClass::Extern) => InitialValue::None,
                    _ => unreachable!(
                        "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                    ),
                },
                Scope::Local(..) => match var.storage_class {
                    Some(ast::StorageClass::Static) => {
                        InitialValue::Initial(InitialData::Bytes(vec![
                            vec![0; var.r#type.base.nbytes()].into(),
                        ]))
                    } // Local Statics with no initilizer get defaulted to zero
                    Some(ast::StorageClass::Extern) | None => InitialValue::None,
                    _ => unreachable!(
                        "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                    ),
                },
            }
        };

        if initial_value == InitialValue::None
            && var.storage_class.is_none()
            && matches!(scope, Scope::Local(..))
        {
            return Ok(Attribute::Local);
        }

        let external_linkage = match scope {
            Scope::Global => match var.storage_class {
                Some(ast::StorageClass::Static) => false,
                Some(ast::StorageClass::Extern) | None => true,
                _ => unreachable!(
                    "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                ),
            },
            Scope::Local(..) => match var.storage_class {
                Some(ast::StorageClass::Static) | None => false,
                Some(ast::StorageClass::Extern) => true,
                _ => unreachable!(
                    "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                ),
            },
        };

        Ok(Attribute::Static {
            initial_value,
            external_linkage,
        })
    }

    fn from_fun(fun: &ast::FunDecl) -> Self {
        Attribute::Fun {
            external_linkage: fun.storage_class != Some(ast::StorageClass::Static),
        }
    }

    pub fn from_decl_with_scope(
        decl: &ast::Declaration,
        scope: Scope,
        symbols: &mut SymbolTable,
    ) -> Result<Self> {
        match decl {
            ast::Declaration::FunDecl(f) => Ok(Self::from_fun(f)),
            ast::Declaration::VarDecl(v) => Self::from_var_with_scope(v, scope, symbols).context(
                format!("Failed to process attributes for variable \"{}\"", v.name),
            ),
        }
    }

    pub fn has_external_linkage(&self) -> bool {
        match self {
            Self::Fun { external_linkage } => *external_linkage,
            Self::Static {
                external_linkage, ..
            } => *external_linkage,
            Self::Local => false,
            Self::Constant { .. } => false,
        }
    }
}
