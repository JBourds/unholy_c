use crate::{ast, tacky};

mod asm_types;
mod cond_code;
mod function;
mod instruction;
mod operand;
mod ops;
mod program;
mod regs;
mod rewrite;
mod statics;

pub use asm_types::*;
pub use cond_code::*;
pub use function::*;
pub use instruction::*;
pub use operand::*;
pub use ops::*;
pub use program::*;
pub use regs::*;
use rewrite::*;
pub use statics::*;

pub(super) const MAX_AGGREGATE_ALIGNMENT: usize = 16;

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    Fun(Function),
    StaticVariable(StaticVariable),
    StaticConstant(StaticConstant),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FpNumber {
    F32(u32),
    F64(u64),
}

// Create unique identifiers (e.g., Labels in cast)
fn make_temp(counter: &'_ mut usize) -> impl FnMut(String) -> String + use<'_> {
    move |name| {
        let n = *counter;
        *counter += 1;
        format!("codegen.{name}.{n}")
    }
}

fn is_float(val: &tacky::Val, symbols: &tacky::SymbolTable) -> bool {
    matches!(
        val.get_type(symbols),
        ast::Type {
            base: ast::BaseType::Float(_) | ast::BaseType::Double(_),
            ..
        }
    )
}

fn is_signed(val: &tacky::Val, symbols: &tacky::SymbolTable) -> bool {
    match val.get_type(symbols) {
        ast::Type {
            base: ast::BaseType::Int { signed, .. },
            ..
        } => signed.is_none_or(|signed| signed),
        _ => true,
    }
}
