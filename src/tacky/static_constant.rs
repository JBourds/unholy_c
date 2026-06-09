use super::*;

use crate::tacky::StaticInit;

use std::rc::Rc;

// This is cursed.
// Checking for uniqueness of floats within a set is annoying since they don't
// implement hashing. This is a hacky solution which kills several birds (and
// code reviewers) with one stone by using the formatted float string as:
//  1. A unique name + hash key
//  2. A representation for the code emission pass
//
// RC so we feel less bad about cloning this bad puppy
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StaticConstant {
    pub id: Rc<String>,
    pub val: StaticInit,
    pub alignment: usize,
}

impl StaticConstant {
    pub const LONG_MAX_VAL: f64 = i64::MAX as f64;

    pub(crate) fn from_symbol_with_name(
        name: Rc<String>,
        symbol: &sema::tc::SymbolEntry,
    ) -> Option<Self> {
        match &symbol.attribute {
            // Constants are `StaticConstant` not variable
            sema::tc::Attribute::Constant(val) => Some(Self {
                id: name,
                val: val.clone(),
                alignment: 1,
            }),
            _ => None,
        }
    }

    pub fn with_alignment(self, alignment: usize) -> Self {
        Self { alignment, ..self }
    }

    pub fn id(&self) -> Rc<String> {
        Rc::clone(&self.id)
    }
}

impl From<f32> for StaticConstant {
    fn from(value: f32) -> Self {
        let val = u32::from_ne_bytes(value.to_ne_bytes());
        Self {
            id: Rc::new(ryu::Buffer::new().format(value).to_string()),
            val: StaticInit::Float(FpNumber::F32(val)),
            alignment: core::mem::align_of::<f32>(),
        }
    }
}

impl From<f64> for StaticConstant {
    fn from(value: f64) -> Self {
        let val = u64::from_ne_bytes(value.to_ne_bytes());
        Self {
            id: Rc::new(ryu::Buffer::new().format(value).to_string()),
            val: StaticInit::Float(FpNumber::F64(val)),
            alignment: core::mem::align_of::<f64>(),
        }
    }
}
