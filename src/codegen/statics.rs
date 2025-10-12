use super::FpNumber;
use crate::tacky;
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
    pub val: FpNumber,
    pub alignment: usize,
}

#[derive(Debug, PartialEq)]
pub struct StaticVariable {
    pub identifier: Rc<String>,
    pub global: bool,
    pub init: Vec<Rc<[u8]>>,
}

impl StaticConstant {
    pub(super) const LONG_MAX_VAL: f64 = i64::MAX as f64;

    pub(super) fn new(id: Rc<String>, val: FpNumber, alignment: usize) -> Self {
        Self { id, val, alignment }
    }

    pub(super) fn with_alignment(self, alignment: usize) -> Self {
        Self { alignment, ..self }
    }

    pub(super) fn id(&self) -> Rc<String> {
        Rc::clone(&self.id)
    }
}

impl From<tacky::StaticVariable> for StaticVariable {
    fn from(value: tacky::StaticVariable) -> Self {
        let tacky::StaticVariable {
            identifier,
            external_linkage: global,
            init,
        } = value;
        Self {
            identifier,
            global,
            init,
        }
    }
}

impl From<f32> for StaticConstant {
    fn from(value: f32) -> Self {
        let val = u32::from_ne_bytes(value.to_ne_bytes());
        Self::new(
            Rc::new(ryu::Buffer::new().format(value).to_string()),
            FpNumber::F32(val),
            core::mem::align_of::<f32>(),
        )
    }
}

impl From<f64> for StaticConstant {
    fn from(value: f64) -> Self {
        let val = u64::from_ne_bytes(value.to_ne_bytes());
        Self::new(
            Rc::new(ryu::Buffer::new().format(value).to_string()),
            FpNumber::F64(val),
            core::mem::align_of::<f64>(),
        )
    }
}
