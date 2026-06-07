use std::rc::Rc;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FpNumber {
    F32(u32),
    F64(u64),
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum StaticInit {
    Float(FpNumber),
    String {
        data: Rc<[u8]>,
        null_terminated: bool,
    },
    Pointer(Rc<String>),
    IntInit(i32),
    UIntInit(u32),
    LongInit(i64),
    ULongInit(u64),
    CharInit(i8),
    UCharInit(i8),
    ZeroInit(usize),
}
