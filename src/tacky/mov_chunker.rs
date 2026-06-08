use std::rc::Rc;

use crate::{
    ast,
    tacky::{Instruction, Val},
};

// Utility struct for chunking bytes moves into longer sequences
pub(super) struct MovChunker<'a> {
    source: &'a [u8],
    destination: Rc<String>,
    /// Size of target array, necessary to ensure proper zero padding
    destination_length: usize,
    i: usize,
    base: usize,
}

impl<'a> MovChunker<'a> {
    pub(super) fn new(
        source: &'a [u8],
        destination: Rc<String>,
        destination_length: usize,
        base: usize,
    ) -> Self {
        Self {
            source,
            destination,
            destination_length,
            i: 0,
            base,
        }
    }
}

impl<'a> Iterator for MovChunker<'a> {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i == self.destination_length {
            return None;
        }

        let remaining = self
            .destination_length
            .saturating_sub(self.i)
            .min(core::mem::size_of::<u64>());
        let (chunk_size, val) = match remaining {
            4..8 => {
                const CHUNK_SIZE: usize = core::mem::size_of::<u32>();
                let mut arr = [0u8; CHUNK_SIZE];
                for (i, byte) in self.source.iter().skip(self.i).take(CHUNK_SIZE).enumerate() {
                    arr[i] = *byte;
                }
                (CHUNK_SIZE, ast::Constant::U32(u32::from_le_bytes(arr)))
            }
            2..4 => {
                const CHUNK_SIZE: usize = core::mem::size_of::<u16>();
                let mut arr = [0u8; CHUNK_SIZE];
                for (i, byte) in self.source.iter().skip(self.i).take(CHUNK_SIZE).enumerate() {
                    arr[i] = *byte;
                }
                (CHUNK_SIZE, ast::Constant::U16(u16::from_le_bytes(arr)))
            }
            0..2 => {
                const CHUNK_SIZE: usize = core::mem::size_of::<u8>();
                let mut arr = [0u8; CHUNK_SIZE];
                for (i, byte) in self.source.iter().skip(self.i).take(CHUNK_SIZE).enumerate() {
                    arr[i] = *byte;
                }
                (CHUNK_SIZE, ast::Constant::U8(u8::from_le_bytes(arr)))
            }
            _ => {
                const CHUNK_SIZE: usize = core::mem::size_of::<u64>();
                let mut arr = [0u8; CHUNK_SIZE];
                for (i, byte) in self.source.iter().skip(self.i).take(CHUNK_SIZE).enumerate() {
                    arr[i] = *byte;
                }
                (CHUNK_SIZE, ast::Constant::U64(u64::from_le_bytes(arr)))
            }
        };
        let offset = self.base + self.i;
        let ret = Instruction::CopyToOffset {
            src: Val::Constant(val),
            dst: self.destination.clone(),
            offset: offset.try_into().expect("cannot convert usize -> isize"),
        };
        self.i += chunk_size;
        Some(ret)
    }
}
