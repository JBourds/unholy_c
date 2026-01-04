use anyhow::{Context, Result, bail};
use num::{NumCast, bigint::BigUint};
use std::str::FromStr;

use super::Type;
use crate::lexer::{ConstantFlag, Token};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Constant {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
}

// Implement these traits knowing they will never get called on floats
impl std::hash::Hash for Constant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::F32(_) | Self::F64(_) => unreachable!(),
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}

impl std::cmp::Eq for Constant {}

impl Constant {
    pub fn is_int(&self) -> bool {
        !matches!(self, Self::F32(_) | Self::F64(_))
    }

    pub fn fits_in<T>(&self) -> bool
    where
        T: TryFrom<i8>,
        T: TryFrom<i16>,
        T: TryFrom<i32>,
        T: TryFrom<i64>,
        T: TryFrom<u8>,
        T: TryFrom<u16>,
        T: TryFrom<u32>,
        T: TryFrom<u64>,
        T: NumCast,
    {
        match &self {
            Constant::I8(v) => <T as TryFrom<i8>>::try_from(*v).is_ok(),
            Constant::I16(v) => <T as TryFrom<i16>>::try_from(*v).is_ok(),
            Constant::I32(v) => <T as TryFrom<i32>>::try_from(*v).is_ok(),
            Constant::I64(v) => <T as TryFrom<i64>>::try_from(*v).is_ok(),
            Constant::U8(v) => <T as TryFrom<u8>>::try_from(*v).is_ok(),
            Constant::U16(v) => <T as TryFrom<u16>>::try_from(*v).is_ok(),
            Constant::U32(v) => <T as TryFrom<u32>>::try_from(*v).is_ok(),
            Constant::U64(v) => <T as TryFrom<u64>>::try_from(*v).is_ok(),
            Constant::F32(v) => num::cast::<f32, T>(*v).is_some(),
            Constant::F64(v) => num::cast::<f64, T>(*v).is_some(),
        }
    }

    pub fn size_bytes(&self) -> usize {
        match self {
            Constant::I8(_) => core::mem::size_of::<i8>(),
            Constant::I16(_) => core::mem::size_of::<i16>(),
            Constant::I32(_) => core::mem::size_of::<i32>(),
            Constant::I64(_) => core::mem::size_of::<i64>(),
            Constant::U8(_) => core::mem::size_of::<u8>(),
            Constant::U16(_) => core::mem::size_of::<u16>(),
            Constant::U32(_) => core::mem::size_of::<u32>(),
            Constant::U64(_) => core::mem::size_of::<u64>(),
            Constant::F32(_) => core::mem::size_of::<f32>(),
            Constant::F64(_) => core::mem::size_of::<f64>(),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Self::U8(_) | Self::U16(_) | Self::U32(_) | Self::U64(_) => {
                Type::int(self.size_bytes(), Some(false))
            }
            Self::I8(_) | Self::I16(_) | Self::I32(_) | Self::I64(_) => {
                Type::int(self.size_bytes(), Some(true))
            }
            Self::F32(_) => Type::float(self.size_bytes()),
            Self::F64(_) => Type::float(self.size_bytes()),
        }
    }

    pub fn const_from_type(r#type: &Type, value: u8) -> Result<Self> {
        match r#type {
            _ if *r#type == Constant::I8(value as i8).get_type() => Ok(Constant::I8(value as i8)),
            _ if *r#type == Constant::I16(value as i16).get_type() => {
                Ok(Constant::I16(value as i16))
            }
            _ if *r#type == Constant::I32(value as i32).get_type() => {
                Ok(Constant::I32(value as i32))
            }
            _ if *r#type == Constant::I64(value as i64).get_type() => {
                Ok(Constant::I64(value as i64))
            }
            _ if *r#type == Constant::U8(value).get_type() => Ok(Constant::U8(value)),
            _ if *r#type == Constant::U16(value as u16).get_type() => {
                Ok(Constant::U16(value as u16))
            }
            _ if *r#type == Constant::U32(value as u32).get_type() => {
                Ok(Constant::U32(value as u32))
            }
            _ if *r#type == Constant::U64(value as u64).get_type() => {
                Ok(Constant::U64(value as u64))
            }
            _ if *r#type == Constant::F32(value as f32).get_type() => {
                Ok(Constant::F32(value as f32))
            }
            _ if *r#type == Constant::F64(value as f64).get_type() => {
                Ok(Constant::F64(value as f64))
            }
            _ => bail!("Could not create a constant with type {type} and value {value}"),
        }
    }

    pub fn consume(tokens: &[Token]) -> Result<(Constant, &[Token])> {
        // FIXME: Is this how we would like to handle integer overflow?
        if let Some(token) = tokens.first() {
            match token {
                // NOTE: The text in these nodes does not include the negative
                // sign so we don't need to worry about absolute value sign
                Token::Constant { text, flag: None } => {
                    if let Ok(val) = text.parse::<i32>() {
                        Ok((Self::I32(val), &tokens[1..]))
                    } else if let Ok(val) = text.parse::<i64>() {
                        Ok((Self::I64(val), &tokens[1..]))
                    } else if let Ok(val) = text.parse::<u32>() {
                        Ok((Self::U32(val), &tokens[1..]))
                    } else if let Ok(val) = text.parse::<u64>() {
                        Ok((Self::U64(val), &tokens[1..]))
                    } else {
                        eprintln!("Warning: Integer constant is being truncated to fit.");
                        let mut bytes = BigUint::from_str(text)
                            .context(format!("Unable to parse BigUint from text: \"{text}\""))?
                            .to_bytes_le();
                        bytes.resize(core::mem::size_of::<i64>(), 0);
                        let val = Self::I64(i64::from_le_bytes(
                            bytes.into_boxed_slice()[..core::mem::size_of::<i64>()]
                                .try_into()
                                .unwrap(),
                        ));
                        Ok((val, &tokens[1..]))
                    }
                }
                Token::Constant { text, flag } => match flag {
                    // FIXME: More intelligent scheme to know what this value
                    // will end up as (e.g., float instead of double) to avoid
                    // the double roundoff error
                    Some(ConstantFlag::Float) => {
                        let val = text
                            .parse::<f64>()
                            .context("Unable to parse double from text: \"{text}\".")?;
                        Ok((Self::F64(val), &tokens[1..]))
                    }
                    Some(ConstantFlag::Unsigned) => {
                        let val = if let Ok(val) = text.parse::<u32>() {
                            Self::U32(val)
                        } else if let Ok(val) = text.parse::<u64>() {
                            Self::U64(val)
                        } else {
                            eprintln!("Warning: Integer constant is being truncated to fit.");
                            let mut bytes = BigUint::from_str(text)
                                .context(format!("Unable to parse BigUint from text: \"{text}\""))?
                                .to_bytes_le();
                            bytes.resize(core::mem::size_of::<u64>(), 0);
                            Self::U64(u64::from_le_bytes(
                                bytes.into_boxed_slice()[..core::mem::size_of::<u64>()]
                                    .try_into()
                                    .unwrap(),
                            ))
                        };
                        Ok((val, &tokens[1..]))
                    }
                    Some(ConstantFlag::UnsignedLong) => {
                        let val = if let Ok(val) = text.parse::<u64>() {
                            Self::U64(val)
                        } else {
                            eprintln!("Warning: Integer constant is being truncated to fit .");
                            let mut bytes = BigUint::from_str(text)
                                .context(format!("Unable to parse BigUint from text: \"{text}\""))?
                                .to_bytes_le();
                            bytes.resize(core::mem::size_of::<u64>(), 0);
                            Self::U64(u64::from_le_bytes(
                                bytes.into_boxed_slice()[..core::mem::size_of::<u64>()]
                                    .try_into()
                                    .unwrap(),
                            ))
                        };

                        Ok((val, &tokens[1..]))
                    }
                    Some(ConstantFlag::Long) => {
                        let val = if let Ok(val) = text.parse::<i64>() {
                            Self::I64(val)
                        } else if let Ok(val) = text.parse::<u64>() {
                            Self::U64(val)
                        } else {
                            eprintln!("Warning: Integer constant is being truncated to fit .");
                            let mut bytes = BigUint::from_str(text)
                                .context(format!("Unable to parse BigUint from text: \"{text}\""))?
                                .to_bytes_le();
                            bytes.resize(core::mem::size_of::<u64>(), 0);
                            Self::U64(u64::from_le_bytes(
                                bytes.into_boxed_slice()[..core::mem::size_of::<u64>()]
                                    .try_into()
                                    .unwrap(),
                            ))
                        };
                        Ok((val, &tokens[1..]))
                    }
                    _ => bail!("Could not parse token into constant."),
                },
                _ => bail!("Could not parse token into constant."),
            }
        } else {
            bail!("No remaining tokens.")
        }
    }

    pub fn as_array_size(&self) -> Result<usize> {
        match self {
            Constant::I8(v) => {
                usize::try_from(*v).context("failed to convert {v} to positive size")
            }
            Constant::I16(v) => {
                usize::try_from(*v).context("failed to convert {v} to positive size")
            }
            Constant::I32(v) => {
                usize::try_from(*v).context("failed to convert {v} to positive size")
            }
            Constant::I64(v) => {
                usize::try_from(*v).context("failed to convert {v} to positive size")
            }
            Constant::U8(v) => Ok(*v as usize),
            Constant::U16(v) => Ok(*v as usize),
            Constant::U32(v) => Ok(*v as usize),
            Constant::U64(v) => Ok(*v as usize),
            Constant::F32(..) => bail!("Floats cannot be used as constants in array declaration"),
            Constant::F64(..) => bail!("Floats cannot be used as constants in array declaration"),
        }
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::I8(v) => write!(f, "{v}"),
            Constant::I16(v) => write!(f, "{v}"),
            Constant::I32(v) => write!(f, "{v}"),
            Constant::I64(v) => write!(f, "{v}"),
            Constant::U8(v) => write!(f, "{v}"),
            Constant::U16(v) => write!(f, "{v}"),
            Constant::U32(v) => write!(f, "{v}"),
            Constant::U64(v) => write!(f, "{v}"),
            Constant::F32(v) => write!(f, "{v}"),
            Constant::F64(v) => write!(f, "{v}"),
        }
    }
}
