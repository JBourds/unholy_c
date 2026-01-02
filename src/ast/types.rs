use crate::lexer::Token;

use anyhow::{Context, Result, bail};
use std::num::NonZeroUsize;

use super::{Constant, StorageClass};

#[derive(Debug, Clone)]
pub enum BaseType {
    Int {
        nbytes: usize,
        signed: Option<bool>,
    },
    Float(usize),
    Double(usize),
    Fun {
        ret_t: Box<Type>,
        param_types: Vec<Type>,
    },
    Ptr {
        to: Box<Type>,
        is_restrict: bool,
    },
    Array {
        element: Box<Type>,
        size: usize,
    },
    // TODO: Implement later and make this a non unit variant
    Struct,
    Void,
}

// Need to implement this since the optional signed parameter should
// be considered signed by default or use the actual value
impl PartialEq for BaseType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Int {
                    nbytes: l_nbytes,
                    signed: l_signed,
                },
                Self::Int {
                    nbytes: r_nbytes,
                    signed: r_signed,
                },
            ) => {
                let l_signed = l_signed.is_none_or(|signed| signed);
                let r_signed = r_signed.is_none_or(|signed| signed);
                l_nbytes == r_nbytes && l_signed == r_signed
            }
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Double(l0), Self::Double(r0)) => l0 == r0,
            (
                Self::Fun {
                    ret_t: l_ret_t,
                    param_types: l_param_types,
                },
                Self::Fun {
                    ret_t: r_ret_t,
                    param_types: r_param_types,
                },
            ) => l_ret_t == r_ret_t && l_param_types == r_param_types,
            (
                Self::Ptr {
                    to: l_to,
                    is_restrict: l_is_restrict,
                },
                Self::Ptr {
                    to: r_to,
                    is_restrict: r_is_restrict,
                },
            ) => *l_to == *r_to && l_is_restrict == r_is_restrict,
            (
                Self::Array {
                    element: l_element,
                    size: l_size,
                },
                Self::Array {
                    element: r_element,
                    size: r_size,
                },
            ) => *l_element == *r_element && l_size == r_size,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl BaseType {
    pub fn nbytes(&self) -> usize {
        match self {
            BaseType::Int { nbytes, .. } => *nbytes,
            BaseType::Float(nbytes) => *nbytes,
            BaseType::Double(nbytes) => *nbytes,
            BaseType::Ptr { .. } => core::mem::size_of::<usize>(),
            BaseType::Array { element, size } => element.base.nbytes() * size,
            BaseType::Fun { .. } => unreachable!(),
            BaseType::Struct => unreachable!(),
            BaseType::Void => unreachable!(),
        }
    }

    pub fn size_of_base_type(&self) -> usize {
        match self {
            BaseType::Int { nbytes, .. } => *nbytes,
            BaseType::Float(nbytes) => *nbytes,
            BaseType::Double(nbytes) => *nbytes,
            BaseType::Ptr { to, .. } => to.size_of(),
            BaseType::Array { element, .. } => element.base.size_of_base_type(),
            BaseType::Fun { .. } => unreachable!(),
            BaseType::Struct => unreachable!(),
            BaseType::Void => unreachable!(),
        }
    }

    pub const fn int(nbytes: usize, signed: Option<bool>) -> Self {
        Self::Int { nbytes, signed }
    }

    pub fn float(double: bool) -> Self {
        if double {
            Self::Double(core::mem::size_of::<f64>())
        } else {
            Self::Float(core::mem::size_of::<f32>())
        }
    }

    // Promotion rules

    fn rank(&self) -> Option<usize> {
        // Completely arbitrary numbers but this ensures that rank favors size,
        // and that floating point representations will always win out
        match self {
            Self::Int { nbytes, signed } => Some(
                *nbytes * 10
                    + 1
                    // Hack to make sure unsigned ints rank above signed
                    + if signed.is_some_and(|signed| !signed) {
                        1
                    } else {
                        0
                    },
            ),
            Self::Float(nbytes) => Some(*nbytes * 20),
            Self::Double(nbytes) => Some(*nbytes * 30),
            _ => None,
        }
    }

    pub fn can_assign_to(&self, other: &Self) -> bool {
        match (self.rank(), other.rank()) {
            (Some(_), Some(_)) => true,
            (Some(_), None) | (None, Some(_)) => false,
            (None, None) => self == other,
        }
    }

    pub fn default_alignment(&self) -> NonZeroUsize {
        match self {
            Self::Int { nbytes, .. } => NonZeroUsize::new(*nbytes).unwrap(),
            Self::Float(nbytes) => NonZeroUsize::new(*nbytes).unwrap(),
            Self::Double(nbytes) => NonZeroUsize::new(*nbytes).unwrap(),
            Self::Fun { .. } | Self::Ptr { .. } => {
                NonZeroUsize::new(core::mem::size_of::<usize>()).unwrap()
            }
            Self::Array { element, .. } => element.base.default_alignment(),
            Self::Struct => todo!(),
            Self::Void => todo!(),
        }
    }

    pub fn lift(lhs: Self, rhs: Self) -> Result<(Self, Self)> {
        let left = lhs.default_promote();
        let right = rhs
            .promote(&left)
            .context("Unable to promote lefthand side to match righthand side.")?;
        let left = left
            .promote(&right)
            .context("Unable to promote righthand side to match lefthand side.")?;
        Ok((left, right))
    }

    fn default_promote(self) -> Self {
        match self {
            // Integer types get promoted to a basic signed int by default
            Self::Int { .. } => {
                let int = Self::default();
                let int_rank = int.rank().expect("Integer does not have a rank?");
                if self.rank().expect("Integer does not have a rank?") >= int_rank {
                    self
                } else {
                    self.promote(&int)
                        .expect("We can always promote an integer to another integer type.")
                }
            }
            _ => self,
        }
    }

    fn promote(self, other: &Self) -> Result<Self> {
        let other_rank = other
            .rank()
            .context(format!("Could not establish a rank for type {other:#?}."))?;
        if self
            .rank()
            .context(format!("Could not establish a rank for type {self:#?}"))?
            >= other_rank
        {
            Ok(self)
        } else {
            Ok(other.clone())
        }
    }

    fn consume(tokens: &[Token]) -> Result<(BaseType, &[Token])> {
        if let Some(t) = tokens.first() {
            // TODO: Add parsing for function type signatures here too:
            // <ret_t> (*<optional name>)(<arg>*)
            // - Name and asterisk can have parens around them
            // - Types cannot have parens
            // - No trailing commas
            match t {
                Token::Void => Ok((Self::Void, &tokens[1..])),
                // Use this as a default, fix it up where this gets called
                // if there are specifiers which change this
                Token::Int => Ok((
                    Self::Int {
                        nbytes: std::mem::size_of::<i32>(),
                        signed: None,
                    },
                    &tokens[1..],
                )),
                Token::Float => Ok((Self::Float(std::mem::size_of::<f32>()), &tokens[1..])),
                Token::Double => Ok((Self::Double(std::mem::size_of::<f64>()), &tokens[1..])),
                // TODO: Recursive parsing logic for structs
                Token::Struct => Ok((Self::Struct, &tokens[1..])),
                _ => bail!("Could not parse base type."),
            }
        } else {
            bail!("No more tokens to parse a base type from.")
        }
    }
}

impl From<&Constant> for BaseType {
    fn from(value: &Constant) -> Self {
        match value {
            Constant::I8(_) => Self::int(core::mem::size_of::<i8>(), None),
            Constant::I16(_) => Self::int(core::mem::size_of::<i16>(), None),
            Constant::I32(_) => Self::int(core::mem::size_of::<i32>(), None),
            Constant::I64(_) => Self::int(core::mem::size_of::<i64>(), None),
            Constant::U8(_) => Self::int(core::mem::size_of::<u8>(), None),
            Constant::U16(_) => Self::int(core::mem::size_of::<u16>(), None),
            Constant::U32(_) => Self::int(core::mem::size_of::<u32>(), Some(false)),
            Constant::U64(_) => Self::int(core::mem::size_of::<u64>(), Some(false)),
            Constant::F32(_) => Self::float(false),
            Constant::F64(_) => Self::float(true),
        }
    }
}

impl Default for BaseType {
    fn default() -> Self {
        Self::int(core::mem::size_of::<i32>(), None)
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub base: BaseType,
    pub alignment: NonZeroUsize,
    pub is_const: bool,
}

impl From<&Constant> for Type {
    fn from(value: &Constant) -> Self {
        let base = BaseType::from(value);
        let alignment = base.default_alignment();
        Self {
            base,
            alignment,
            is_const: false,
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base && self.alignment == other.alignment
    }
}

impl Type {
    pub const PTR_ALIGNMENT: NonZeroUsize =
        NonZeroUsize::new(core::mem::size_of::<usize>()).unwrap();

    pub const PTRDIFF_T: Self = Self::int(core::mem::size_of::<isize>(), Some(true));

    pub fn bool() -> Self {
        Self::int(core::mem::size_of::<i32>(), None)
    }

    pub const fn int(nbytes: usize, signed: Option<bool>) -> Self {
        Self {
            base: BaseType::int(nbytes, signed),
            alignment: NonZeroUsize::new(nbytes).unwrap(),
            is_const: true,
        }
    }

    pub fn float(nbytes: usize) -> Self {
        Self {
            base: BaseType::float(nbytes == core::mem::size_of::<f64>()),
            alignment: NonZeroUsize::new(nbytes).unwrap(),
            is_const: true,
        }
    }

    pub fn size_of(&self) -> usize {
        self.base.nbytes()
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.base, BaseType::Ptr { .. })
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self.base,
            BaseType::Int { .. } | BaseType::Float(_) | BaseType::Double(_)
        )
    }

    pub fn is_integer(&self) -> bool {
        matches!(self.base, BaseType::Int { .. },)
    }

    pub fn is_float(&self) -> bool {
        matches!(self.base, BaseType::Float(_) | BaseType::Double(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self.base, BaseType::Fun { .. })
    }

    pub fn is_array(&self) -> bool {
        matches!(&self.base, BaseType::Array { .. })
    }

    pub fn deref(self) -> Self {
        match self.base {
            BaseType::Ptr { to, is_restrict: _ } => *to,
            BaseType::Array { element, .. } => *element,
            _ => unreachable!("Cannot dereference non-pointer type."),
        }
    }

    /// Try to find a common type which one element can decay to the other as.
    pub fn is_or_decays_to(&self, target: &Self) -> bool {
        if self == target {
            return true;
        }
        let self_decayed = self.maybe_decay();
        self_decayed == *target
    }

    pub fn maybe_decay(&self) -> Self {
        match self {
            // Array types decay to pointers of their element types
            Self {
                base: BaseType::Array { element, .. },
                ..
            } => Self {
                base: BaseType::Ptr {
                    to: Box::new(*element.clone()),
                    is_restrict: false,
                },
                alignment: Self::PTR_ALIGNMENT,
                is_const: false,
            },
            // "Decaying" a function type is just making sure all of its
            // parameters have their types decayed
            Self {
                base: BaseType::Fun { ret_t, param_types },
                ..
            } => Self {
                base: BaseType::Fun {
                    ret_t: ret_t.clone(),
                    param_types: param_types.iter().map(|t| t.maybe_decay()).collect(),
                },
                ..self.clone()
            },
            _ => self.clone(),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_const {
            write!(f, "const ")?;
        }
        self.base.fmt(f)?;

        Ok(())
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct TypeBuilder {
    stream_offset: usize,
    n_longs: usize,
    is_signed: Option<bool>,
    is_short: bool,
    is_const: bool,
    alignment: Option<NonZeroUsize>,
    storage: Option<StorageClass>,
    base: Option<BaseType>,
}

impl TypeBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_base(mut self, tokens: &[Token]) -> Result<Self> {
        let mut remaining = tokens;
        while let Some(t) = remaining.first() {
            if self.check_for_specifier(t)? || self.check_for_storage(t)? || self.check_for_const(t)
            {
                remaining = &remaining[1..];
            } else if let (true, tokens) = self.check_for_base_type(remaining)? {
                remaining = tokens;
            } else {
                break;
            }
        }

        // Integer checks
        // 1. If any of the integer flags changed, the base type either has
        // to be explicitly declared as an integer or have been elided
        if self.n_longs > 0 && self.is_short {
            bail!("Integer cannot be both a long and a short.");
        }
        if self.n_longs > 0 && matches!(&self.base, Some(BaseType::Double { .. })) {
            self.base
                .replace(BaseType::Double(std::mem::size_of::<f64>()));
        } else if self.n_longs > 0 || self.is_signed.is_some() || self.is_short {
            match self.base {
                Some(BaseType::Int { .. }) | None => {
                    let nbytes = if self.n_longs > 0 {
                        std::mem::size_of::<i64>()
                    } else if self.is_short {
                        std::mem::size_of::<i16>()
                    } else {
                        std::mem::size_of::<i32>()
                    };
                    self.base.replace(BaseType::Int {
                        nbytes,
                        signed: self.is_signed,
                    });
                }
                _ => bail!(
                    "Cannot provide type specifiers specific to integers for non integer type."
                ),
            }
        }

        self.stream_offset = tokens.len() - remaining.len();

        Ok(self)
    }

    fn check_for_base_type<'a>(&mut self, tokens: &'a [Token]) -> Result<(bool, &'a [Token])> {
        if let Ok((r#type, tokens)) = BaseType::consume(tokens) {
            if self.base.is_some() {
                bail!("Error: Found two conflicting types.");
            }
            self.base = Some(r#type);
            Ok((true, tokens))
        } else {
            Ok((false, tokens))
        }
    }

    fn check_for_const(&mut self, t: &Token) -> bool {
        if *t == Token::Const {
            // More `const` won't make it more constant but still allow it
            self.is_const = true;
            true
        } else {
            false
        }
    }

    fn check_for_specifier(&mut self, t: &Token) -> Result<bool> {
        match t {
            Token::Short => {
                self.is_short = true;
                if self.is_short && self.n_longs > 0 {
                    bail!("Found \"short\" and \"long\" in same type declaration.");
                }
                Ok(true)
            }
            Token::Long => {
                self.n_longs += 1;
                if self.is_short && self.n_longs > 0 {
                    bail!("Found \"short\" and \"long\" in same type declaration.");
                }
                Ok(true)
            }
            Token::Unsigned => {
                if self.is_signed.is_some_and(|signed| signed) {
                    bail!("Type cannot be both signed and unsigned.");
                } else if self.is_signed.is_some() {
                    bail!("Error: duplicate unsigned");
                }
                self.is_signed = Some(false);
                Ok(true)
            }
            Token::Signed => {
                if self.is_signed.is_some_and(|signed| !signed) {
                    bail!("Type cannot be both signed and unsigned.");
                } else if self.is_signed.is_some() {
                    bail!("Error: duplicate nsigned");
                }
                self.is_signed = Some(true);
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn check_for_storage(&mut self, t: &Token) -> Result<bool> {
        let storage = match t {
            Token::Static => Some(StorageClass::Static),
            Token::Extern => Some(StorageClass::Extern),
            Token::Auto => Some(StorageClass::Auto),
            Token::Register => Some(StorageClass::Register),
            _ => None,
        };
        match (self.storage.is_some(), storage.is_some()) {
            (true, true) => bail!("Error: Multiple storage class specifiers."),
            (false, true) => {
                self.storage = storage;
                Ok(true)
            }
            (_, false) => Ok(false),
        }
    }

    pub fn into_type(self) -> Result<(usize, Type, Option<StorageClass>)> {
        if let Some(base) = self.base {
            let alignment = self.alignment.unwrap_or(base.default_alignment());
            if !alignment.is_power_of_two() {
                bail!("Alignment must be a power of 2");
            }

            Ok((
                self.stream_offset,
                Type {
                    base,
                    alignment,
                    is_const: self.is_const,
                },
                self.storage,
            ))
        } else {
            bail!("Type builder has not parsed a base type yet");
        }
    }
}
