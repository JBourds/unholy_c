use anyhow::{Context, Result, bail, ensure};
use std::rc::Rc;

use super::{BaseType, Block, Declarator, Initializer, StorageClass, Type, TypeBuilder};
use crate::{ast::token_stream::eat_semi, lexer::Token};

#[derive(Clone, Debug, PartialEq)]
pub struct FunDecl {
    pub r#type: Type,
    pub name: Rc<String>,
    pub params: Vec<Option<Rc<String>>>,
    pub block: Option<Block>,
    pub storage_class: Option<StorageClass>,
}

impl From<&FunDecl> for Type {
    fn from(value: &FunDecl) -> Self {
        value.r#type.clone()
    }
}

impl FunDecl {
    #[allow(clippy::type_complexity)]
    pub fn signature(&self) -> Result<Vec<(Type, Option<&Rc<String>>)>> {
        if let Type {
            base: BaseType::Fun {
                ref param_types, ..
            },
            ..
        } = self.r#type
        {
            ensure!(
                param_types.len() == self.params.len(),
                "ast.FunDecl.signature(): Parameter type and name vectors should be the same length."
            );
            Ok(param_types
                .iter()
                .zip(self.params.iter())
                .map(|(r#type, name)| (r#type.clone().maybe_decay(), name.as_ref()))
                .collect())
        } else {
            bail!("ast.FunDecl.signature(): How did we get here? This match should always work!");
        }
    }

    fn new_uninit(
        r#type: Type,
        storage_class: Option<StorageClass>,
        name: Rc<String>,
        params: Vec<Option<Rc<String>>>,
    ) -> Self {
        Self {
            r#type,
            name,
            params,
            block: None,
            storage_class,
        }
    }

    fn check_for_definition(mut self, tokens: &[Token]) -> Result<(Self, &[Token])> {
        ensure!(
            self.block.is_none(),
            "ast.FunDecl.check_for_definition: Attempting to check for definition on function which already has one."
        );

        let (block, tokens) = match tokens {
            [Token::Semi, tokens @ ..] => (None, tokens),
            [Token::LSquirly, ..] => {
                let (block, tokens) = Block::consume(tokens)
                    .context("Failed to parse block within function definition.")?;
                (Some(block), tokens)
            }
            [token, ..] => bail!(
                "Expected \";\" or \"{{\" after function signature but found token: {}.",
                token
            ),
            [] => {
                bail!("Expected \";\" or \"{{\" after function signature but found no more tokens.")
            }
        };
        self.block = block;
        Ok((self, tokens))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl {
    pub r#type: Type,
    pub name: Rc<String>,
    pub init: Option<Initializer>,
    pub storage_class: Option<StorageClass>,
}

impl VarDecl {
    fn check_for_definition(mut self, tokens: &[Token]) -> Result<(Self, &[Token])> {
        match tokens {
            [Token::Assign, tokens @ ..] => {
                let (initializer, tokens) = Initializer::consume(tokens)?;
                let tokens = eat_semi(tokens)
                    .context("Semicolon required after expression in variable declaration")?;
                self.init = Some(initializer);
                Ok((self, tokens))
            }
            [Token::Semi, tokens @ ..] => Ok((self, tokens)),
            _ => bail!("Unable to parse valid variable declaration."),
        }
    }

    fn new_uninit(r#type: Type, storage_class: Option<StorageClass>, name: Rc<String>) -> Self {
        Self {
            r#type,
            name,
            init: None,
            storage_class,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDecl {
    pub tag: Rc<String>,
    pub members: Vec<MemberDecl>,
}

impl StructDecl {
    pub fn consume(tokens: &[Token], tag: Rc<String>) -> Result<Option<(Self, &[Token])>> {
        let mut members = vec![];
        match tokens {
            [Token::LSquirly, tokens @ ..] => {
                let mut tokens = tokens;
                let error;
                loop {
                    let val = MemberDecl::consume(tokens);
                    let Ok((member, remaining)) = val else {
                        error = Some(val);
                        break;
                    };
                    members.push(member);
                    match remaining {
                        [Token::Semi, remaining @ ..] => tokens = remaining,
                        [remaining @ ..] => bail!(
                            "Expected semi colon after member declaration, found {:?} instead",
                            remaining.first()
                        ),
                    }
                }
                ensure!(
                    !members.is_empty(),
                    "cannot declare struct declaration with empty member list"
                );
                match tokens {
                    [Token::RSquirly, Token::Semi, tokens @ ..] => {
                        Ok(Some((Self { tag, members }, tokens)))
                    }
                    [tokens @ ..] => bail!(
                        "struct with member declaration must be followed by semicolon, found {:?} instead. Error from member decl parse: {error:?}",
                        tokens.first()
                    ),
                }
            }
            [Token::Semi, tokens @ ..] => Ok(Some((StructDecl { tag, members }, tokens))),
            _ => Ok(None),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnionDecl {
    pub tag: Rc<String>,
    pub members: Vec<MemberDecl>,
}

impl UnionDecl {
    pub fn consume(tokens: &[Token], tag: Rc<String>) -> Result<Option<(Self, &[Token])>> {
        let mut members = vec![];
        match tokens {
            [Token::LSquirly, tokens @ ..] => {
                let mut tokens = tokens;
                let error;
                loop {
                    let val = MemberDecl::consume(tokens);
                    let Ok((member, remaining)) = val else {
                        error = Some(val);
                        break;
                    };
                    members.push(member);
                    match remaining {
                        [Token::Semi, remaining @ ..] => tokens = remaining,
                        [remaining @ ..] => bail!(
                            "Expected semi colon after member declaration, found {:?} instead",
                            remaining.first()
                        ),
                    }
                }
                ensure!(
                    !members.is_empty(),
                    "cannot declare union declaration with empty member list"
                );
                match tokens {
                    [Token::RSquirly, Token::Semi, tokens @ ..] => {
                        Ok(Some((Self { tag, members }, tokens)))
                    }
                    [tokens @ ..] => bail!(
                        "struct with member declaration must be followed by semicolon, found {:?} instead. Error from member decl parse: {error:?}",
                        tokens.first()
                    ),
                }
            }
            [Token::Semi, tokens @ ..] => Ok(Some((Self { tag, members }, tokens))),
            _ => Ok(None),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MemberDecl {
    pub name: Rc<String>,
    pub r#type: Type,
}

impl MemberDecl {
    pub fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        let (stream_offset, base, storage_class) = TypeBuilder::new(tokens)
            .build()
            .context("Error building base type from token stream.")?;
        ensure!(
            storage_class.is_none(),
            "member variables are not allowed to have storage class"
        );
        ensure!(!base.is_function(), "member variables cannot be functions");

        let (declarator, tokens) = Declarator::consume(&tokens[stream_offset..])
            .context("ast.MemberDecl.consume(): error while parsing member declarator")?;

        ensure!(
            !matches!(declarator, Declarator::Fun { .. }),
            "member declarations for structs can't be functions",
        );

        let (Some(name), r#type, ..) = Declarator::process(declarator, base)
            .context("could not process declarator of member declaration")?
        else {
            bail!("member declarations need to be named");
        };

        Ok((MemberDecl { name, r#type }, tokens))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    FunDecl(FunDecl),
    VarDecl(VarDecl),
    StructDecl(StructDecl),
}

impl Declaration {
    pub fn defining(&self) -> bool {
        match self {
            Declaration::FunDecl(decl) => decl.block.is_some(),
            Declaration::VarDecl(decl) => decl.init.is_some(),
            Declaration::StructDecl(..) => unreachable!(),
        }
    }

    pub fn storage_class(&self) -> Option<&StorageClass> {
        match self {
            Declaration::FunDecl(decl) => decl.storage_class.as_ref(),
            Declaration::VarDecl(decl) => decl.storage_class.as_ref(),
            Declaration::StructDecl(..) => unreachable!(),
        }
    }

    pub fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        let (stream_offset, base, storage_class) = TypeBuilder::new(tokens)
            .build()
            .context("Error building base type from token stream.")?;

        // Structs are to be handled seperatly
        if base.is_struct() {
            let BaseType::Struct(ref tag) = base.base else {
                unreachable!()
            };
            if let Some((struct_decl, tokens)) =
                StructDecl::consume(&tokens[stream_offset..], Rc::clone(tag))
                    .context("failed to parse struct declaration after parsing struct type")?
            {
                ensure!(
                    storage_class.is_none(),
                    "structs cannot have an associated storage class"
                );
                return Ok((Declaration::StructDecl(struct_decl), tokens));
            }
        }
        let (declarator, tokens) = Declarator::consume(&tokens[stream_offset..])
            .context("ast.Declaration.consume(): Error while parsing declarator.")?;

        // This is ugly, but the book demands it be a parse error instead of the much
        // more sensible type error. So here it is...
        match declarator {
            Declarator::Array { decl, .. } if matches!(*decl, Declarator::Fun { .. }) => {
                bail!(
                    "Arrays declarators cannot hold functions, also known as: annoying test case that should be a type error but is a parse error >:(("
                )
            }
            _ => {}
        }

        let (name, decl_type, params) = Declarator::process(declarator, base)
            .context("ast.Declaration.consume(): Error while processing declarator.")?;
        let name =
            name.expect("ast.Declaration.consume(): Declaration must have name being declared.");

        match decl_type {
            Type {
                base: BaseType::Fun { .. },
                ..
            } => {
                let (decl, tokens) = FunDecl::new_uninit(
                    decl_type,
                    storage_class,
                    Rc::clone(&name),
                    params,
                )
                .check_for_definition(tokens)
                .context(format!(
                    "ast.Declaration.consume(): Error parsing function declaration \"{name}\"."
                ))?;
                Ok((Declaration::FunDecl(decl), tokens))
            }
            _ => {
                let (decl, tokens) = VarDecl::new_uninit(
                    decl_type,
                    storage_class,
                    Rc::clone(&name),
                )
                .check_for_definition(tokens)
                .context(format!(
                    "ast.Declaration.consume(): Error parsing variable declaration for \"{name}\"."
                ))?;
                Ok((Declaration::VarDecl(decl), tokens))
            }
        }
    }
}

impl From<&Declaration> for Type {
    fn from(value: &Declaration) -> Self {
        match value {
            Declaration::FunDecl(decl) => decl.r#type.clone(),
            Declaration::VarDecl(decl) => decl.r#type.clone(),
            Declaration::StructDecl(..) => todo!(),
        }
    }
}
