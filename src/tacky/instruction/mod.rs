use crate::{sema::tc::MemberEntry, tacky::mov_chunker::MovChunker};

use super::*;

mod conditionals;
mod loops;

use conditionals::*;
use loops::*;

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Return(Option<Val>),
    SignExtend {
        src: Val,
        dst: Val,
    },
    ZeroExtend {
        src: Val,
        dst: Val,
    },
    DoubleToInt {
        src: Val,
        dst: Val,
    },
    IntToDouble {
        src: Val,
        dst: Val,
    },
    DoubleToUInt {
        src: Val,
        dst: Val,
    },
    UIntToDouble {
        src: Val,
        dst: Val,
    },
    Truncate {
        src: Val,
        dst: Val,
    },
    Unary {
        op: UnaryOp,
        src: Val,
        dst: Val,
    },
    Binary {
        op: BinaryOp,
        src1: Val,
        src2: Val,
        dst: Val,
    },
    Copy {
        src: Val,
        dst: Val,
    },
    GetAddress {
        src: Val,
        dst: Val,
    },
    Load {
        src_ptr: Val,
        dst: Val,
    },
    Store {
        src: Val,
        dst_ptr: Val,
    },
    Jump(Rc<String>),
    JumpIfZero {
        condition: Val,
        target: Rc<String>,
    },
    JumpIfNotZero {
        condition: Val,
        target: Rc<String>,
    },
    Label(Rc<String>),
    FunCall {
        name: Rc<String>,
        args: Vec<Val>,
        dst: Option<Val>,
    },
    AddPtr {
        ptr: Val,
        index: Val,
        scale: usize,
        dst: Val,
    },
    CopyToOffset {
        src: Val,
        dst: Rc<String>,
        offset: isize,
    },
    CopyFromOffset {
        src: Rc<String>,
        dst: Val,
        offset: isize,
    },
}

impl Instruction {
    pub(crate) fn parse_decl_with(decl: ast::Declaration, ctx: &mut Ctx) -> Vec<Self> {
        match decl {
            ast::Declaration::Var(decl) => Self::parse_var_decl_with(decl, ctx),
            ast::Declaration::Fun(decl) => Self::parse_fun_decl_with(decl),
            ast::Declaration::Struct(..) => vec![],
            ast::Declaration::Union(..) => vec![],
        }
    }

    pub(crate) fn parse_fun_decl_with(decl: ast::FunDecl) -> Vec<Self> {
        assert!(decl.block.is_none());
        vec![]
    }

    pub(crate) fn process_initializer_rec(
        base: &mut usize,
        in_array: bool,
        name: Rc<String>,
        init: ast::Initializer,
        r#type: &ast::Type,
        ctx: &mut Ctx,
    ) -> Vec<Self> {
        match init {
            ast::Initializer::SingleInit(expr)
                if r#type.is_array() && matches!(&*expr, ast::Expr::String { .. }) =>
            {
                let ast::Expr::String { value } = *expr else {
                    unreachable!()
                };
                let len = r#type.size_of();
                let instructions =
                    MovChunker::new(value.as_bytes(), Rc::clone(&name), len, *base).collect();
                *base += len;
                instructions
            }
            ast::Initializer::SingleInit(init) => {
                let Expr {
                    mut instructions,
                    val: src,
                } = Expr::parse_with_and_convert(*init, ctx);
                if in_array {
                    instructions.push(Instruction::CopyToOffset {
                        src,
                        dst: Rc::clone(&name),
                        offset: (*base).try_into().unwrap(),
                    });
                    *base += r#type.size_of();
                } else {
                    instructions.push(Instruction::Copy {
                        src,
                        dst: Val::Var(name),
                    });
                }
                instructions
            }
            ast::Initializer::CompoundInit(inits) => {
                // Get a trait object over Iterator<&ast::Type> to avoid making
                // expensive clones for array/struct types and be able to
                // uniformly handle the compound initializers below.
                let members: Rc<[MemberEntry]>;
                let element_types: Box<dyn Iterator<Item = &ast::Type> + '_> = match &r#type.base {
                    ast::BaseType::Array { element, size } => {
                        Box::new(std::iter::repeat_n(element.as_ref(), *size))
                    }
                    ast::BaseType::Struct { tag, .. } => {
                        members = ctx.get_struct(tag).members.clone();
                        Box::new(members.iter().map(|mem| &mem.r#type))
                    }
                    _ => Box::new(std::iter::once(r#type)),
                };
                let mut instructions = vec![];
                for (init, element_t) in inits.into_iter().zip(element_types) {
                    instructions.extend(Self::process_initializer_rec(
                        base,
                        true,
                        name.clone(),
                        init,
                        element_t,
                        ctx,
                    ));
                }
                instructions
            }
        }
    }

    pub(crate) fn process_initializer(
        name: Rc<String>,
        init: ast::Initializer,
        r#type: &ast::Type,
        ctx: &mut Ctx,
    ) -> Vec<Self> {
        let mut base = 0;
        Self::process_initializer_rec(&mut base, false, name, init, r#type, ctx)
    }

    pub(crate) fn parse_var_decl_with(decl: ast::VarDecl, ctx: &mut Ctx) -> Vec<Self> {
        if decl.storage_class != Some(ast::StorageClass::Extern) {
            ctx.symbols
                .new_entry(Rc::clone(&decl.name), decl.r#type.clone());
        }
        match decl.init {
            Some(init) => Self::process_initializer(Rc::clone(&decl.name), init, &decl.r#type, ctx),
            _ => vec![],
        }
    }

    pub(crate) fn parse_stmt_with(stmt: ast::Stmt, ctx: &mut Ctx) -> Vec<Self> {
        match stmt {
            ast::Stmt::Null => vec![],
            ast::Stmt::Return(Some(expr)) => {
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with_and_convert(expr, ctx);
                instructions.push(Instruction::Return(Some(val)));
                instructions
            }
            ast::Stmt::Return(None) => {
                vec![Instruction::Return(None)]
            }
            ast::Stmt::Expr(expr) => {
                let Expr { instructions, .. } = Expr::parse_with_and_convert(expr, ctx);
                instructions
            }
            ast::Stmt::Compound(block) => Self::parse_block_with(block, ctx),
            ast::Stmt::Goto(label) => {
                vec![Instruction::Jump(label)]
            }
            ast::Stmt::Label { name, stmt } => {
                let mut block_instructions = vec![Instruction::Label(name)];
                block_instructions.extend(Self::parse_stmt_with(*stmt, ctx));
                block_instructions
            }
            ast::Stmt::Break(label) => {
                let label = Rc::new(format!("{}.break", label.unwrap()));
                vec![Instruction::Jump(label)]
            }
            ast::Stmt::Continue(label) => {
                let label = Rc::new(format!("{}.continue", label.unwrap()));
                vec![Instruction::Jump(label)]
            }
            ast::Stmt::While { .. } => parse_while(stmt, ctx),
            ast::Stmt::DoWhile { .. } => parse_do_while(stmt, ctx),
            ast::Stmt::For { .. } => parse_for(stmt, ctx),
            ast::Stmt::If { .. } => parse_if(stmt, ctx),
            ast::Stmt::Case {
                value: _,
                stmt,
                label,
            } => {
                let label = label.expect("Case must have label");
                let mut block_instructions = vec![Instruction::Label(Rc::clone(&label))];
                block_instructions.extend(Self::parse_stmt_with(*stmt, ctx));
                block_instructions
            }
            ast::Stmt::Default { label, stmt } => {
                let label = label.expect("Default must have label");
                let mut block_instructions = vec![Instruction::Label(Rc::clone(&label))];
                block_instructions.extend(Self::parse_stmt_with(*stmt, ctx));
                block_instructions
            }
            ast::Stmt::Switch { .. } => parse_switch(stmt, ctx),
        }
    }

    pub(crate) fn parse_block_with(node: ast::Block, ctx: &mut Ctx) -> Vec<Self> {
        let mut block_instructions = vec![];
        for item in node.into_items().into_iter() {
            match item {
                // Statics already get initialized at the top level.
                // If we reinitialized them here they would act like local
                // variables (suboptimal)
                ast::BlockItem::Decl(ast::Declaration::Var(ast::VarDecl {
                    storage_class: Some(ast::StorageClass::Static),
                    ..
                })) => {}
                ast::BlockItem::Decl(decl) => {
                    block_instructions.extend(Self::parse_decl_with(decl, ctx));
                }
                ast::BlockItem::Stmt(stmt) => {
                    block_instructions.extend(Self::parse_stmt_with(stmt, ctx));
                }
            }
        }
        block_instructions
    }
}
