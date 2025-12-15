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
        dst: Val,
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
}

impl Instruction {
    pub(crate) fn parse_decl_with(
        decl: ast::Declaration,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        match decl {
            ast::Declaration::VarDecl(decl) => {
                Self::parse_var_decl_with(decl, symbols, make_temp_var)
            }
            ast::Declaration::FunDecl(decl) => Self::parse_fun_decl_with(decl, make_temp_var),
        }
    }

    pub(crate) fn parse_fun_decl_with(
        decl: ast::FunDecl,
        _make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        assert!(decl.block.is_none());
        vec![]
    }

    pub(crate) fn process_initializer_rec(
        base: usize,
        in_array: bool,
        name: Rc<String>,
        init: ast::Initializer,
        r#type: &ast::Type,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        match init {
            ast::Initializer::SingleInit(init) => {
                let Expr {
                    mut instructions,
                    val: src,
                } = Expr::parse_with_and_convert(*init, symbols, make_temp_var);
                if in_array {
                    instructions.push(Instruction::CopyToOffset {
                        src,
                        dst: Rc::clone(&name),
                        offset: base.try_into().unwrap(),
                    });
                } else {
                    let dst = Val::Var(name);
                    instructions.push(Instruction::Copy {
                        src,
                        dst: dst.clone(),
                    });
                }
                instructions
            }
            ast::Initializer::CompundInit(inits) => {
                let mut instructions = vec![];
                let per_element_size = r#type.base.size_of_base_type();
                let mut current_base = base;
                for init in inits {
                    instructions.extend(Self::process_initializer_rec(
                        current_base,
                        true,
                        name.clone(),
                        init,
                        r#type,
                        symbols,
                        make_temp_var,
                    ));
                    current_base += per_element_size;
                }
                instructions
            }
        }
    }

    pub(crate) fn process_initializer(
        name: Rc<String>,
        init: ast::Initializer,
        r#type: &ast::Type,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        Self::process_initializer_rec(0, false, name, init, r#type, symbols, make_temp_var)
    }

    pub(crate) fn parse_var_decl_with(
        decl: ast::VarDecl,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        if decl.storage_class != Some(ast::StorageClass::Extern) {
            symbols.new_entry(Rc::clone(&decl.name), decl.r#type.clone());
        }
        match decl.init {
            Some(init) => Self::process_initializer(
                Rc::clone(&decl.name),
                init,
                &decl.r#type,
                symbols,
                make_temp_var,
            ),
            _ => vec![],
        }
    }

    pub(crate) fn parse_stmt_with(
        stmt: ast::Stmt,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        match stmt {
            ast::Stmt::Null => vec![],
            ast::Stmt::Return(Some(expr)) => {
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with_and_convert(expr, symbols, make_temp_var);
                instructions.push(Instruction::Return(Some(val)));
                instructions
            }
            ast::Stmt::Return(None) => {
                vec![Instruction::Return(None)]
            }
            ast::Stmt::Expr(expr) => {
                let Expr { instructions, .. } =
                    Expr::parse_with_and_convert(expr, symbols, make_temp_var);
                instructions
            }
            ast::Stmt::Compound(block) => Self::parse_block_with(block, symbols, make_temp_var),
            ast::Stmt::Goto(label) => {
                vec![Instruction::Jump(label)]
            }
            ast::Stmt::Label { name, stmt } => {
                let mut block_instructions = vec![Instruction::Label(name)];
                block_instructions.extend(Self::parse_stmt_with(*stmt, symbols, make_temp_var));
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
            ast::Stmt::While { .. } => parse_while(stmt, symbols, make_temp_var),
            ast::Stmt::DoWhile { .. } => parse_do_while(stmt, symbols, make_temp_var),
            ast::Stmt::For { .. } => parse_for(stmt, symbols, make_temp_var),
            ast::Stmt::If { .. } => parse_if(stmt, symbols, make_temp_var),
            ast::Stmt::Case {
                value: _,
                stmt,
                label,
            } => {
                let label = label.expect("Case must have label");
                let mut block_instructions = vec![Instruction::Label(Rc::clone(&label))];
                block_instructions.extend(Self::parse_stmt_with(*stmt, symbols, make_temp_var));
                block_instructions
            }
            ast::Stmt::Default { label, stmt } => {
                let label = label.expect("Default must have label");
                let mut block_instructions = vec![Instruction::Label(Rc::clone(&label))];
                block_instructions.extend(Self::parse_stmt_with(*stmt, symbols, make_temp_var));
                block_instructions
            }
            ast::Stmt::Switch { .. } => parse_switch(stmt, symbols, make_temp_var),
        }
    }

    pub(crate) fn parse_block_with(
        node: ast::Block,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        let mut block_instructions = vec![];
        for item in node.into_items().into_iter() {
            match item {
                // Statics already get initialized at the top level.
                // If we reinitialized them here they would act like local
                // variables (suboptimal)
                ast::BlockItem::Decl(ast::Declaration::VarDecl(ast::VarDecl {
                    storage_class: Some(ast::StorageClass::Static),
                    ..
                })) => {}
                ast::BlockItem::Decl(decl) => {
                    block_instructions.extend(Self::parse_decl_with(decl, symbols, make_temp_var));
                }
                ast::BlockItem::Stmt(stmt) => {
                    block_instructions.extend(Self::parse_stmt_with(stmt, symbols, make_temp_var));
                }
            }
        }
        block_instructions
    }
}
