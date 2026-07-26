use super::*;

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub external_linkage: bool,
    pub signature: Vec<(ast::Type, Option<Rc<String>>)>,
    pub instructions: Vec<Instruction>,
}

impl Function {
    pub(crate) fn from_symbol(decl: ast::FunDecl, ctx: &mut Ctx) -> Option<Self> {
        // Insert function parameter types for type inference
        // FIXME: Make sure that parameter names are unique!!!!
        let mut signature = vec![];
        for (r#type, name) in decl
            .signature()
            .expect("tacky.Function.from_symbol(): Error getting function declaration signature.")
            .into_iter()
        {
            if let Some(name) = name {
                ctx.symbols.new_entry(Rc::clone(name), r#type.clone());
                signature.push((r#type.clone(), Some(Rc::clone(name))));
            } else {
                signature.push((r#type.clone(), None));
            }
        }

        let ast::FunDecl {
            name,
            block: Some(block),
            ..
        } = decl
        else {
            return None;
        };

        ctx.push_scope(name.as_ref());
        let mut instructions = Instruction::parse_block_with(block, ctx);
        ctx.pop_scope();

        // Temporary fix suggested by the book for the case where a function
        // is supposed to return something but does not.
        instructions.push(Instruction::Return(Some(Val::Constant(
            ast::Constant::I32(0),
        ))));

        let name = Rc::new(name.to_string());

        // Check symbol table to get external linkage since the function
        // declaration could be static but the definition can elide it.
        // ```
        // static int foo();
        // int foo() {
        //      ...
        // }
        // ```

        let external_linkage = {
            if let Some(SymbolEntry {
                r#type:
                    ast::Type {
                        base: ast::BaseType::Fun { .. },
                        ..
                    },
                attribute,
            }) = ctx.symbols.get(&name)
            {
                attribute.has_external_linkage()
            } else {
                unreachable!()
            }
        };

        Some(Function {
            name,
            signature,
            external_linkage,
            instructions,
        })
    }
}
