use super::*;

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    Fun(Function),
    Static(StaticVariable),
}
