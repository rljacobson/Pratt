mod ast;
mod parsing;
mod interned_string;

pub use parsing::{
    Lexer,
    Parser
};
pub use interned_string::{
    IString,
    intern,
    intern_str,
    interned_static
};
pub use ast::{
    Atom
};

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
