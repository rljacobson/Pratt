/*!

Parsing strings to expressions.

*/

mod lexer;
mod operator;
mod parser;

pub use lexer::Lexer;
pub use parser::Parser;


#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
