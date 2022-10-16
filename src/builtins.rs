/*!

Built-in functionality that cannot be implemented in the guest language.

*/



use crate::atom::Atom;
use crate::context::Context;

pub type BuiltinFn = fn(Atom, Context) -> Atom;


pub fn register_builtins(_context: &mut Context) {

}



#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
