/*!

Formatting of expressions.

A formatter knows how to format expressions in a given style. Certain expressions and symbols may have their
own custom formatting function stored in the `Context`, which the formatter takes into account. What's more,
parent expressions can affect how child expressions are formatted.

*/



use std::rc::Rc;
use crate::{
  atom::Atom,
  context::Context
};

pub enum FormatStyle {
  Traditional,
  Standard,
  FullForm,
}

// todo: Make an expression formatter.
pub struct ExpressionFormatter {
  context: Rc<Context>
}

impl ExpressionFormatter {

  pub fn new(context: Rc<Context>) -> ExpressionFormatter {
    ExpressionFormatter{
      context
    }
  }

  pub fn format(&self, expression: Atom) -> String {
    match expression {

      Atom::String(s)           => {
        format!("\"{}\"", self.context.resolve_str(s))
      }

      Atom::Integer(n)          => {
        format!("{}", n)
      }

      Atom::Real(r)             => {
        format!("{:.4}", r)
      }

      Atom::Symbol(name) => {
        format!("{}", self.context.resolve_str(name))
      }

      Atom::SExpression(children) => {
        let mut child_iter = children.iter();
        let mut head       = self.format(child_iter.next().unwrap().clone());
        let rest           = child_iter.map(|e| self.format(e.clone())).collect::<Vec<_>>().join(", ");

        format!("{}[{}]", head, rest)
      }
    }
  }
}


#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
