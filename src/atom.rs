/*!

Primitive expression node types.

 */

use std::{
  fmt::{Display, Formatter},
  rc::Rc
};

use strum_macros::{
  IntoStaticStr
};
use rug::{
  Integer as BigInteger,
  Float as BigFloat,
};


use crate::{
  context::{
    Context,
    InternedString
  },
  attributes::{
    Attribute,
    Attributes
  },
  formatter::ExpressionFormatter
};

#[derive(Clone, PartialEq, IntoStaticStr)]
pub enum Atom {
  String(InternedString),
  Integer(BigInteger),
  Real(BigFloat),
  Symbol(InternedString),
  SExpression(Rc<Vec<Atom>>)
}

impl Atom {
  pub fn head(&self) -> Atom {
    match self {
      Atom::SExpression(children) => {
        match children.first() {
          Some(expression) => expression.clone(),
          None => headless_s_expression(),
        }
      }

      atom => {
        static_symbol(atom.into())
      }
    }
  }

  pub fn name(&self) -> Option<InternedString> {
    match self {
      Atom::SExpression(_) => {
        match self.head() {
          Atom::Symbol(name) => Some(name),
          _                  => None
        }
      },
      Atom::Symbol(name) => Some(*name),
      _                  => None
    }
  }

  pub fn is_variable(&self) -> bool {
    match self.head() {
      h if h == static_symbol("Blank") => true,
      _ => false,
    }
  }

  pub fn is_sequence_variable(&self) -> bool {
    match self.head() {
      h if h == static_symbol("Sequence") || h == static_symbol("BlankSequence") => true,
      _ => false,
    }
  }

  pub fn is_number(&self) -> bool {
    match self {
      Atom::Integer(_)
      | Atom::Real(_) => true,
      _ => false,
    }
  }

  pub fn is_numeric_function(&self, context: &mut Context) -> bool {
    match self.head() {
      Atom::Symbol(name) => {
        let record = context.get_symbol(name);
        record.attributes.numeric_function()
      },
      _ => false,
    }
  }

  /// A formatter knows how to format expressions in a given style. Certain expressions and symbols may have their
  /// own custom formatting function stored in the `Context`, which the formatter takes into account. What's more,
  /// parent expressions can affect how child expressions are formatted.
  pub fn formatted(&self) -> String {
    let formatter = ExpressionFormatter::default();
    formatter.format(self.clone())
  }

  // region Convenience constructor functions

  /// We often have a need to create an expression for some standard built-in or stdlib symbol.
  pub fn function_with_str_head(head_str: &'static str) -> Atom {
    Atom::SExpression(Rc::new(vec![static_symbol(head_str)]))
  }

  /// We often have a need to create an expression for some standard built-in or stdlib symbol.
  pub fn function_with_symbolic_head(head: InternedString) -> Atom {
    Atom::SExpression(Rc::new(vec![Atom::Symbol(head)]))
  }
  // endregion
}

impl Display for Atom {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.formatted())
  }
}

/// We often have a need to create an expression for some standard built-in or stdlib symbol.
pub(crate) fn static_symbol(name: &'static str) -> Atom {
  Atom::Symbol(interned_static(name))
}

/// A critical error state.
fn headless_s_expression() -> ! {
  unreachable!("Encountered an S-expression without a head, which is impossible. This is a bug.")
}


#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
