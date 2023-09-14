/*!

Primitive expression node types.

 */

use std::{
  fmt::{Display, Formatter},
  rc::Rc
};

use crate::interned_string::{interned_static, IString};


#[derive(Clone, PartialEq, Debug)]
pub enum Atom {
  String(IString),
  Integer(IString),
  Real(IString),
  Symbol(IString),
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

      Atom::String(_) => {static_symbol("String")}
      Atom::Integer(_) => {static_symbol("Integer")}
      Atom::Real(_) => {static_symbol("Real")}

      symbol => symbol.clone()
    }
  }

  pub fn name(&self) -> Option<IString> {
    match self {
      Atom::SExpression(_) => {
        match self.head() {
          Atom::Symbol(name) => Some(name),
          _                  => None
        }
      },
      Atom::Symbol(name) => Some(name.clone()),
      _                  => None
    }
  }

  pub fn is_number(&self) -> bool {
    match self {
      Atom::Integer(_)
      | Atom::Real(_) => true,
      _ => false,
    }
  }

  /// We often have a need to create an expression for some standard built-in or stdlib symbol.
  pub fn function_with_str_head(head_str: &'static str) -> Atom {
    Atom::SExpression(Rc::new(vec![static_symbol(head_str)]))
  }

  /// We often have a need to create an expression for some standard built-in or stdlib symbol.
  pub fn function_with_symbolic_head(head: IString) -> Atom {
    Atom::SExpression(Rc::new(vec![Atom::Symbol(head)]))
  }

}

impl Display for Atom {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    // write!(f, "{}", self.formatted())
    let text = match self {

      Atom::String(s)           => {
        format!("\"{}\"", s)
      }

      Atom::Integer(n)          => {
        format!("{}", n)
      }

      Atom::Real(r)             => {
        format!("{:.4}", r)
      }

      Atom::Symbol(name) => {
        format!("{}", name)
      }

      Atom::SExpression(children) => {
        let mut child_iter = children.iter();
        let head           = child_iter.next().unwrap().to_string();
        let rest           = child_iter.map(|e| e.to_string()).collect::<Vec<_>>().join(", ");

        format!("{}[{}]", head, rest)
      }
    };

    write!(f, "{}", text)
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
