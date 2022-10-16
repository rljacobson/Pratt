/*!

A `Context` is a namespace. A `Context` struct is a symbol table that holds the
values, definitions, and attributes for symbols within a context.

Much of this should arguably be implemented in the language being implemented.
However, doing so would mean that information about the original expression that
created the symbol and its definitions would not be retained.


*/

use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::Peekable;
use string_interner::StringInterner;
use string_interner::symbol::SymbolU32;

use crate::{
  atom::{
    Atom,
    static_symbol
  },
  attributes::{
    Attributes,
    Attribute
  },
  builtins::{
    BuiltinFn,
    register_builtins
  },
};
use crate::parsing::{Lexer, parse};


pub type InternedString = SymbolU32;


pub struct Context{
  // todo: Should there be a context path object?
  name   : InternedString,
  symbols: HashMap<InternedString, SymbolRecord>,
  /// This interner is currently not thread safe.
  interner: RefCell<StringInterner>,
}

impl Context {

  pub fn new_global_context() -> Context {
    let mut interner = StringInterner::default();
    let name = interner.interned_static("Global");
    let mut context = Context{
      name,
      symbols: HashMap::new(),
      interner: RefCell::new(interner)
    };

    register_builtins(&mut context);
    context
  }

  // region Getters and Setters

  pub fn get_symbol(&mut self, symbol: InternedString) ->  &mut SymbolRecord {
    self.symbols.entry(symbol).or_insert_with(
      | | {
        SymbolRecord::new(symbol)
      }
    )
  }

  /// This method does not check for read-only! Only use for registering built-ins.
  pub(crate) fn set_down_value_attribute(&mut self, symbol: InternedString, value: SymbolValue, attributes: Attributes) {
    let mut record = self.get_symbol(symbol);
    record.down_values.push(value);
    record.attributes.update(attributes);
  }

  pub fn set_attribute(&mut self, symbol: InternedString, attribute: Attribute) -> Result<(), String> {
    let mut record = self.get_symbol(symbol);

    if record.attributes.attributes_read_only() {
      Err(format!("Symbol {} has read-only attributes", self.resolve_str(symbol)))
    } else {
      record.attributes.set(attribute);
      Ok(())
    }
  }

  pub fn set_down_value(&mut self, symbol: InternedString, value: SymbolValue) -> Result<(), String> {
    let record = self.get_symbol(symbol);

    if record.attributes.read_only() {
      Err(format!("Symbol {} is read-only", self.resolve_str(symbol)))
    } else {
      record.down_values.push(value);
      Ok(())
    }
  }

  pub fn set_up_value(&mut self, symbol: InternedString, value: SymbolValue) -> Result<(), String> {
    let mut record = self.get_symbol(symbol);

    if record.attributes.read_only() {
      Err(format!("Symbol {} is read-only", self.resolve_str(symbol)))
    } else {
      record.up_values.push(value);
      Ok(())
    }
  }

  pub fn set_own_value(&mut self, symbol: InternedString, value: SymbolValue) -> Result<(), String> {
    let mut record = self.get_symbol(symbol);

    if record.attributes.read_only() {
      Err(format!("Symbol {} is read-only", self.resolve_str(symbol)))
    } else {
      record.own_values.push(value);
      Ok(())
    }
  }

  pub fn set_sub_value(&mut self, symbol: InternedString, value: SymbolValue) -> Result<(), String> {
    let mut record = self.get_symbol(symbol);

    if record.attributes.read_only() {
      Err(format!("Symbol {} is read-only", self.resolve_str(symbol)))
    } else {
      record.sub_values.push(value);
      Ok(())
    }
  }

  // todo: Not especially efficient if the symbol was never defined.
  pub fn clear_symbol(&mut self, symbol: InternedString) -> Result<(), String> {
    { // Scope for record
      let mut record = self.get_symbol(symbol);
      if record.attributes.read_only() || record.attributes.protected() {
        return Err(format!("Symbol {} is read-only", self.resolve_str(symbol)))
      }
    }
    self.symbols.remove(&symbol) ;
    Ok(())
  }
/*
  pub fn get_up_values(&self, symbol: InternedString) -> Option<Vec<SymbolValue>> {
    match self.symbols[symbol] {
      None => None,
      Some(record) => {
        // todo: get rid of this clone.
        Some(record.up_values.clone())
      }
    }
  }
  */
/*
  pub fn get_own_values(&self, symbol: InternedString) -> Option<Vec<SymbolValue>> {
    match self.symbols[symbol] {
      None => None,
      Some(record) => {
        // todo: get rid of this clone.
        Some(record.own_values.clone())
      }
    }
  }
*/

  // endregion

  // region String Interning Methods

  pub fn interned(&mut self, string: &str) -> InternedString {
    self.interner.borrow_mut().get_or_intern(string)
  }


  pub fn interned_static(&mut self, string: &'static str) -> InternedString {
    self.interner.borrow_mut().get_or_intern_static(string)
  }


  pub fn get_interned(&self, string: &str) -> Option<InternedString> {
    self.interner.borrow().get(string)
  }

  pub fn resolve_str(&self, symbol: InternedString) -> &'static str {
    self.interner.borrow().resolve(symbol).unwrap()
  }

  // Does not automatically unwrap the resolved `&str`.
  pub fn resolve_str_checked(&self, symbol: InternedString) -> Option<&'static str> {
    self.interner.borrow().resolve(symbol)
  }

  // endregion

}

pub struct SymbolRecord {
  pub symbol: InternedString,
  pub attributes: Attributes,

  /// OwnValues define how the symbol appearing alone should be evaluated. They have the form `x :> expr` or `x=expr`.
  pub own_values: Vec<SymbolValue>,

  /// UpValues define how M-expressions having the symbol as an argument should be evaluated. They typically have the
  /// form `f[pattern,g[pattern],pattern]:>expr`. UpValues are applied before DownValues.
  pub up_values:  Vec<SymbolValue>,

  /// DownValues define how M-expressions having the symbol as their head should be evaluated. They typically have the
  /// form `f[pattern]:>expr`
  pub down_values: Vec<SymbolValue>,

  /// SubValues define how M-expressions having an M-expression with the symbol as a head should be evaluated. They
  /// typically have the form `f[pat][pat]:>exp`.
  pub sub_values: Vec<SymbolValue>,
}

impl SymbolRecord {
  pub fn new(name: InternedString) -> SymbolRecord{
    SymbolRecord {
      symbol: name,
      attributes: Default::default(),
      own_values: vec![],
      up_values: vec![],
      down_values: vec![],
      sub_values: vec![]
    }
  }
}


/// A `SymbolValue` is a wrapper for `RuleDelayed` used for storing the rule in a symbol table as an own/up/down/sub
/// value. The wrapper provides convenience methods and stores the expression that originally created the value.
#[derive(Clone)]
pub enum SymbolValue{
  Definitions {
    def: Atom, // The original (sub)expression used to create this `SymbolValue`.
    lhs: Atom, // Treated as if wrapped in HoldPattern
    rhs: Atom,
    condition: Option<Atom>,
  },
  BuiltIn {
    pattern  : Atom,
    condition: Option<Atom>,
    built_in : BuiltinFn
  }
}
