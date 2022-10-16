/*!
A global dictionary of interned strings. This is currently not thread safe. Provides an abstraction API for any
interner library.

*/

use lazy_static::lazy_static;
use string_interner::{
  StringInterner,
  symbol::SymbolU32
};

pub type InternedString = SymbolU32;


static mut STRING_INTERNER: Option<&'static mut StringInterner> = None;


pub fn interned(string: &str) -> InternedString {
  unsafe {
    if STRING_INTERNER.is_none() {
      STRING_INTERNER = &mut StringInterner::default();
    }
    STRING_INTERNER.get_or_intern(string)
  }
}


pub fn interned_static(string: &'static str) -> InternedString {
  unsafe {
    if STRING_INTERNER.is_none() {
      STRING_INTERNER = &mut StringInterner::default();
    }
    STRING_INTERNER.get_or_intern_static(string)
  }
}


pub fn get_interned(string: &str) -> Option<InternedString> {
  unsafe {
    if STRING_INTERNER.is_none() {
      STRING_INTERNER = &mut StringInterner::default();
    }
    STRING_INTERNER.get(string)
  }
}

pub fn resolve_str(symbol: InternedString) -> &'static str {
  unsafe {
    if STRING_INTERNER.is_none() {
      STRING_INTERNER = &mut StringInterner::default();
    }
    STRING_INTERNER.resolve(symbol).unwrap()
  }
}

pub fn resolve_str_checked(symbol: InternedString) -> Option<&'static str> {
  unsafe {
    if STRING_INTERNER.is_none() {
      STRING_INTERNER = &mut StringInterner::default();
    }
    STRING_INTERNER.resolve(symbol)
  }
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
