/*!

String interning.

Two choices for resolution:

  1. Either we have `resolve(interned_string)` all over the codebase, or
  2. we restrict ourselves only to implementations in which an interned string dereferences to a `String`/`&str`.

*/

use string_cache::DefaultAtom;

pub type IString = DefaultAtom;

/// Interns a `String`
pub fn intern(text: String) -> IString {
  IString::from(text)
}

/// Interns a `&str`.
pub fn intern_str(text: &str) -> IString {
  IString::from(text)
}

#[inline(always)]
pub fn interned_static(text: &'static str) -> IString {
  intern_str(text)
}

/// Produces a `String` from `atom`.
pub fn resolve(atom: IString) -> String {
  atom.to_string()
}

