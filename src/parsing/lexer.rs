/*!

Minimalist lexer.

*/

use std::{
  fmt::{Display, Formatter},
  cmp::min
};

use lazy_static::lazy_static;
use aho_corasick::{AhoCorasickBuilder, AhoCorasick, MatchKind, StartKind};
use regex::{
  Regex,
  Match as RegexMatch
};

use crate::{
  ast::Atom,
  interned_string::{
    intern_str,
    IString,
  }
};

/*
To have dynamic lexing of operators, the lexer needs facilities for adding and removing operators.
*/

// Keep in sync with numbering in `REGEXES`.
const STRING_LITERAL_IDX: usize = 0;
const REAL_IDX          : usize = 1;
const INTEGER_IDX       : usize = 2;
const IDENTIFIER_IDX    : usize = 3;
const EOL_COMMENT_IDX   : usize = 4;
const WHITESPACE_IDX    : usize = 5;

lazy_static! {
  pub static ref REGEXES: [Regex; 6] = [
    Regex::new(r#""(?:[^"]|\\")*""#).unwrap(),    //  0. StringLiteral  ToDo: Make strings more sophisticated.
    Regex::new(r"[0-9]+\.[0-9]+").unwrap(),       //  1. Real
    Regex::new(r"[0-9]+").unwrap(),               //  2. Integer
    Regex::new(r"[a-zA-Z][a-zA-Z0-9]*").unwrap(), //  3. Identifiers,
    Regex::new(r"//.*\n?").unwrap(),              //  4. EOL Comments
    Regex::new(r"[ \t\n\f]+").unwrap(),           //  5. Whitespace
  ];
}

// The Lexer supplies `Token` variants to the client code.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
  Integer(IString),
  Operator(IString),
  Real(IString),
  String(IString),
  Symbol(IString),
  Error(String),
}

impl Display for Token {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Token::Integer(v) => {
        write!(f, "Integer({})", v)
      }
      Token::Operator(v) => {
        write!(f, "Operator({})", v)
      }
      Token::Real(v) => {
        write!(f, "Real({})", v)
      }
      Token::String(v) => {
        write!(f, "String(\"{}\")", v)
      }
      Token::Symbol(v) => {
        write!(f, "Symbol({})", v)
      }
      Token::Error(v) => {
        write!(f, "Error({})", v)
      }
    }
  }
}

impl Token {

  /// Leaf tokens can be trivially converted to `Atoms`.
  pub fn try_into_atom(self) -> Result<Atom, ()>{
    match self {
      Token::Operator(_)
      | Token::Error(_) => { Err(())}

      // Literals and symbols are the leaf tokens.
      Token::Integer(n) => Ok(Atom::Integer(n)),
      Token::Real(r)    => Ok(Atom::Real(r)),
      Token::String(s)  => Ok(Atom::String(s)),
      Token::Symbol(s)  => Ok(Atom::Symbol(s)),
    }
  }
}

pub struct Lexer<'t>  {
  token_matcher: AhoCorasick,
  /// A cursor pointing to the start of the next token to be tokenized.
  start: usize,
  text : &'t str,
  next_token: Option<Token>
}

impl<'t> Lexer<'t> {

  pub fn new(text: &'t str, tokens: Vec<String>) -> Self {
    // ToDo: Cache `token_matcher` (make static)
    let token_matcher = AhoCorasickBuilder::new()
        // .start_kind(StartKind::Anchored)
        // .auto_configure(&tokens)
        .match_kind(MatchKind::LeftmostLongest)
        .build(tokens)
        .unwrap(); // Should be infallible with these parameters.

    Lexer {
      token_matcher,
      start: 0,
      text,
      next_token: None
    }
  }

  pub fn peek(&mut self) -> Option<Token> {
    if self.next_token.is_none() {
      self.next_token = self.next();
    }
    return self.next_token.clone()
  }

  // This method is a copy+paste, because `aho_corasick::Match` is a different type from `regex::Match`.
  // todo: Factor out this common code the right way.
  fn get_operator_match(&mut self) -> Result<IString, Token> {
    match self.token_matcher.try_find(&self.text[self.start..]) {

      Ok(Some(token)) => {
        if token.start() != 0 {
          // Token must start at beginning of the string.
          let error_start: usize = self.start;
          self.start += token.start(); // Skip over this unrecognized token.
          // Don't print text of unbounded length.
          let error_end: usize = min(error_start + 20, error_start + token.end());
          Err(Token::Error(format!("Unexpected token: {}", &self.text[error_start..error_end])))

        } else {
          let start = self.start;
          let end = self.start + token.end();
          self.start = end;
          Ok(intern_str(
            &self.text[start..end]
          ))
        }
      }

      Ok(_) => {
        Err(Token::Error("Unrecognized operator.".to_string()))
      }

      Err(e) => Err(
        Token::Error(format!("No matching tokens found: {}", e))
      )

    }
  }

  /// Processes matches resulting from `regex.find(&self.text[self.start..])`, updating `self.start` appropriately.
  fn get_match(&mut self, found: Option<RegexMatch>) -> Result<IString, Token> {

    match found {

      Some(token) => {
        if token.start() != 0 {
          // Token must start at beginning of the string.
          let error_start: usize = self.start;
          self.start += token.start(); // Skip over this unrecognized token.
          // Don't print text of unbounded length.
          let error_end: usize = min(error_start + 20, error_start + token.end());
          Err(Token::Error(format!("Unexpected token: {}", &self.text[error_start..error_end])))

        } else {
          let start = self.start;
          let end = self.start + token.end();
          self.start = end;
          Ok(intern_str(
            &self.text[start..end]
          ))
        }
      } // end found match

      None=>{
        // Error!
        Err(Token::Error("No matching tokens found.".to_string()))
      } // end found no match

    } // end match on found token match

  }

}

impl<'t> Iterator for Lexer<'t> {
  type Item = Token;

  // todo: Decide if we want to automatically convert leaves to Atoms, in which case we only need three `Token`
  //       variants: Error(String), Operator(IString), Leaf(Atom). Leaving as a token allows parsing based on
  //       leaf type.
  fn next(&mut self) -> Option<Self::Item> {
    if let Some(t) = self.next_token.take() {
      return Some(t);
    }

    // Eat whitespace.
    let stripped = self.text[self.start..].trim_start();
    self.start = self.text.len() - stripped.len();

    // Eat EOL comments.
    _ = self.get_match( REGEXES[EOL_COMMENT_IDX].find(&self.text[self.start..]) );

    // Empty text
    if self.start == self.text.len() {
      return None;
    }


    match self.text[self.start..].as_bytes()[0] {
      b'"' => {
        // Lex string literal.
        match
          self.get_match(REGEXES[STRING_LITERAL_IDX].find(&self.text[self.start..])){
          Ok(interned_string) => {
            Some(Token::String(interned_string))
          }
          Err(t) => Some(t)
        }

      }

      c if char::is_ascii_digit(&(c as char)) => {
        // Lex number literal. Note that reals must begin with a digit, not `.`.
        match
          self.get_match(REGEXES[REAL_IDX].find(&self.text[self.start..])) {
          Ok(token) => {
            Some(Token::Real(token))
          },
          Err(_) => {
            Some(
              Token::Integer(
                self.get_match(REGEXES[INTEGER_IDX].find(&self.text[self.start..])).unwrap()
              )
            )
          }
        }
      }

      c if char::is_alphabetic(c as char) => {
        // Lex identifier.
        match
          self.get_match(REGEXES[IDENTIFIER_IDX].find(&self.text[self.start..]))
        {
          Ok(interned_string) => {
            Some(Token::Symbol(interned_string))
          }
          Err(t) => Some(t)
        }
      }

      _ => {
        // Lex operator
        match self.get_operator_match() {
          Ok(interned_string) => Some(Token::Operator(interned_string)),
          Err(t) => Some(t)
        }
      } // end if non-regex token

    } // end match on first character
  }

}


// See `Parser` for tests.

// #[cfg(test)]
// mod tests {
//   #[test]
//   fn it_works() {
//     assert_eq!(2 + 2, 4);
//   }
// }
