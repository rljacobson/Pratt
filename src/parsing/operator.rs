/*!

An operator is a syntactic component of an expression grammar that may take arguments. The
`Operator` struct holds syntactic data about the operator, which is used by the generic Pratt
parsing algorithm.

A table of operators will hold the operator database for all the operators in the expression
grammar. The parsing algorithm will look up a given operator using the operator's token (sigil).
Thus, the operator table is a `HashMap` from `String` to `Operator`.

*/
#![allow(dead_code)]

use std::collections::{HashMap, HashSet};
use csv;

use crate::interned_string::{intern_str, interned_static, IString};
#[allow(unused_imports)]
use tiny_logger::{
  Channel,
  log,
  set_verbosity
};

pub struct OperatorTables{
  pub(crate) left: OperatorTable,
  pub(crate) null: OperatorTable
}

/// An operator has a set of properties that determine how it is parsed. Other properties like commutativity that do
/// not affect how an expression is parsed are not associated with the operator but rather with the function the
/// operator is interpreted as.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operator {
  /// A placeholder for a leaf, such as a literal or symbol.
  NullaryLeaf{
    name: IString,
    precedence: i32,
    associativity: Associativity
  },

  /// Binary parenthesizing: `a[[i]]`, `f[a, b, c, …]`
  /// Use for indexing into arrays or function calls.
  /// Note: In this implementation, the head is required, but the parenthesized part is variadic, leaving validation
  /// of arguments to be done by the resulting function expression it parses into.
  Indexing{
    name: IString,
    precedence: i32,
    associativity: Associativity,
    l_token: IString,
    o_token: IString
  },

  /// The most common type: `a+b`, `xy`
  BinaryInfix{
    name: IString,
    precedence: i32,
    /// If the associativity is "Full", the operator is chaining.
    /// Chaining means automatically flattening, e.g. `a + b + c` -> `Plus[a, b, c]` instead of `Plus[a, Plus[b, c]]`.
    associativity: Associativity,
    l_token: IString
  },

  /// Unary minus or boolean NOT.
  Prefix{
    name: IString,
    precedence: i32,
    n_token: IString
  },

  /// Postfix: `4!`
  Postfix{
    name: IString,
    precedence: i32,
    l_token: IString
  },

  /// Operators with a parenthesized middle argument: `a?b:c`.
  /// Note: In this implementation, we allow the middle argument to be empty.
  Ternary{
    name: IString,
    precedence: i32,
    associativity: Associativity,
    l_token: IString,
    o_token: IString
  },

  /// Operators with an optional "tail": `a:=b/;c`, where the `/;c` can optionally be left off, `a:=b`.
  /// In this implementation, the `b` expression cannot be empty.
  OptionalTernary{
    name: IString,
    precedence: i32,
    associativity: Associativity,
    l_token: IString,
    /// The token present in the optional form.
    o_token: IString
  },

  /// Parenthesizing operators: `(exp)`, `| -42 |`, `{a, c, b, …}`
  /// This implementation is variadic, leaving validation of arguments to be done by the resulting function expression.
  /// An alternative implementation could have an arity attribute or an arity range attribute.
  Matchfix{
    name: IString,
    precedence: i32, // ignored
    n_token: IString,
    o_token: IString,
  },

  /*
  // Parses the unusual case of the range operator, which can be any of `a;;b;;c`, `;;b;;c`, `;; ;;c`, `;;b;;`, etc.
  ZeroOrOneTernary{
    name: IString,
    precedence: i32,
    // In our only use case, the same token `;;` plays the role of Left, Null, and Other token.
    l_token: IString,
    n_token: IString,
    o_token: IString,
  }
*/
  /*
  Other types of operators not included here are possible. For example:
    * Nullary: leaves, e.g. `Null`, `ø`. We choose to not consider these operators.
    * Quaternary: `a<|b|c|>d`
    * MatchfixBinary: `<|x|y|>` (Dirac's Bra-Ket notation)
    * MatchfixTernary: `<x:y:z>`
  */
}

#[derive(Clone, Debug,  Default)]
pub struct OperatorTable {
  map: HashMap < IString, Operator >,
}

impl OperatorTable {

  pub fn new() -> OperatorTable {
    OperatorTable::default()
  }


  /// If `token` has a record in the operator table, return it. Otherwise, return `None`.
  pub fn look_up(&self, token: IString) -> Option<&Operator> {
    self.map.get(&token)
  }

  pub fn insert(&mut self, name: IString, operator: Operator) {
    self.map.insert(name, operator);
  }

}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Associativity {
  Null,  // Things like constants or identifiers that have no affix or associativity. Also,
         // matchfix operators.
  Non,   // The operator cannot be adjacent to another operator of the same precedence.
  Right, // E.g. 2^3^4 == 2^(3^4) != (2^3)^4
  Left,  // E.g. 3-4-5 == (3-4)-5 != 3 - (4-5)
  Full   // Adjacent operators collapse into a single variadic function,
         // e.g. 1 + 2 + 3 + 4 == Plus(1, 2, 3, 4)
}

impl Associativity{
  pub fn from_str(s: &str) -> Associativity {
    match s {

      "R" => Associativity::Right,

      "L" => Associativity::Left,

      "F" => Associativity::Full,

      "N" => Associativity::Non,

      ""  => Associativity::Null,

      anything => {
        eprint!("Unreachable associativity: {}", anything);
        unreachable!()
      }
    }
  }
}

impl Operator {

  pub fn nullary_leaf() -> Self {
    Operator::NullaryLeaf {
      name         : interned_static("None"),
      precedence   : 0,
      associativity: Associativity::Null
    }
  }

  pub fn name(&self) -> IString{
    match self {
      | Operator::NullaryLeaf     { name, .. }
      | Operator::Indexing        { name, .. }
      | Operator::BinaryInfix     { name, .. }
      | Operator::Prefix          { name, .. }
      | Operator::Postfix         { name, .. }
      | Operator::Ternary         { name, .. }
      | Operator::OptionalTernary { name, .. }
      | Operator::Matchfix        { name, .. } => {
        name.clone()
      }
    }
  }

  pub fn left_binding_power(&self) -> i32 {
    match self {
      | Operator::NullaryLeaf     { precedence, .. }
      | Operator::Indexing        { precedence, .. }
      | Operator::BinaryInfix     { precedence, .. }
      | Operator::Prefix          { precedence, .. }
      | Operator::Postfix         { precedence, .. }
      | Operator::Ternary         { precedence, .. }
      | Operator::OptionalTernary { precedence, .. }
      | Operator::Matchfix        { precedence, .. } => {
        *precedence
      }
    }
  }

  /// Right binding power is computed from precedence and associativity.
  pub fn right_binding_power(&self) -> i32 {
    match self.associativity() {

      | Associativity::Left
      | Associativity::Non => self.left_binding_power() + 1,

      Associativity::Right => self.left_binding_power(),

      // Fully associative means "chaining".
      Associativity::Full  => self.left_binding_power() - 1,

      Associativity::Null  => -1 // Technically, Matchfix is N/A.

    }

  }

  /// This property would be recorded for each operator type if the operator types themselves are to be read in from
  /// a database dynamically.
  fn associativity(&self) -> Associativity {
    match self {

      | Operator::BinaryInfix     { associativity, .. }
      | Operator::Ternary         { associativity, .. }
      | Operator::OptionalTernary { associativity, .. }
          => *associativity,

      // Even though associativity is meaningless for prefix operators, setting their associativity to right causes
      // their right binding power to be their precedence as required.
      Operator::Prefix { .. } => Associativity::Right,

        // Setting Postfix's associativity to null makes its right binding power -1, preventing it from taking an
        // expression on the RHS.
      | Operator::Postfix { .. }
      // Indexing acts like an infix operator on the left and a postfix operator on the right.
      | Operator::Indexing { .. }
      | Operator::Matchfix { .. }
      | Operator::NullaryLeaf { .. }  => Associativity::Null,
    }
  }

  pub fn l_token(&self) -> Option<IString> {
    match self {
      | Operator::Indexing        { l_token, .. }
      | Operator::BinaryInfix     { l_token, .. }
      | Operator::Ternary         { l_token, .. }
      | Operator::Postfix         { l_token, .. }
      | Operator::OptionalTernary { l_token, .. }
        => Some(l_token.clone()),

      | Operator::Prefix { .. }
      | Operator::Matchfix { .. }
      | Operator::NullaryLeaf { .. }
        => None
    }
  }

  pub fn n_token(&self) -> Option<IString> {
    match self {
      | Operator::Prefix   { n_token, .. }
      | Operator::Matchfix { n_token, .. }
        => Some(n_token.clone()),

      | Operator::NullaryLeaf { .. }
      | Operator::Indexing { .. }
      | Operator::BinaryInfix { .. }
      | Operator::Postfix { .. }
      | Operator::Ternary { .. }
      | Operator::OptionalTernary { .. }
        => None
    }
  }

  pub fn o_token(&self) -> Option<IString> {
    match self {
      | Operator::Indexing        { o_token, .. }
      | Operator::Ternary         { o_token, .. }
      | Operator::OptionalTernary { o_token, .. }
      | Operator::Matchfix        { o_token, .. }
        => Some(o_token.clone()),

      | Operator::NullaryLeaf { .. }
      | Operator::BinaryInfix { .. }
      | Operator::Prefix { .. }
      | Operator::Postfix { .. }
        => None
    }
  }

  // The parse-time functionality of `Operator` lives in the `impl Parser`.
}

/// Reads operators from the given file.
///
/// Note: Only the `Indexing`, `MatchFix`, `BinaryInfix`, and `Prefix` operator variants are
/// implemented for reading from a CSV file.
pub(crate) fn read_operators(path: &str, separator: u8) -> Result<Vec<Operator>, csv::Error> {
  let mut operators = Vec::new();

  let mut reader
      = csv::ReaderBuilder::new().delimiter(separator)
                                 .has_headers(true)
                                 .trim(csv::Trim::All)
                                 .from_path(path)?;

  for record in reader.records() {
    let record = record?;
    let name = intern_str(&record[0]);
    let precedence = str::parse::<i32>(&record[1]).unwrap();

    let operator = // The result of the match block below
      match &record[6] {

        "I" => {
          // Infix
          if &record[4] != "" {
            Operator::Indexing {
              name,
              precedence,
              associativity: Associativity::from_str(&record[5]),
              l_token: intern_str(&record[2]),
              o_token: intern_str(&record[4]),
            }
          } else {
            Operator::BinaryInfix {
              name,
              precedence,
              associativity: Associativity::from_str(&record[5]),
              l_token: intern_str(&record[2])
            }
          }
        }

        "P" => {
          // Prefix
          Operator::Prefix {
            name,
            precedence,
            n_token: intern_str(&record[3]),
          }
        }

        "M" => {
          // Matchfix
          Operator::Matchfix {
            name,
            precedence,
            n_token: intern_str(&record[3]),
            o_token: intern_str(&record[4]),
          }
        }

        other => {
          unimplemented!("The affix `{}` is not implemented.", other);
        }

      };

    operators.push(operator);
  }

  Ok(operators)
}

/// Read in a list of operators and their syntactic properties and generate a `left_operator_table` and
/// `null_operator_table` for use in parsing.
pub(crate) fn get_operator_tables_from_file(file_path: &str) -> (OperatorTables, Vec<String>) {

  let operators = match read_operators(file_path, b'|') {
    Ok(ops) => ops,
    Err(e) => panic!("Couldn't read from operator file. {}", e)
  };

  let mut l_command_table = OperatorTable::new();
  let mut n_command_table = OperatorTable::new();
  let mut tokens = HashSet::new();

  for op in operators.iter(){
    if let Some(token) = op.n_token() {
      tokens.insert(token.clone());
      n_command_table.insert(op.n_token().unwrap(), op.clone());
    }
    if let Some(token) = op.l_token() {
      tokens.insert(token.clone());
      l_command_table.insert(op.l_token().unwrap(), op.clone());
    }
    if let Some(token) = op.o_token() {
      tokens.insert(token.clone());
    }
  }

  let tokens: Vec<String> = tokens.into_iter().map(|s| s.to_string()).collect();
  // println!("TOKENS:");
  // for t in &tokens {
  //   println!("{}", t);
  // }

  // for op in operators.iter().filter(|op| op.n_token().is_some()) {
  //   n_command_table.insert(op.n_token().unwrap(), *op);
  // }
  // for op in operators.iter().filter(|op| op.l_token().is_some()) {
  //   l_command_table.insert(op.l_token().unwrap(), *op);
  // }
  (
    OperatorTables{
      left: l_command_table,
      null: n_command_table
    },
    tokens
  )
}
