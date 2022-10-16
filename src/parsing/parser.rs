/*!
A parser owns the `OperatorTable` and parses the expression grammar, building a syntax tree along
the way. Because the `OperatorTable` drives the control flow of the parsing algorithm, the
`OperatorTable` and `Parser` are tightly coupled, with `OperatorTable` being a data structure
only and `Parser` holding all methods.

This design is an evolution of that described by Theodore Norvell, which itself is very similar to
the standard object oriented design and Pratt's original description. In the original design,
information about operators are stored in a table during runtime, and there are facilities for
loading, managing, and searching this table.

Norvell describes three tables: `LeftCommand`, `NullCommand`, and "Other." The "Command" suffix is
historical and refers to what command the parsing algorithm should take when it encounters a
particular token. `LeftCommand` holds data about operators that take a operand on their left.
Operators in `NullCommand` *start* expressions.

L (left) tokens:  Takes a left operand (binary ops, postfix ops).
                  L tokens are those directly consumed by the E procedure.
N (null) tokens:  No left operand (prefix ops and variables).
                  N tokens are those that are used to make the initial
                  choice in the P procedure and that are then consumed as
                  opposed to leading to an error being reported
O (other) tokens: All other tokens: ), ], etc.

See Theodore S. Norvell, "From Precedence Climbing to Pratt Parsing," 2016:
https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm

The present design differs in several ways:
  1. Operator data is stored in a single table.
  2. An operator may have any (or all) of L-, N-, and O-tokens. If an operator has no token of a
     category, the column for that category is set to NULL.
  3. Methods for parsing operators are moved out of operator (sub)classes and into the `Parser`
     with the rest of the parsing code. This not only consolidates the parsing code but also
     separates the concerns of describing data about an operator from the parsing algorithm.

*/
#![allow(dead_code)]


// Todo: Error handling is a mess. There is no consistency. Need to distinguish errors that happen _inside_ the
//       session and errors that happen when executing an otherwise valid session.

// Todo: The `Parser` struct holds very little state. Investigate the feasibility of not having a `Parser` struct at
//       all. Already most methods should be free functions. Alternatively, does it make more sense to hold more
//       state in `Parser` instead of passing state around? Issue is dependent on error handling design.

// Todo: Think about whether or not there is a better code factorization between `left_denotation` and
//       `null_denotation`, as they differ only in which operator types they match on.


use std::{
  rc::Rc,
  iter::Peekable
};
use lazy_static::lazy_static;

use crate::{
  context::{
    Context,
    InternedString
  },
  atom::{
    Atom,
  },
  logging::{
    log,
    Channel,
  },
  parsing::{
    lexer::Token,
    operator::{
      get_operator_table,
      Operator,
      OperatorTable
    },
    Lexer as Prelexer,
  }
};
use crate::parsing::operator::OperatorTables;

type Lexer<'t> = Peekable<Prelexer<'t>>;

// lazy_static!{
//   static ref OPERATOR_TABLES: OperatorTables = get_operator_table();
// }

static mut OPERATOR_TABLES: Option<OperatorTables> = None;


pub fn parse(input: &str) -> Result<Atom, ()>{
  let mut lexer: Lexer = Prelexer::new(input).peekable();

  // Bootstrap the parsing algorithm...
  match parse_expression(0, &mut lexer) {

    Err(token) => {
      log(Channel::Error, 1, format!("Aborted parse with unexpected: {:?}", &token).as_str());
      Err(())
    }

    any => any

  }
}


#[allow(non_snake_case)]
fn parse_expression(
  previous_binding_power: i32, // The binding power (precedence) of the parent expression that called us.
  lexer: &mut Lexer
)
  -> Result<Atom, Token> {

  // STEP 1: Parse null tokens, i.e. tokens that can begin an expression. Includes leaf tokens.

  let mut token = consume_token(lexer);
  log(Channel::Debug, 5, format!("Consuming token: {}", &token).as_str());

  // STEP 2: Convert the token to an expression and look up operator information. The `current_root` holds
  // the root of the expression we are currently parsing. The relevant operator for the token drives the
  // parsing algorithm. If the expression is a leaf, a generic `NullaryLeaf` operator record is returned.
  // The `NullaryLeaf` is distinguished by having a precedence of 0 and null associativity, as a leaf must.
  let (mut current_root, operator): (Atom, Operator) =
      { // Scope of null
        let null_table = unsafe {
          &OPERATOR_TABLES.unwrap().null
        };
        lookup_token(&token, null_table)?
      };
  log(
    Channel::Debug,
    5,
    format!("Parsed expression: {}, rbp={} (prev={})",
            &current_root,
            operator.right_binding_power(),
            previous_binding_power
    ).as_str()
  );


  // STEP 3: Parse the expressions that form the RHS of this null token.
  // In the case of a leaf, this is a no-op. We pass in the operator record to know
  // the highest binding power of the expression we are going to parse, and we pass
  // in the current root so that we can push expressions onto it as RHS children.
  // (A null operator with a fully parsed RHS is the "null denotation" of that operator.)
  null_denotation(&mut current_root, &operator, lexer)
      .map_err(|_| token.clone())?;

  // STEP 4: Parse left tokens, i.e. tokens that take an expression on their LHS, placing the `current_root`
  // in the LHS position of new left tokens as we go (so long as their precedence is high enough). The sub-steps of
  // step 4 mirror the previous steps we have already seen. We will number the substeps to coincide with the
  // similar previous step: STEP 4.1 <=> STEP 1, STEP 4.2 <=> STEP 2, STEP 4.3 <=> STEP 3…

  // At this point we have already parsed an expression to put in the LHS of something by completing
  // steps 1-3. As long as we keep seeing new l-tokens of equal or lower precedence than the current
  // precedence (the *previous* precedence is *higher*), we can keep building up the expression tree.
  //
  // It is possible, though, that the precedence of the l_token we find is too low to bind to `current_root`,
  // in which case the expression that will end up on the LHS of the l_token lives in an ancestor caller of this
  // iteration of `parse_expression`. (High precedence means binding at deeper call levels, high in the call stack.)

  loop {
    // STEP 4.1: Parse the left token, i.e. the token that takes an expression on its LHS.
    // Don't consume the token in case its binding power is out of bound. This is just a peek.
    token = match get_current_token(lexer) {
      Some(t) => t.clone(),
      None => {
        // There is no next token, so we are finished in this call.
        break;
      }
    };
    log(Channel::Debug, 5, format!("Peeking, found token: {}", &token).as_str());

    // STEP 4.2: Convert the token to an expression and look up operator information.
    // The operator provides it's left and right binding power.
    let (mut new_root, operator): (Atom, Operator) =
        { // Scope of left_table
          let left_table = unsafe {
            &OPERATOR_TABLES.unwrap().left
          };
          lookup_token(&token, left_table)?
        };
    // (If we don't find an l-expression, it's an error, because that means we have an expression on the LHS of
    // something that isn't allowed to have something on its LHS!)

    // The `previous_binding_power` is the precedence of the parent expression that called this iteration of
    // `parse_expression` in the first place. If the new operator have a precedence even lower than the parent
    // expression's, then the parent expression itself deserves to be on its LHS, not the `current_root`, which is
    // merely a subexpression of the parent. If we hit this case, we are done this iteration of `parse_expression`
    // and return the expression we built to the caller.
    if previous_binding_power > operator.left_binding_power() {
      log(
        Channel::Debug,
        5,
        format!(
          "Found expression but binding power out of range: p={}, exp.lbp={}",
          &previous_binding_power,
          operator.left_binding_power()
        ).as_str()
      );
      // Break out of the loop to stop parsing l-expressions and return the expression we built.
      break;
    }

    // On the other hand, if the l-operator binds at least as tight as `previous_binding_power`, we commit to
    // parsing it and consume the token.
    consume_token(lexer);
    log(
      Channel::Debug,
      5,
      format!(
        "Found expression: {}, rbp={}. Consuming peeked token.",
        &new_root,
        operator.right_binding_power()
      ).as_str()
    );
    push_child(&mut new_root, current_root).map_err(|_| token)?;

    /*
    Question: How do we know the null expression we parsed at the top of this function is the LHS child of the left
              token we just found, as opposed to the left token belonging as a RHS child of the null expression?
    Answer: Because if the left token was supposed to be a RHS child of the null expression, it would have already
            been parsed in the `null_denotation` method, and we wouldn't be seeing it now.
    */

    log(
      Channel::Debug,
      1,
      format!(
        "Now tying to parse RHS (in LeD). lhs={} root={} op.rbp={}",
        &current_root,
        new_root,
        operator.right_binding_power()
      ).as_str()
    );

    // STEP 4.3: Parse the expressions that form the RHS of this left token.
    // The `current_root` will become the LHS, as we push both `current_root` and the RHS onto `new_root` as
    // children. The `new_root` then becomes `current_root`, and we repeat the process of parsing left tokens.
    left_denotation(&mut new_root, &operator, lexer)
        .map_err(|_| token)? ;

    /*
    Question: What is the point of having both `null_denotation` and `left_denotation` methods instead of just one
              single method?
    Answer: Because by separating them, we can have the same token be interpreted as two distinct operators
            depending on whether the token is operating as a null token or as a left token. This is useful in the
            very common case of "-" being used both as unary minus (a null operator) and as subtraction (a left
            operator), for example. This feature makes Pratt parsing especially powerful.
    */

    current_root = new_root;
  }

  // Done!
  return Ok(current_root);

}


/// This method parses the expressions that will form the RHS of the null `expression`, pushing them onto
/// `expression` as children. The `operator` holds information about `expression`.
fn null_denotation(expression: &mut Atom, operator: &Operator, lexer: &mut Lexer)
                   -> Result<(), ()> {
  // For tokens that just don't take anything on their RHS (because they're leaf nodes), do nothing.
  // This isn't strictly necessary, because the logic below would eventually return anyway.
  // todo: It isn't clear if these two conditions are ever not simultaneously satisfied.
  if operator.n_token().is_none() || operator.right_binding_power() <= 0{
    return Ok(());
  }

  /// We now parse a brand-new expression, which is allowed to be made up of an operator with precedence no greater
  /// than our own right binding power. (If it were greater, we would be parsed as the LHS of that operator by that
  /// operator rather than us parsing it as our own RHS.)
  match parse_expression(operator.right_binding_power(), lexer) {
    // The nonempty case
    Ok(child_expression) => {
      push_child(expression, child_expression)?;
    }

    // The empty case. This happens when there is no expression with sufficiently low precedence to form our RHS.
    // This is not necessarily an error if our RHS can optionally be empty, as in the case of an empty list, `{}`.
    Err(_token) => {

      match operator {
        // In our implementation, Matchfix is the only case where the RHS of a null token can be empty.
        // We'll keep this as a match statement just to emphasize that there could be several operator kinds.
        Operator::Matchfix{ .. } => {
          // There's nothing to push onto `expression`, so this is a no-op.
          /* pass */
        }

        _ => {
          // No other null operator can have an empty RHS, so this is an error.
          log(
            Channel::Error,
            1,
            format!("Expected a new expression but found none. The RHS of the null operator {} is not allowed to be \
              empty.", resolve_str(operator.name())).as_str()
          );

          return Err(());
        }// end non-matchfix match branch
      }// end match operator

    } // end empty RHS case.
  } // end attempt to parse RHS expression.

  // Some null operators have a token following their RHS. If that token is optional, its presence signals that we
  // need to parse a third expression to the right of the optional token. (We have no operators of the last type in
  // this implementation.) Example: `(exp)` with required o-token `)`.
  match operator {
    // All *null* operators having an o-token. In our implementation this is only Matchfix, but we keep the match
    // expression to emphasize that there could be many.
    Operator::Matchfix{ o_token, .. } => {
      // The o-token is not optional.
      expect(*o_token, lexer)?
    }

    /*
    It is possible to have an operator of the form `n_token exp1 o_token exp2`. The second expression would be
    parsed after the o-token. One could have a separate match-branch in this match expression for such operators.
    Alternatively, one could include the "arity" in the `Operator` variant and parse the second expression in an
    if-block that checks for `operator.arity == 2`.

    Mathematica's Span operator in it's "null" form, `;;endexp;;stepexp`,  is an example of such a null operator.
    */

    _ => {
      // If this null operator has no o-token, this is a no-op.
      /* pass */
    }
  }

  /*
  Other null operator forms that we are not accounting for in this code are possible:

    * A "chaining" (variadic) null operator: `$ exp1 : exp2 : exp3 : … : expn`. In this case you would parse
      additional expressions in a loop with calls to `parse_expression(operator.right_binding_power(), lexer)`.
    * A null operator with a final token:  `<|x|y|>` (Dirac's Bra-Ket notation)
    * An n-ary matchfix operator: `<x:y:z>` (ternary matchfix). In the general case, you would loop precisely n times.
  */


  Ok(())
}


/// Consumes the next token, giving success if the token is an operator matching `expected_token` and failure
/// otherwise.
fn expect(expected_token: InternedString, lexer: &mut Lexer) -> Result<(), ()> {
  match consume_token(lexer) {

    Token::Operator(t) if t==expected_token => {
      Ok(())
    }

    anything_else => {
      log(Channel::Error, 1, format!("Expected: {}, Found: {}", resolve_str(expected_token), anything_else).as_str());
      Err(())
    }

  }
}


/// Peeks at the next token. If the next token is an operator equal to `expected_token`, it is consumed, and we
/// return true. Otherwise, we return false.
fn optional_expect(expected_token: InternedString, lexer: &mut Lexer) -> bool {
  match lexer.peek() {

    Some(Token::Operator(token)) if *token == expected_token => {
      // Consume what we peeked.
      lexer.next();
      true
    }

    _ => {
      false
    }

  }
}


/// This method parses the expressions that will form the RHS of the left `expression`, pushing them onto
/// `expression` as children. The `operator` holds information about `expression`.
fn left_denotation(root: &mut Atom, operator: &Operator, lexer: &mut Lexer)
                   -> Result<(), ()>{

  // There is a special case of the Ternary operator whose right binding power applies only to its RHS
  // argument, not to its middle argument, which is treated as if it were parenthesized. For these operators,
  // we first parse the parenthesized middle expression before proceeding to the RHS. As elsewhere,
  // we use a match statement to emphasize that there could be several operators with this property.
  match operator {
    Operator::Ternary{ o_token, .. } => {
      // Note the `previous_binding_power` is 0.
      match parse_expression(0, lexer) {

        // The nonempty case. This is the only case we have in this implementation.
        Ok(middle_expression) => {
          push_child(root, middle_expression)?;
          // Now consume the o_token and proceed as if `Operator::Ternary` were a binary operator.
          expect(*o_token, lexer)?;
        }

        // The empty case. For our implementation this is always an error, but nothing says you can't have ternary
        // operators with empty middle argument.
        Err(_) => {
          log(
            Channel::Error,
            1,
            format!(
              "Expected a new expression but found none. The middle of the left operator {} is not allowed to be \
                empty.",
              resolve_str(operator.name())
            ).as_str()
          );

          return Err(())
        }
      }
    }

    _ => {
      // No other left operator parenthesizes its middle argument.
      /* pass */
    }
  }

  // For tokens that just don't take anything on their RHS (because they're postfix operators), do nothing.
  // This isn't strictly necessary, because the logic below would eventually return anyway.
  if operator.right_binding_power() <= 0 {
    return Ok(());
  }

  /// We now parse a brand-new expression, which is allowed to be made up of an operator with precedence no greater
  /// than our own right binding power. (If it were greater, we would be parsed as the LHS of that operator by that
  /// operator rather than us parsing it as our own RHS.)
  match parse_expression(operator.right_binding_power(), lexer) {

    // The nonempty case
    Ok(rhs_expression) => {
      push_child(root, rhs_expression)?
    }

    // The empty case. This happens when there is no expression with sufficiently low precedence to form our RHS.
    // This is not necessarily an error if our RHS can optionally be empty, as in the case of an empty function call,
    // call `f[]`.
    Err(_token) => {

      match operator {
        // In our implementation, Indexing and Postfix are the only cases where the RHS of a left token can be empty.
        // (Note that `Operator::Postfix` has rbp=-1, which we already returned on. But this is a technicality.)
        // We'll keep this as a match statement just to emphasize that there could be several operator kinds.
        | Operator::Indexing{ .. }
        | Operator::Postfix{ .. } => {
          // There's nothing to push onto `expression`, so this is a no-op.
          /* pass */
        }

        _ => {
          // No other left operator can have an empty RHS, so this is an error.
          log(
            Channel::Error,
            1,
            format!("Expected a new expression but found none. The RHS of the left operator {} is not allowed to be \
              empty.", resolve_str(operator.name())).as_str()
          );

          return Err(())
        }// end all other match branch
      }// end match operator

    } // end empty RHS case.
  };

  // Some left operators have a token following their RHS. If that token is optional, its presence signals that we
  // need to parse a third expression to the right of the optional token. Note that we have made
  // `Operator::Ternary` a special case and have already parsed its RHS in the previous step.
  match operator {
    // All *left* operators having a *required* o-token but no third child expression
    | Operator::Indexing{ o_token, .. }
    => {
      // The o-token is not optional.
      expect(*o_token, lexer)?
    }

    // All *left* operators having an *optional* o-token indicating an additional expression needs to be parsed.
    Operator::OptionalTernary{ o_token, ..} => {
      // First we peek to check for the optional token, consuming it if it is present.
      if optional_expect(*o_token, lexer) {
        // Since the o-token is present, we require another expression on the RHS.
        match parse_expression(operator.right_binding_power(), lexer) {

          Ok(far_rhs) => {
            push_child(root, far_rhs)?;
          }

          Err(_token) => {
            log(
              Channel::Error,
              1,
              format!("Expected a new expression but found none. The RHS of the null operator {} is not allowed to be \
              empty.", resolve_str(operator.name())).as_str()
            );

            return Err(());
          }

        } // end match on parse_expression(..)
      } // end if optional o-token is present
    } // end OptionalTernary match branch

    _ => {
      // If this left operator has no o-token, this is a no-op.
      /* pass */
    }
  } // end match on operator

  // Other left operator forms that we are not accounting for in this code are possible.

  Ok(())
}




/// Fetches the next token and consumes it. This is the default `next` behavior. The alternative behavior is
/// `peek`, which does not consume the token from the iterator.
fn consume_token(lexer: &mut Lexer) -> Token {
  // This *should* be infallible, because errors should produce error tokens.
  lexer.next().unwrap()
}

/// Fetches the next token but does not consume it. A call to either `get_current_token` or `consume_next_token`
/// will return the same token again.
fn get_current_token<'a>(lexer: &'a mut Lexer) -> Option<&'a Token> {
  lexer.peek()
}

#[allow(unused_parens)]
/// Push `child` onto `parent`, applying fix-ups for "Construct", "Sequence", and "Parentheses".
fn push_child(parent: &mut Atom, child: Atom) -> Result<(), ()> {
  // Destructure parent…
  return match parent {
    Atom::SExpression(mut parents_children) => {
      if Some(&Atom::Symbol(interned_static("Construct"))) == parents_children.first(){
        // To convert `Construct` to the function it's constructing, just remove the head!
        // In this case, `child` is the head of the function being constructed, so we just
        // overwrite the `Construct` head with it.
        parents_children[0] = child;
        return Ok(());
      }

      // Destructure the child
      if let Atom::SExpression(mut children) = child {
        if Some(&Atom::Symbol(interned_static("Sequence"))) == children.first(){
          // Splice in the sequence's children, skipping the head.
          parents_children.extend(children[1..].iter().cloned());
          return Ok(());
        }
      }
      // If we get here it's just a normal child of a normal function.
      parents_children.push(child.clone());
      Ok(())
    }

    _ => {
      // Tried to push a child onto an expression that cannot have children.
      Err(())
    }
  }

}


/// Finds the given `Token` in the given `OperatorTable`, converts it to an `Atom`, and returns the `Atom` and
/// `OperatorRecord`.
fn lookup_token(token: &Token, table: &OperatorTable) -> Result<(Atom, Operator), Token> {
  match &token {

    Token::Operator(operator) => {
      match table.look_up(*operator) {

        Some(op) => {
          // Operators become function calls: `a+b` -> `Add[a, b]`.
          let expression = Atom::function_with_symbolic_head(op.name());
          Ok((expression, *op))
        }

        None => {
          // An `OpToken` should never contain a leaf token, only things that are in the operator table.
          log(Channel::Error, 1, format!("Unexpected: {:?}", &operator).as_str());
          return Err(token.clone());
        }

      }
    } // end match OpToken

    Token::Error(msg) => {
      // An `OpToken` should never contain a leaf token, only things that are in the operator table.
      log(Channel::Error, 1, msg.as_str());
      return Err(token.clone());
    }

    any_other_token => {
      // This is the leaf node case.
      let exp = (*any_other_token).try_into_atom().unwrap();
      log(Channel::Debug, 5, format!("Found leaf node: {}", exp).as_str());
      let op = Operator::nullary_leaf();
      Ok((exp, op))
    }

  } // end match token
}


#[cfg(test)]
mod tests {
  #[allow(unused_imports)]
  use crate::logging::set_verbosity;
  use super::*;

  #[test]
  fn function_test() {
    let text = "3.8*f[1+x, y]";
    // let text = "f[1+3.8]";
    // set_verbosity(5);

    match parse(text) {

      Ok(e) => {
        assert_eq!("Times[3.8, f[Plus[1, x], y]]", e.to_string().as_str());
        println!("Success: {}", e);
      },

      Err(_) => assert!(false)

    };
  }

  #[test]
  fn precedence_test() {
    let text = "3.8*x^2 + 2*x^f[a+b, c*d, e]";

    match parse(text) {

      Ok(e) => {
        assert_eq!(
          "Plus[Times[3.8, Power[x, 2]], Times[2, Power[x, f[Plus[a, b], Times[c, d], e]]]]",
          e.to_string().as_str()
        );
        println!("Success: {}", e);
      },

      Err(_) => assert!(false)

    };
  }

  #[test]
  fn parentheses_test() {
    let text = "2*(3+a)";

    match parse(text) {

      Ok(e) => {
        assert_eq!(
          "Times[2, Plus[3, a]]",
          e.to_string().as_str()
        );
        println!("Success: {}", e);
      },

      Err(_) => assert!(false)

    };
  }


  #[test]
  fn unary_minus_test() {
    let text = "2 - 2 + -3 * -4 * f[-5-6] - -7";
    let expected = "Subtract[2, Plus[2, Minus[Times[3, Minus[Subtract[Times[4, f[Minus[Subtract[5, 6]]]], \
    Minus[7]]]]]]]";
    // let text = "2  -3";
    set_verbosity(5);

    match parse(text) {

      Ok(e) => {
        assert_eq!(expected, e.to_string().as_str());
        println!("Success: {}", e);
      },

      Err(_) => assert!(false)

    };
  }


  #[test]
  /// The parser fixes up artifacts of the parsing process, e.g. "evaluating" `Construct`s, eliding slices, etc.
  fn fixup_test() {
    let text = "a[b, c, d[e, f], g, h]";
    // set_verbosity(5);

    match parse(text) {

      Ok(e) => {
        assert_eq!(text, e.to_string());
        println!("Success: {}", e);
      },

      Err(_) => assert!(false)

    };
  }

}
