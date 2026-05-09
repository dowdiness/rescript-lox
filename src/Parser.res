// Recursive-descent parser for the Chapter 6 expression grammar:
//
//   expression → equality
//   equality   → comparison ( ( "!=" | "==" ) comparison )*
//   comparison → term       ( ( ">"  | ">=" | "<" | "<=" ) term )*
//   term       → factor     ( ( "-"  | "+"  ) factor )*
//   factor     → unary      ( ( "/"  | "*"  ) unary  )*
//   unary      → ( "!" | "-" ) unary | primary
//   primary    → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
//
// Threaded-tokens style: each rule takes a list of remaining tokens and returns
// either (parsedNode, restOfTokens) or a parseError. No mutable cursor.

type parseError = {message: string, line: int}
type parseResult<'a> = result<('a, list<Scanner.token>), parseError>

let errorAtHead = (tokens: list<Scanner.token>, message: string): parseError =>
  switch tokens {
  | list{t, ..._} => {message, line: t.line}
  | list{} => {message, line: 0}
  }

let consume = (
  tokens: list<Scanner.token>,
  expected: Scanner.tokenType,
  message: string,
): parseResult<Scanner.token> =>
  switch tokens {
  | list{t, ...rest} if t.tokenType == expected => Ok((t, rest))
  | _ => Error(errorAtHead(tokens, message))
  }

let toBinaryOp = (tt: Scanner.tokenType): option<Ast.operator> =>
  switch tt {
  | Plus => Some(Add)
  | Minus => Some(Minus)
  | Star => Some(Multiply)
  | Slash => Some(Divide)
  | EqualEqual => Some(Equal)
  | BangEqual => Some(NotEqual)
  | Less => Some(LessThan)
  | LessEqual => Some(LessThanOrEqual)
  | Greater => Some(GreaterThan)
  | GreaterEqual => Some(GreaterThanOrEqual)
  | _ => None
  }

let toUnaryOp = (tt: Scanner.tokenType): option<Ast.unaryOperator> =>
  switch tt {
  | Minus => Some(Negate)
  | Bang => Some(Exclamation)
  | _ => None
  }

let rec expression = tokens => equality(tokens)

and equality = tokens =>
  parseLeftAssoc(tokens, [Scanner.BangEqual, Scanner.EqualEqual], comparison)

and comparison = tokens =>
  parseLeftAssoc(
    tokens,
    [Scanner.Greater, Scanner.GreaterEqual, Scanner.Less, Scanner.LessEqual],
    term,
  )

and term = tokens => parseLeftAssoc(tokens, [Scanner.Plus, Scanner.Minus], factor)

and factor = tokens => parseLeftAssoc(tokens, [Scanner.Star, Scanner.Slash], unary)

and unary = (tokens: list<Scanner.token>): parseResult<Ast.expr> =>
  switch tokens {
  | list{t, ...rest} =>
    switch toUnaryOp(t.tokenType) {
    | Some(op) =>
      unary(rest)->Result.map(((inner, rest')) => (Ast.Unary(op, inner), rest'))
    | None => primary(tokens)
    }
  | list{} => Error({message: "Expected expression.", line: 0})
  }

and primary = (tokens: list<Scanner.token>): parseResult<Ast.expr> =>
  switch tokens {
  | list{t, ...rest} =>
    switch t.tokenType {
    | False => Ok((Ast.Literal(Bool(false)), rest))
    | True => Ok((Ast.Literal(Bool(true)), rest))
    | Nil => Ok((Ast.Literal(Null), rest))
    | Number =>
      switch t.literal {
      | LoxNumber(n) => Ok((Ast.Literal(Float(n)), rest))
      | LoxInt(n) => Ok((Ast.Literal(Float(Int.toFloat(n))), rest))
      | _ => Error({message: "Number token missing numeric literal.", line: t.line})
      }
    | String =>
      switch t.literal {
      | LoxString(s) => Ok((Ast.Literal(Ast.String(s)), rest))
      | _ => Error({message: "String token missing string literal.", line: t.line})
      }
    | LeftParen =>
      expression(rest)->Result.flatMap(((inner, rest')) =>
        consume(rest', Scanner.RightParen, "Expect ')' after expression.")->Result.map(((
          _,
          rest'',
        )) => (Ast.Grouping(inner), rest''))
      )
    | _ => Error({message: "Expected expression.", line: t.line})
    }
  | list{} => Error({message: "Expected expression.", line: 0})
  }

// Drives left-associative binary chains: parses one `nextRule`, then folds
// successive `op nextRule` pairs into a left-leaning Binary tree.
and parseLeftAssoc = (
  tokens: list<Scanner.token>,
  ops: array<Scanner.tokenType>,
  nextRule: list<Scanner.token> => parseResult<Ast.expr>,
): parseResult<Ast.expr> => {
  let rec loop = (left: Ast.expr, tokens: list<Scanner.token>) =>
    switch tokens {
    | list{t, ...rest} if ops->Array.includes(t.tokenType) =>
      switch toBinaryOp(t.tokenType) {
      | Some(op) =>
        nextRule(rest)->Result.flatMap(((right, rest')) =>
          loop(Ast.Binary(left, op, right), rest')
        )
      | None => Error({message: "Unexpected operator.", line: t.line})
      }
    | _ => Ok((left, tokens))
    }

  nextRule(tokens)->Result.flatMap(((left, rest)) => loop(left, rest))
}

let parse = (tokens: array<Scanner.token>): result<Ast.expr, parseError> => {
  expression(tokens->List.fromArray)->Result.map(((expr, _)) => expr)
}
