type rec expr =
  | Literal(literal)
  | Unary(unaryOperator, expr)
  | Binary(expr, operator, expr)
  | Grouping(expr)

and literal =
  | Float(float)
  | String(string)
  | Bool(bool)
  | @as(null) Null

and unaryOperator =
  | Negate
  | Exclamation

and operator =
  | Add
  | Minus
  | Multiply
  | Divide
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual

let rec pretty = (expr: expr): string => {
  switch expr {
  | Literal(Float(n)) => String.make(n)
  | Literal(String(s)) => s
  | Literal(Bool(b)) => Bool.toString(b)
  | Literal(Null) => "null"
  | Unary(op, e) => parenthesize(unaryPretty(op), [e])
  | Binary(left, op, right) => parenthesize(binaryPretty(op), [left, right])
  | Grouping(e) => parenthesize("group", [e])
  }
}

and parenthesize = (name: string, exprs: array<expr>): string => {
  `(${name} ${exprs->Array.map(pretty)->Array.join(" ")})`
}

and unaryPretty = (unary: unaryOperator): string => {
  switch unary {
  | Negate => "-"
  | Exclamation => "!"
  }
}

and binaryPretty = (binary: operator): string => {
  switch binary {
  | Add => "+"
  | Minus => "-"
  | Multiply => "*"
  | Divide => "/"
  | Equal => "=="
  | NotEqual => "!="
  | LessThan => "<"
  | LessThanOrEqual => "<="
  | GreaterThan => ">"
  | GreaterThanOrEqual => ">="
  }
}
