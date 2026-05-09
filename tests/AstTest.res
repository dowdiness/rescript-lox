open TestFramework

test("pretty: Float literal", () => {
  Assert.equal(Ast.pretty(Literal(Float(42.0))), "42")
})

test("pretty: string literal", () => {
  Assert.equal(Ast.pretty(Literal(String("hello"))), "hello")
})

test("pretty: bool literals", () => {
  Assert.equal(Ast.pretty(Literal(Bool(true))), "true")
  Assert.equal(Ast.pretty(Literal(Bool(false))), "false")
})

test("pretty: null literal", () => {
  Assert.equal(Ast.pretty(Literal(Null)), "null")
})

test("pretty: unary negation", () => {
  Assert.equal(Ast.pretty(Unary(Negate, Literal(Float(5.0)))), "(- 5)")
})

test("pretty: unary not", () => {
  Assert.equal(Ast.pretty(Unary(Exclamation, Literal(Bool(true)))), "(! true)")
})

test("pretty: binary add", () => {
  Assert.equal(
    Ast.pretty(Binary(Literal(Float(1.0)), Add, Literal(Float(2.0)))),
    "(+ 1 2)",
  )
})

test("pretty: all binary operators", () => {
  let cases: array<(Ast.operator, string)> = [
    (Add, "+"),
    (Minus, "-"),
    (Multiply, "*"),
    (Divide, "/"),
    (Equal, "=="),
    (NotEqual, "!="),
    (LessThan, "<"),
    (LessThanOrEqual, "<="),
    (GreaterThan, ">"),
    (GreaterThanOrEqual, ">="),
  ]
  cases->Array.forEach(((op, sym)) => {
    Assert.equal(
      Ast.pretty(Binary(Literal(Float(1.0)), op, Literal(Float(2.0)))),
      `(${sym} 1 2)`,
    )
  })
})

test("pretty: grouping", () => {
  Assert.equal(Ast.pretty(Grouping(Literal(Float(7.0)))), "(group 7)")
})

test("pretty: nested expression from book", () => {
  let expr = Ast.Grouping(
    Unary(Negate, Binary(Literal(Float(5.0)), Add, Literal(Float(2.0))))
  )
  Assert.equal(Ast.pretty(expr), "(group (- (+ 5 2)))")
})

test("pretty: precedence is unambiguous", () => {
  // (1 + 2) * 3   — left side is the multiplication's left child
  let expr = Ast.Binary(
    Binary(Literal(Float(1.0)), Add, Literal(Float(2.0))),
    Multiply,
    Literal(Float(3.0)),
  )
  Assert.equal(Ast.pretty(expr), "(* (+ 1 2) 3)")

  // 1 - (2 - 3)  — right-associative grouping survives the round trip
  let expr2 = Ast.Binary(
    Literal(Float(1.0)),
    Minus,
    Binary(Literal(Float(2.0)), Minus, Literal(Float(3.0))),
  )
  Assert.equal(Ast.pretty(expr2), "(- 1 (- 2 3))")
})
