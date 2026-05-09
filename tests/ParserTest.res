open TestFramework

// End-to-end tests: source string → Scanner → Parser → Ast.pretty.
// Compares the printed AST to the expected S-expression.

let parseToSexpr = (source: string): result<string, Parser.parseError> => {
  let tokens = Scanner.scanTokens(Scanner.make(source))
  Parser.parse(tokens)->Result.map(Ast.pretty)
}

let assertParses = (source: string, expected: string) =>
  switch parseToSexpr(source) {
  | Ok(s) => Assert.equal(s, expected)
  | Error(e) => Assert.equal(`error: ${e.message}`, expected)
  }

test("parse: integer literal", () => {
  assertParses("42", "42")
})

test("parse: addition", () => {
  assertParses("1 + 2", "(+ 1 2)")
})

test("parse: precedence — * binds tighter than +", () => {
  assertParses("1 + 2 * 3", "(+ 1 (* 2 3))")
})

test("parse: left-associativity of +", () => {
  assertParses("1 + 2 + 3", "(+ (+ 1 2) 3)")
})

test("parse: unary negation", () => {
  assertParses("-5", "(- 5)")
})

test("parse: unary applied to grouped expression", () => {
  assertParses("-(5 + 2)", "(- (group (+ 5 2)))")
})

test("parse: comparison and equality", () => {
  assertParses("1 < 2 == true", "(== (< 1 2) true)")
})

test("parse: parenthesized expression", () => {
  assertParses("(1 + 2) * 3", "(* (group (+ 1 2)) 3)")
})

test("parse: error on missing right paren", () => {
  switch parseToSexpr("(1 + 2") {
  | Ok(s) => Assert.equal(`unexpected ok: ${s}`, "error")
  | Error(e) => Assert.equal(e.message, "Expect ')' after expression.")
  }
})

test("parse: error on bare operator", () => {
  switch parseToSexpr("+ 1") {
  | Ok(s) => Assert.equal(`unexpected ok: ${s}`, "error")
  | Error(e) => Assert.equal(e.message, "Expected expression.")
  }
})
