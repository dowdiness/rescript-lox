// open NodeJs

module Value = {
  type t =
    | LoxBool(bool)
    | LoxInt(int)
    | LoxNumber(float)
    | LoxString(string)
    | LoxNil
}

type tokenType =
  // Single character tokens
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  // One or two character tokens
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  // Literals
  | Identifier
  | String
  | Number
  // Keywords
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | Eof

type token = {
  tokenType: tokenType,
  lexeme: string,
  literal: Value.t,
  line: int,
}

type scanner = {
  source: string,
  tokens: array<token>,
  start: int,
  current: int,
  line: int,
}

let makeScanner = (source) => {
  source: source,
  tokens: [],
  start: 0,
  current: 0,
  line: 1
}

let isAtEnd = (scanner) => {
  scanner.current >= scanner.source->Js.String.length
}

let advanceScanner = (scanner) => {
  ...scanner,
  current: scanner.current + 1
}

let getChar = (scanner) => {
  if scanner.current > scanner.source->Js.String.length {
    None
  } else {
    Some(Js.String.get(scanner.source, scanner.current - 1))
  }
}

let getLexeme = (scanner: scanner) => {
  scanner.source->Js.String2.substring(~from=scanner.start, ~to_=scanner.current)
}

let addToken = (scanner: scanner, tokenType) => {
  let token = {
    tokenType: tokenType,
    lexeme: getLexeme(scanner),
    literal: Value.LoxNil,
    line: scanner.line,
  }
  {
    ...scanner,
    tokens: Array.concat(scanner.tokens, [token])
  }
}

let addDoubleToken = (scanner, doubleToken, singleToken) => {
  switch scanner->advanceScanner->getChar {
    | None => addToken(scanner, singleToken)
    | Some(c) =>
      if c == "=" {
        addToken(advanceScanner(scanner), doubleToken)
      } else {
        addToken(scanner, singleToken)
      }
  }
}

let scanToken = (scanner) => {
  let scanner = advanceScanner(scanner)
  let c = getChar(scanner)
  switch c {
    | None => scanner
    | Some(char) =>
      switch char {
        | "(" => addToken(scanner, LeftParen)
        | ")" => addToken(scanner, RightParen)
        | "{" => addToken(scanner, LeftBrace)
        | "}" => addToken(scanner, RightBrace)
        | "," => addToken(scanner, Comma)
        | "." => addToken(scanner, Dot)
        | "-" => addToken(scanner, Minus)
        | "+" => addToken(scanner ,Plus)
        | ";" => addToken(scanner, Semicolon)
        | "*" => addToken(scanner, Star)
        | "!" => addDoubleToken(scanner, BangEqual, Bang)
        | "=" => addDoubleToken(scanner, EqualEqual, Equal)
        | "<" => addDoubleToken(scanner, LessEqual, Less)
        | ">" => addDoubleToken(scanner, GreaterEqual, Greater)
        | " " | "\r" | "\t" => scanner
        | "\n" => { ...scanner, line: scanner.line + 1 }
        | _ => {
          LoxError.error(Js.String.make(scanner.line), "Unexpected character.")
          scanner
        }
      }
  }
}

let rec scanTokens = (scanner) => {
  if isAtEnd(scanner) {
    let token =
      {
        tokenType: Eof,
        lexeme: "",
        literal: Value.LoxNil,
        line:scanner.line
      }
    Array.concat(scanner.tokens, [token])
  } else {
    let scanner = { ...scanner, start: scanner.current }
    scanTokens(scanToken(scanner))
  }
}

let tokenTypeToString = (tokenType: tokenType) => {
  switch tokenType {
    | LeftParen => "LeftParen"
    | RightParen => "RightParen"
    | LeftBrace => "LeftBrace"
    | RightBrace => "RightBrace"
    | Comma => "Comma"
    | Dot => "Dot"
    | Minus => "Minus"
    | Plus => "Plus"
    | Semicolon => "Semicolon"
    | Slash => "Slash"
    | Star => "Star"
    // One or two character tokens
    | Bang => "Bang"
    | BangEqual => "BangEqual"
    | Equal => "Equal"
    | EqualEqual => "EqualEqual"
    | Greater => "Greater"
    | GreaterEqual => "GreaterEqual"
    | Less => "Less"
    | LessEqual => "LessEqual"
    // Literals
    | Identifier => "Identifier"
    | String => "String"
    | Number => "Number"
    // Keywords
    | And => "And"
    | Class => "Class"
    | Else => "Else"
    | False => "False"
    | Fun => "Fun"
    | For => "For"
    | If => "If"
    | Nil => "Nil"
    | Or => "Or"
    | Print => "Print"
    | Return => "Return"
    | Super => "Super"
    | This => "This"
    | True => "True"
    | Var => "Var"
    | While => "While"
    | Eof => "Eof"
  }
}

let tokenToString = (token: token) => {
  tokenTypeToString(token.tokenType) ++ ", " ++ token.lexeme ++ ", " ++ Js.String.make(token.literal)
}

let tokens = scanTokens(makeScanner("((!*+-=<> <= =====))"))
Js.log(tokens->Array.map(tokenToString))
