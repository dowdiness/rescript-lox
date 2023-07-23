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

type t = {
  source: string,
  tokens: array<token>,
  start: int,
  current: int,
  line: int,
}

let keywords = [
  ("and", And),
  ("class", Class),
  ("else", Else),
  ("false" ,False),
  ("fun", Fun),
  ("for", For),
  ("if", If),
  ("nil", Nil),
  ("or", Or),
  ("print", Print),
  ("return", Return),
  ("super", Super),
  ("this", This),
  ("true", True),
  ("var", Var),
  ("while", While),
]

let make = (source) => {
  source: source,
  tokens: [],
  start: 0,
  current: 0,
  line: 1
}

let isAtEnd = ({ source, current }) => {
  current >= source->Js.String.length
}

let isDigit = (c) => {
  c >= "0" && c <= "9"
}

let isAlpha = (c) => {
  (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c == "_"
}

let isAlphaNumeric = (c) => {
  isAlpha(c) || isDigit(c)
}

let advanceScanner = (scanner) => {
  ...scanner,
  current: scanner.current + 1
}

let getChar = ({ source, current }) => {
  if current > source->Js.String.length {
    None
  } else {
    Some(Js.String.get(source, current - 1))
  }
}

let getLexeme = ({ source, start, current }) => {
  source->Js.String2.substring(~from=start, ~to_=current)
}

let peek = (scanner) => {
  if isAtEnd(scanner) {
    "\0"
  } else {
    String.charAt(scanner.source, scanner.current)
  }
}

let peekNext = (scanner) => {
  if scanner.current + 1 >= scanner.source->String.length {
    "\0"
  } else {
    String.charAt(scanner.source, scanner.current + 1)
  }
}

let addToken = (scanner, tokenType) => {
  {
    ...scanner,
    tokens: Array.concat(
      scanner.tokens,
      [{
        tokenType: tokenType,
        lexeme: getLexeme(scanner),
        literal: Value.LoxNil,
        line: scanner.line,
      }])
  }
}

let addTokenWithLiteral = (scanner, tokenType, literal) => {
  {
    ...scanner,
    tokens: Array.concat(
      scanner.tokens,
      [{
        tokenType: tokenType,
        lexeme: getLexeme(scanner),
        literal: literal,
        line: scanner.line,
      }])
  }
}

let addDoubleToken = (scanner, doubleToken, singleToken) => {
  switch scanner->advanceScanner->getChar {
    | None => addToken(scanner, singleToken)
    | Some("=") => addToken(advanceScanner(scanner), doubleToken)
    | Some(_) => addToken(scanner, singleToken)
  }
}

let addCommentToken = (scanner) => {
  let rec commentOut = (scanner) => {
    if peek(scanner) != "\n" && !isAtEnd(scanner) {
      scanner->advanceScanner->commentOut
    } else {
      scanner
    }
  }
  switch scanner->advanceScanner->getChar {
    | None => addToken(scanner, Slash)
    | Some("/") => commentOut(scanner)
    | Some(_) => addToken(scanner, Slash)
  }
}

let addStringToken = (scanner) => {
  let rec consumeString = (scanner) => {
    if peek(scanner) != "\"" && !isAtEnd(scanner) {
      if peek(scanner) != "\n" {
        { ...scanner, line: scanner.line + 1 }->advanceScanner->consumeString
      } else {
        scanner->advanceScanner->consumeString
      }
    } else {
      scanner
    }
  }
  let closing = (scanner) => {
    if isAtEnd(scanner) {
      LoxError.error(Js.String.make(scanner.line), "Unterminated string.")
      scanner
    } else {
      let trim = (scanner) => {
        let literal= Value.LoxString(String.substring(scanner.source, ~start=scanner.start + 1, ~end=scanner.current - 1))
        addTokenWithLiteral(scanner, String, literal)
      }
      scanner->advanceScanner->trim
    }
  }
  scanner->consumeString->closing
}

let addNumberToken = (scanner) => {
  let rec consumeNumber = (scanner) => {
    if peek(scanner)->isDigit {
      scanner->advanceScanner->consumeNumber
    } else {
      if peek(scanner) == "." && peekNext(scanner)->isDigit {
        scanner->advanceScanner->consumeNumber
      } else {
        scanner
      }
    }
  }
  let scanner = consumeNumber(scanner)
  addToken(scanner, Number)
}

let addIdentifierToken = (scanner) => {
  let rec consumeIdentifier = (scanner) => {
    if peek(scanner)->isAlphaNumeric {
      scanner->advanceScanner->consumeIdentifier
    } else {
      scanner
    }
  }
  let scanner = consumeIdentifier(scanner)
  let text = String.substring(scanner.source, ~start=scanner.start, ~end=scanner.current)
  let token =
    switch keywords->Js.Array2.find(((keyword, _)) => keyword == text) {
      | Some((_, token)) => token
      | None => Identifier
    }
  addToken(scanner, token)
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
        | "/" => addCommentToken(scanner)
        | " " | "\r" | "\t" => scanner
        | "\n" => { ...scanner, line: scanner.line + 1 }
        | `"` => addStringToken(scanner)
        | c when isDigit(c) => addNumberToken(scanner)
        | c when isAlpha(c) => addIdentifierToken(scanner)
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
