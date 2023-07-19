open NodeJs
open Promise

type lox = {
  hadError: bool
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
  kind: tokenType,
  lexeme: string,
  literal: bool,
  line: int,
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
  tokenTypeToString(token.kind) ++ " " ++ token.lexeme ++ " " ++ Js.String.make(token.literal)
}

let main = () => {
  let _loxEnv = {
    hadError: false
  }

  let token = {
    kind: Comma,
    lexeme: "test",
    literal: true,
    line: 43,
  }

  Js.log(tokenToString(token))
}

let _ = main()

let readFileByLine = (path) => {
  let lines = []

  let rl = Readline.make(
    Readline.interfaceOptions(~input=Fs.createReadStream(path), ~crlfDelay=infinity, ())
  )

  Promise.make((resolve, reject) => {
    rl
      ->Readline.Interface.on(Event.fromString("close"), _ => {
        rl->Readline.Interface.close
        resolve(. lines)
      })
      ->ignore
    rl
      ->Readline.Interface.on(Event.fromString("error"), err => {
        reject(. err)
      })
      ->ignore
    rl
      ->Readline.Interface.on(Event.fromString("line"), line => {
        open Js.Array2
        lines->push(line)->ignore
      })
      ->ignore
  })
}

let run = (source) => {
  // let tokens = scanTokens(source)
  Js.log(`Scanner: ${source}`)
}

let runFile = (path: string) => {
  readFileByLine(path)
    ->then(lines => {
      resolve(Js.Array.joinWith("\n", lines))
    })
    ->then(code => {
      if code->String.length > 0 {
        run(code)
      }
      resolve()
    })
    ->ignore
}

let runPrompt = () => {
  let rl = Readline.make(
    Readline.interfaceOptions(
      ~input=Process.stdin(Process.process),
      ~output=Process.stdout(Process.process),
      ~crlfDelay=infinity,
      ~prompt="> ",
      ()
    )
  )
  rl->Readline.Interface.prompt(null)
  rl
    ->Readline.Interface.on(Event.fromString("line"), code => {
      if code->String.length > 0 {
        run(code)
      }
      rl->Readline.Interface.prompt(null)
    })
    ->ignore
}

let report = (line, where, message) => {
  Js.log(`[line ${line}] Error"${where}": " ${message}`)
}

let error = (line, message) => {
  report(line, "", message)
}

// runFile("./example.lox")
// runPrompt()
