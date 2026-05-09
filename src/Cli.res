open NodeJs

module Log = Stdlib.Console

let readFileByLine = path => {
  let lines = []

  let rl = Readline.make({
    input: Fs.createReadStream(path),
    crlfDelay: infinity,
  })

  Promise.make((resolve, reject) => {
    rl
    ->Readline.Interface.on(Event.fromString("close"), _ => {
      rl->Readline.Interface.close
      resolve(lines)
    })
    ->ignore
    rl
    ->Readline.Interface.on(Event.fromString("error"), err => {
      reject(err)
    })
    ->ignore
    rl
    ->Readline.Interface.on(Event.fromString("line"), line => {
      lines->Array.push(line)
    })
    ->ignore
  })
}

let run = source => {
  let tokens = Scanner.scanTokens(Scanner.make(source))
  Parser.parse(tokens)
    ->Result.mapError(err => Log.log(err))
    ->Result.map(expr => Log.log(Ast.pretty(expr)))
    ->ignore
}

let runFile = (path: string) => {
  readFileByLine(path)
  ->Promise.then(lines => {
    Promise.resolve(lines->Array.join("\n"))
  })
  ->Promise.then(code => {
    if code->String.length > 0 {
      run(code)
    }
    Promise.resolve()
  })
  ->ignore
}

let runPrompt = () => {
  let rl = Readline.make({
    input: Process.stdin(Process.process),
    output: Process.stdout(Process.process),
    crlfDelay: infinity,
    prompt: "> ",
  })
  rl->Readline.Interface.prompt(Nullable.null)
  rl
  ->Readline.Interface.on(Event.fromString("line"), code => {
    if code->String.length > 0 {
      run(code)
    }
    rl->Readline.Interface.prompt(Nullable.null)
  })
  ->ignore
}

// runFile("./example.lox")
runPrompt()
