open NodeJs
open Promise

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
  let tokens = Scanner.scanTokens(Scanner.makeScanner(source))
  Js.log(tokens->Array.map(Scanner.tokenToString))
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

// runFile("./example.lox")
runPrompt()
