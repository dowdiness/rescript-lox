let hadError = ref(false)

let report = (line, where, message) => {
  Console.log(`[line ${line}] Error"${where}": " ${message}`)
  hadError := true
}

let error = (line, message) => {
  report(line, "", message)
}
