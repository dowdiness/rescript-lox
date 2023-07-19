let hadError = ref(false)

let report = (line, where, message) => {
  Js.log(`[line ${line}] Error"${where}": " ${message}`)
  hadError := true
}

let error = (line, message) => {
  report(line, "", message)
}
