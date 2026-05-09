type t = {
  scanner: Scanner.t
}

let make = (source: string) => {
  let scanner = Scanner.make(source)
  { scanner }
}
