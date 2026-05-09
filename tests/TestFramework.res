@module("node:test") external test: (string, unit => unit) => unit = "test"

module Assert = {
  @module("node:assert/strict")
  external equal: ('a, 'a) => unit = "strictEqual"

  @module("node:assert/strict")
  external deepEqual: ('a, 'a) => unit = "deepStrictEqual"
}
