// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Scanner from "./scanner.bs.js";
import * as Process from "process";
import * as Js_array from "rescript/lib/es6/js_array.js";
import * as Readline from "readline";
import * as Pervasives from "rescript/lib/es6/pervasives.js";

function readFileByLine(path) {
  var lines = [];
  var rl = Readline.createInterface({
        input: Fs.createReadStream(path),
        crlfDelay: Pervasives.infinity
      });
  return new Promise((function (resolve, reject) {
                rl.on("close", (function (param) {
                        rl.close();
                        resolve(lines);
                      }));
                rl.on("error", (function (err) {
                        reject(err);
                      }));
                rl.on("line", (function (line) {
                        lines.push(line);
                      }));
              }));
}

function run(source) {
  var tokens = Scanner.scanTokens(Scanner.make(source));
  console.log(tokens.map(Scanner.tokenToString));
}

function runFile(path) {
  readFileByLine(path).then(function (lines) {
          return Promise.resolve(Js_array.joinWith("\n", lines));
        }).then(function (code) {
        if (code.length > 0) {
          run(code);
        }
        return Promise.resolve(undefined);
      });
}

function runPrompt(param) {
  var rl = Readline.createInterface({
        input: Process.stdin,
        output: Process.stdout,
        prompt: "> ",
        crlfDelay: Pervasives.infinity
      });
  rl.prompt(null);
  rl.on("line", (function (code) {
          if (code.length > 0) {
            run(code);
          }
          rl.prompt(null);
        }));
}

runPrompt(undefined);

export {
  readFileByLine ,
  run ,
  runFile ,
  runPrompt ,
}
/*  Not a pure module */
