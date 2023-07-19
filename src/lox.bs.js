// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Process from "process";
import * as Js_array from "rescript/lib/es6/js_array.js";
import * as Readline from "readline";
import * as Pervasives from "rescript/lib/es6/pervasives.js";

function tokenTypeToString(tokenType) {
  switch (tokenType) {
    case /* LeftParen */0 :
        return "LeftParen";
    case /* RightParen */1 :
        return "RightParen";
    case /* LeftBrace */2 :
        return "LeftBrace";
    case /* RightBrace */3 :
        return "RightBrace";
    case /* Comma */4 :
        return "Comma";
    case /* Dot */5 :
        return "Dot";
    case /* Minus */6 :
        return "Minus";
    case /* Plus */7 :
        return "Plus";
    case /* Semicolon */8 :
        return "Semicolon";
    case /* Slash */9 :
        return "Slash";
    case /* Star */10 :
        return "Star";
    case /* Bang */11 :
        return "Bang";
    case /* BangEqual */12 :
        return "BangEqual";
    case /* Equal */13 :
        return "Equal";
    case /* EqualEqual */14 :
        return "EqualEqual";
    case /* Greater */15 :
        return "Greater";
    case /* GreaterEqual */16 :
        return "GreaterEqual";
    case /* Less */17 :
        return "Less";
    case /* LessEqual */18 :
        return "LessEqual";
    case /* Identifier */19 :
        return "Identifier";
    case /* String */20 :
        return "String";
    case /* Number */21 :
        return "Number";
    case /* And */22 :
        return "And";
    case /* Class */23 :
        return "Class";
    case /* Else */24 :
        return "Else";
    case /* False */25 :
        return "False";
    case /* Fun */26 :
        return "Fun";
    case /* For */27 :
        return "For";
    case /* If */28 :
        return "If";
    case /* Nil */29 :
        return "Nil";
    case /* Or */30 :
        return "Or";
    case /* Print */31 :
        return "Print";
    case /* Return */32 :
        return "Return";
    case /* Super */33 :
        return "Super";
    case /* This */34 :
        return "This";
    case /* True */35 :
        return "True";
    case /* Var */36 :
        return "Var";
    case /* While */37 :
        return "While";
    case /* Eof */38 :
        return "Eof";
    
  }
}

function tokenToString(token) {
  return tokenTypeToString(token.kind) + " " + token.lexeme + " " + String(token.literal);
}

function main(param) {
  console.log(tokenToString({
            kind: /* Comma */4,
            lexeme: "test",
            literal: true,
            line: 43
          }));
}

main(undefined);

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
  console.log("Scanner: " + source + "");
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

function report(line, where, message) {
  console.log("[line " + line + "] Error\"" + where + "\": \" " + message + "");
}

function error(line, message) {
  report(line, "", message);
}

export {
  tokenTypeToString ,
  tokenToString ,
  main ,
  readFileByLine ,
  run ,
  runFile ,
  runPrompt ,
  report ,
  error ,
}
/*  Not a pure module */
