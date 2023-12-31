// Generated by ReScript, PLEASE EDIT WITH CARE

import * as LoxError from "./loxError.bs.js";

var Value = {};

var keywords = [
  [
    "and",
    /* And */22
  ],
  [
    "class",
    /* Class */23
  ],
  [
    "else",
    /* Else */24
  ],
  [
    "false",
    /* False */25
  ],
  [
    "fun",
    /* Fun */26
  ],
  [
    "for",
    /* For */27
  ],
  [
    "if",
    /* If */28
  ],
  [
    "nil",
    /* Nil */29
  ],
  [
    "or",
    /* Or */30
  ],
  [
    "print",
    /* Print */31
  ],
  [
    "return",
    /* Return */32
  ],
  [
    "super",
    /* Super */33
  ],
  [
    "this",
    /* This */34
  ],
  [
    "true",
    /* True */35
  ],
  [
    "var",
    /* Var */36
  ],
  [
    "while",
    /* While */37
  ]
];

function make(source) {
  return {
          source: source,
          tokens: [],
          start: 0,
          current: 0,
          line: 1
        };
}

function isAtEnd(param) {
  return param.current >= param.source.length;
}

function isDigit(c) {
  if (c >= "0") {
    return c <= "9";
  } else {
    return false;
  }
}

function isAlpha(c) {
  if (c >= "a" && c <= "z" || c >= "A" && c <= "Z") {
    return true;
  } else {
    return c === "_";
  }
}

function isAlphaNumeric(c) {
  if (isAlpha(c)) {
    return true;
  } else {
    return isDigit(c);
  }
}

function advanceScanner(scanner) {
  return {
          source: scanner.source,
          tokens: scanner.tokens,
          start: scanner.start,
          current: scanner.current + 1 | 0,
          line: scanner.line
        };
}

function getChar(param) {
  var current = param.current;
  var source = param.source;
  if (current > source.length) {
    return ;
  } else {
    return source[current - 1 | 0];
  }
}

function getLexeme(param) {
  return param.source.substring(param.start, param.current);
}

function peek(scanner) {
  if (isAtEnd(scanner)) {
    return "\0";
  } else {
    return scanner.source.charAt(scanner.current);
  }
}

function peekNext(scanner) {
  if ((scanner.current + 1 | 0) >= scanner.source.length) {
    return "\0";
  } else {
    return scanner.source.charAt(scanner.current + 1 | 0);
  }
}

function addToken(scanner, tokenType) {
  return {
          source: scanner.source,
          tokens: scanner.tokens.concat([{
                  tokenType: tokenType,
                  lexeme: getLexeme(scanner),
                  literal: /* LoxNil */0,
                  line: scanner.line
                }]),
          start: scanner.start,
          current: scanner.current,
          line: scanner.line
        };
}

function addTokenWithLiteral(scanner, tokenType, literal) {
  return {
          source: scanner.source,
          tokens: scanner.tokens.concat([{
                  tokenType: tokenType,
                  lexeme: getLexeme(scanner),
                  literal: literal,
                  line: scanner.line
                }]),
          start: scanner.start,
          current: scanner.current,
          line: scanner.line
        };
}

function addDoubleToken(scanner, doubleToken, singleToken) {
  var match = getChar(advanceScanner(scanner));
  if (match === "=") {
    return addToken(advanceScanner(scanner), doubleToken);
  } else {
    return addToken(scanner, singleToken);
  }
}

function addCommentToken(scanner) {
  var match = getChar(advanceScanner(scanner));
  if (match === "/") {
    var _scanner = scanner;
    while(true) {
      var scanner$1 = _scanner;
      if (!(peek(scanner$1) !== "\n" && !isAtEnd(scanner$1))) {
        return scanner$1;
      }
      _scanner = advanceScanner(scanner$1);
      continue ;
    };
  } else {
    return addToken(scanner, /* Slash */9);
  }
}

function addStringToken(scanner) {
  var consumeString = function (_scanner) {
    while(true) {
      var scanner = _scanner;
      if (!(peek(scanner) !== "\"" && !isAtEnd(scanner))) {
        return scanner;
      }
      if (peek(scanner) !== "\n") {
        _scanner = advanceScanner({
              source: scanner.source,
              tokens: scanner.tokens,
              start: scanner.start,
              current: scanner.current,
              line: scanner.line + 1 | 0
            });
        continue ;
      }
      _scanner = advanceScanner(scanner);
      continue ;
    };
  };
  var scanner$1 = consumeString(scanner);
  if (isAtEnd(scanner$1)) {
    LoxError.error(String(scanner$1.line), "Unterminated string.");
    return scanner$1;
  }
  var scanner$2 = advanceScanner(scanner$1);
  var literal = {
    TAG: /* LoxString */3,
    _0: scanner$2.source.substring(scanner$2.start + 1 | 0, scanner$2.current - 1 | 0)
  };
  return addTokenWithLiteral(scanner$2, /* String */20, literal);
}

function addNumberToken(scanner) {
  var consumeNumber = function (_scanner) {
    while(true) {
      var scanner = _scanner;
      if (isDigit(peek(scanner))) {
        _scanner = advanceScanner(scanner);
        continue ;
      }
      if (!(peek(scanner) === "." && isDigit(peekNext(scanner)))) {
        return scanner;
      }
      _scanner = advanceScanner(scanner);
      continue ;
    };
  };
  var scanner$1 = consumeNumber(scanner);
  return addToken(scanner$1, /* Number */21);
}

function addIdentifierToken(scanner) {
  var consumeIdentifier = function (_scanner) {
    while(true) {
      var scanner = _scanner;
      if (!isAlphaNumeric(peek(scanner))) {
        return scanner;
      }
      _scanner = advanceScanner(scanner);
      continue ;
    };
  };
  var scanner$1 = consumeIdentifier(scanner);
  var text = scanner$1.source.substring(scanner$1.start, scanner$1.current);
  var match = keywords.find(function (param) {
        return param[0] === text;
      });
  var token = match !== undefined ? match[1] : /* Identifier */19;
  return addToken(scanner$1, token);
}

function scanToken(scanner) {
  var scanner$1 = advanceScanner(scanner);
  var c = getChar(scanner$1);
  if (c === undefined) {
    return scanner$1;
  }
  switch (c) {
    case "!" :
        return addDoubleToken(scanner$1, /* BangEqual */12, /* Bang */11);
    case "(" :
        return addToken(scanner$1, /* LeftParen */0);
    case ")" :
        return addToken(scanner$1, /* RightParen */1);
    case "*" :
        return addToken(scanner$1, /* Star */10);
    case "+" :
        return addToken(scanner$1, /* Plus */7);
    case "," :
        return addToken(scanner$1, /* Comma */4);
    case "-" :
        return addToken(scanner$1, /* Minus */6);
    case "." :
        return addToken(scanner$1, /* Dot */5);
    case "/" :
        return addCommentToken(scanner$1);
    case ";" :
        return addToken(scanner$1, /* Semicolon */8);
    case "<" :
        return addDoubleToken(scanner$1, /* LessEqual */18, /* Less */17);
    case "=" :
        return addDoubleToken(scanner$1, /* EqualEqual */14, /* Equal */13);
    case ">" :
        return addDoubleToken(scanner$1, /* GreaterEqual */16, /* Greater */15);
    case "\"" :
        return addStringToken(scanner$1);
    case "\n" :
        return {
                source: scanner$1.source,
                tokens: scanner$1.tokens,
                start: scanner$1.start,
                current: scanner$1.current,
                line: scanner$1.line + 1 | 0
              };
    case " " :
    case "\r" :
    case "\t" :
        return scanner$1;
    case "{" :
        return addToken(scanner$1, /* LeftBrace */2);
    case "}" :
        return addToken(scanner$1, /* RightBrace */3);
    default:
      if (isDigit(c)) {
        return addNumberToken(scanner$1);
      } else if (isAlpha(c)) {
        return addIdentifierToken(scanner$1);
      } else {
        LoxError.error(String(scanner$1.line), "Unexpected character.");
        return scanner$1;
      }
  }
}

function scanTokens(_scanner) {
  while(true) {
    var scanner = _scanner;
    if (isAtEnd(scanner)) {
      var token_line = scanner.line;
      var token = {
        tokenType: /* Eof */38,
        lexeme: "",
        literal: /* LoxNil */0,
        line: token_line
      };
      return scanner.tokens.concat([token]);
    }
    var scanner_source = scanner.source;
    var scanner_tokens = scanner.tokens;
    var scanner_start = scanner.current;
    var scanner_current = scanner.current;
    var scanner_line = scanner.line;
    var scanner$1 = {
      source: scanner_source,
      tokens: scanner_tokens,
      start: scanner_start,
      current: scanner_current,
      line: scanner_line
    };
    _scanner = scanToken(scanner$1);
    continue ;
  };
}

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
  return tokenTypeToString(token.tokenType) + ", " + token.lexeme + ", " + String(token.literal);
}

export {
  Value ,
  keywords ,
  make ,
  isAtEnd ,
  isDigit ,
  isAlpha ,
  isAlphaNumeric ,
  advanceScanner ,
  getChar ,
  getLexeme ,
  peek ,
  peekNext ,
  addToken ,
  addTokenWithLiteral ,
  addDoubleToken ,
  addCommentToken ,
  addStringToken ,
  addNumberToken ,
  addIdentifierToken ,
  scanToken ,
  scanTokens ,
  tokenTypeToString ,
  tokenToString ,
}
/* No side effect */
