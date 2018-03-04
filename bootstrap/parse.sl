#lang reader "sl.rkt"




# What's a parser?
# A parser takes some stream of input, like chars or tokens,
# and either:
# - returns a value, leaving the input partially-eaten
# - fails, maybe with some explanation?

struct Peek
struct Get
struct HasNext
struct Fail why
struct Parsed val remainder
struct EOF

peek ! = match perform HasNext ! {
  True => perform Peek !
  False => EOF
}
get ! = match perform HasNext ! {
  True => perform Get !
  False => fail "EOF" !
}
fail why ! = perform (Fail why) !

runParser p input = match capture p {
  Done val => Parsed val input
  Perform HasNext k => {
    hasNext = less 0 (strlen input)
    runParser (k hasNext) input
  }

  Perform Peek k => {
# TODO be more generic about the input sequence
    c = slice input 0 1
    runParser (k c) input
  }
  Perform Get k => {
    c = slice input 0 1
    rest = slice input 1 (strlen input)
    runParser (k c) rest
  }
  Perform (Fail why) k => Fail why
}


getWhere errMsg pred ! = match peek ! {
  EOF => fail "EOF" !
  c =>
    match pred c {
      True => get !
      False => fail errMsg !
    }
}

either left right ! = match capture left {
# interesting case
  Perform (Fail why) k => {
# abandon left.
# also abandon the handler.
# just continue with right.
# TODO could also *combine* the two whys if right fails
    right !
  }

# passthrough cases
  Done v => v
  Perform eff k => {
# re-throw the effect
    v = perform eff !
# continue with what we got,
# still covered by the same handler
    either (k v) right !
  }
}
check runParser (either (return 1) (return 2)) "input" = Parsed 1 "input"
check runParser (either (fail "derp") (return 2)) "input" = Parsed 2 "input"
check runParser (either (fail "derp") (fail "doop")) "input" = Fail "doop"


# return is a very general procedure:
# all it does is return a value.
# This lets you wrap any value as a procedure.
return val ! = val


# TokNumber contains the string repr of the number.
struct TokNumber n


lex ! = {
  skipWhitespace !
# TODO or lex other things...
  TokNumber (lexNumber !)
}
check runParser lex " 9;" = Parsed (TokNumber "9") ";"


skipWhitespace ! = match peek ! {
  EOF => Void
  c =>
    match char.isSpace c {
      False => Void
      True => {
        get !
        skipWhitespace !
      }
    }
}
check runParser skipWhitespace " f " = Parsed Void "f "
check runParser skipWhitespace "  " = Parsed Void ""

lexNumber ! = {
  head = getWhere "isDigit" char.isDigit !
  tail = either lexNumber (return "") !
  strcat head tail
}

check runParser lexNumber "1" = Parsed "1" ""
check runParser lexNumber  "123 " = Parsed "123" " "
# TODO thread a counter through the parser to track "where" failed
check runParser (either (getWhere "isDigit" char.isDigit) (return "")) ";" = Parsed "" ";"

stringToNumber prefix s = match strlen s {
  0 => prefix
  len => {
    c = slice s 0 1
    d = sub (ord c) (ord "0")
    newPrefix = add d (mul 10 prefix)
    stringToNumber newPrefix (slice s 1 len)
  }
}
check stringToNumber 0 "123" = 123


parseExpr ! = {
  n = lex !
  match n {
    TokNumber n => core.Quote (stringToNumber 0 n)
  }
}

skipWhitespaceAfter p ! = {
  v = p !
  skipWhitespace !
  v
}

parse s = runParser (skipWhitespaceAfter parseExpr) s


check parse " 123 " = Parsed (core.Quote 123) ""


# Try writing imports as footnotes
import char
import core
