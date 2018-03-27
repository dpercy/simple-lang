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
# re-throw the effect - TODO is this why effects escape the "try" ?
#  - maybe instead, do the "amb" thing:
#    - effect for fail
#    - effect for choice
#    - either is just choose-then-run?
    v = perform eff !
# continue with what we got,
# still covered by the same handler
    either (k v) right !
  }
}
check runParser (either (return 1) (return 2)) "input" = Parsed 1 "input"
check runParser (either (fail "derp") (return 2)) "input" = Parsed 2 "input"
check runParser (either (fail "derp") (fail "doop")) "input" = Fail "doop"

seq x y ! = {
  x !
  y !
}
check runParser (seq get get) "input" = Parsed "n" "put"
check runParser (either (seq get (fail "derp")) (return 1)) "input" = Parsed 1 "input"

alternatives parsers = list.foldr1 either parsers
check runParser (alternatives [fail "a", return 1, fail "c"]) "input" = Parsed 1 "input"
check runParser (alternatives [fail "a", fail "b", fail "c"]) "input" = Fail "c"


# return is a very general procedure:
# all it does is return a value.
# This lets you wrap any value as a procedure.
return val ! = val


# TokNumber contains the string repr of the number.
struct TokNumber n
struct TokOpenParen
struct TokCloseParen


lex ! = {
  skipWhitespace !
# TODO or lex other things...
  alternatives [
    lexNumber
    lexPunctuation "(" TokOpenParen
    lexPunctuation ")" TokCloseParen
  ] !
}
check runParser lex " 9;" = Parsed (TokNumber "9") ";"
check runParser lex "(9;" = Parsed TokOpenParen "9;"
check runParser lex ")9;" = Parsed TokCloseParen "9;"


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


lexPunctuation c t ! = {
  getWhere c (equal c) !
  t
}

lexNumber ! = {
  head = getWhere "isDigit" char.isDigit !
  match either lexNumber (return (TokNumber "")) ! {
    TokNumber tail =>
      TokNumber (strcat head tail)
  }
}

check runParser lexNumber "1" = Parsed (TokNumber "1") ""
check runParser lexNumber  "123 " = Parsed (TokNumber "123") " "
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
  f = parseArg !
# TODO when parseCall fails, the state isn't rolled back??
  either (parseCall f) (return f) !
}

parseCall f ! = {
  a = parseArg !
  core.App f a
}

parseArg ! = {
  n = lex !
  match n {
    TokNumber n => core.Quote (stringToNumber 0 n)
    TokOpenParen => {
      e = parseExpr !
      t = lex !
      match t {
#TokCloseParen => e
      }
    }
    t => fail "unexpected token" !
  }
}

skipWhitespaceAfter p ! = {
  v = p !
  skipWhitespace !
  v
}

parse s = runParser (skipWhitespaceAfter parseExpr) s


check parse " 123 " = Parsed (core.Quote 123) ""
check parse " 1 2 " = Parsed (core.App (core.Quote 1) (core.Quote 2)) ""
check runParser parseArg "1)" = Parsed (core.Quote 1) ")"
check runParser parseExpr "1)" = Parsed (core.Quote 1) ")"
runParser (either (return 4) (parseCall 4)) ")"
runParser (either (parseCall 4) (return 4)) ")"
#check parse " (123 456) " = Parsed (core.App (core.Quote 123) (core.Quote 456)) ""


# Try writing imports as footnotes
import char
import core
import list
