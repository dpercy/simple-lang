#lang reader "sl.rkt"

# assume some builtins: ord and chr

isDigit c = {
  i = ord c
  (and
   (lessEq (ord "0") i)
   (lessEq i (ord "9")))
}

isSpace c = match c {
  " " => True
  "\t" => True
  "\n" => True
  "\r" => True
  _ => False
}
