#lang reader "sl.rkt"
1
2


3

x = 4

x

f y = 123
(f)
3
f 9
(f f)
(f

 f)

struct Point x y
(Point 1 2)
(Point 1)
Point
"hrm"
match Point 1 2 {
  v => v
}

match Point 1 2 {
  Point x y => y
}

struct Wrap x

match Wrap 1 {
  Point x y => y
  Wrap q => q
}

match Wrap (Point 1 2) {
  Wrap (Wrap _) => 9
  Wrap (Point x y) => x
}

struct Yolo
Yolo

match Yolo {
  Yolo => 12321
}

match 5 {
  Yolo => 9
  Wrap 5 => 99
  3 => 999
  5 => 1337
}

"string"


import lib
lib.x
lib.asdf
lib.Foo
lib.W 1

match lib.Foo {
  lib.Foo => "yes"
}

match lib.W "ok" {
  lib.Foo => "no"
  lib.W x => x
}


match True {
  False => "nope"
  True => "yay"
}
True
False


# TODO effects!
# - primitive procedure "perform",
#   throws any value.
# - primitive structs Done and Perform
# - primitive function iter?
#                      capture?
#                      reify?
# - new syntax: def proc
# - new syntax: invoke proc


map f lst = match lst {
  Empty => Empty
  Cons x xs => Cons (f x) (map f xs)
}

map Wrap (Cons 1 (Cons 2 Empty))

listEffects comp = match capture comp {
  Done v => Empty
  Perform eff k => Cons eff (listEffects
                             (k "asdf"))
}


struct Yield val

begin2 x y = y

range lo hi ! = match less lo hi {
  True => (begin2
           (perform (Yield lo) !)
           ((range (add lo 1) hi) !)
          )
  False => "ok"
}

listEffects (range -3 10)
