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




match True {
  False => "nope"
  True => "yay"
}
True
False


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

range lo hi ! = match less lo hi {
  True => ({
    (perform (Yield lo) !)
    (range (add lo 1) hi !)
  })
  False => "ok"
}

listEffects (range -3 10)


limit n comp ! = match less 0 n {
  False => "done!"
  True =>
    match capture comp {
      Done v => v
      Perform (Yield v) k => {
        perform (Yield v) !
        limit (add n -1) (k "ok") !
      }
      Perform eff k => {
        perform eff !
        limit n (k "ok") !
      }
    }
}

listEffects (limit 4 (range -3 10))



# TODO check plan - start JS VM?
