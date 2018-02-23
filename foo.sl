#lang reader "sl.rkt"
1
2


3

x = 4

x

f y = 123
(f)
3
(f f)
(f

 f)

struct Point x y
(Point 1 2)
(Point 1)
Point

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
