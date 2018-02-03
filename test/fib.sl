#lang s-exp "../bootstrap/sl.rkt"


(def < primitives.<)
(def + primitives.+)
(def - primitives.-)

(def (fib n)
  (match (< n 2)
    [#true n]
    [#false (+ (fib (- n 1)) (fib (- n 2)))]))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)

(def x (fib 0))
(def y (fib 3))
