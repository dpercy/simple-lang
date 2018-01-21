#lang s-exp "sl.rkt"

(import "int.sl")

(def (fib n)
  (match (< n 2)
    [#true n]
    [#false (+ (fib (- n 1)) (fib (- n 2)))]))

(fib 0)
(fib 3)

(def x (fib 0))
(def y (fib 3))
