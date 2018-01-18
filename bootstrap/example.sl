#lang s-exp "sl.rkt"

(include "nat.sl")

(+ (S (S (Z))) (S (S (S (Z)))))


(def (fib n)
  (match n
    [(Z) n]
    [(S (Z)) n]
    [somethingelse (+ (fib (pred n)) (fib (pred (pred n))))]))

(fib (Z))
(fib (S (S (S (Z)))))

(def x (fib (Z)))
(def y (fib (S (S (S (Z))))))
