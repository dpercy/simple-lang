#lang s-exp "sl.rkt"


(struct (Z))
(struct (S n))

(def (pred n)
  (match n
    [(S w) w]))

(def (+ x y)
  (match x
    [(Z) y]
    [(S n) (S (+ n y))]))
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
