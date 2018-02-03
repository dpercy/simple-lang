#lang s-exp "sl.rkt"

(def (int? v) (primitives.int? v))

(def (+ x y) (primitives.+ x y))
(def (- x y) (primitives.- x y))
(def (* x y) (primitives.* x y))
(def (/ x y) (primitives./ x y))

(def (< x y) (primitives.< x y))
(def (= x y) (primitives.= x y))

(def (> x y)
  (< y x))

(def (>= x y)
  (bool.not (< x y)))

(def (<= x y)
  (bool.not (> x y)))


; TODO what if x or y is negative? learn about rounding modes...
(def (mod x y)
  (- x (* y (/ x y))))
