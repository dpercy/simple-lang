#lang s-exp "sl.rkt"

; built-in:
; (struct true 0)
; (struct false 0)

(def (not b)
  (match b
    [(true) (false)]
    [(false) (true)]))

; can't really do short-circuiting operations...
; would need laziness, macros, or something.


(def (and2 x y)
  (match x
    [(true) y]
    [(false) (false)]))

(def (and3 x y z)
  (and2 x (and2 y z)))

(def (or2 x y)
  (match x
    [(true) (true)]
    [(false) y]))

(def (or3 x y z)
  (or2 x (or2 y z)))

(def (or4 a b c d)
  (or2 a (or3 b c d)))

(def (or5 a b c d e)
  (or2 a (or4 b c d e)))
