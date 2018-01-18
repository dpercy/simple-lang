

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

(def (or2 x y)
  (match x
    [(true) (true)]
    [(false) y]))
