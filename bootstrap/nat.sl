

; built-in:
; (struct (Z))
; (struct (S n))

(def (pred n)
  (match n
    [(S w) w]))

(def (+ x y)
  (match x
    [(Z) y]
    [(S n) (S (+ n y))]))
