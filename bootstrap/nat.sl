

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

(struct (Less))
(struct (Equal))
(struct (Greater))

(def (compare x y)
  (match x
    [(Z) (match y
           [(Z) (Equal)]
           [(S y*) (Less)])]
    [(S x*) (match y
              [(Z) (Greater)]
              [(S y*) (compare x* y*)])]))
