

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

(def (- x y)
  (match y
    [(Z) x]
    [(S y*) (match x
              [(Z) (error "subtraction: went below zero")]
              [(S x*) (- x* y*)])]))

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

(def (< x y)
  (match (compare x y)
    [(Less) (true)]
    [_ (false)]))

(def (> x y)
  (match (compare x y)
    [(Greater) (true)]
    [_ (false)]))

(def (= x y)
  (match (compare x y)
    [(Equal) (true)]
    [_ (false)]))

(def (>= x y)
  (not (< x y)))

(def (<= x y)
  (not (> x y)))
