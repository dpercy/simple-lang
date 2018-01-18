

; built-in:
; (struct empty 0)
; (struct cons 2)

(def (first lst)
  (match lst
    [(cons x xs) x]))

(def (rest lst)
  (match lst
    [(cons x xs) xs]))

(def (length lst)
  (match lst
    [(empty) (Z)]
    [(cons x xs) (S (length xs))]))

(def (list-ref lst i)
  (match i
    [(Z) (first lst)]
    [(S n) (list-ref (rest lst) n)]))

(def (map f lst)
  (match lst
    [(empty) (empty)]
    [(cons x xs) (cons (f x)
                       (map f xs))]))

(def (filter f lst)
  (match lst
    [(empty) (empty)]
    [(cons x xs) (match (f x)
                   [(true) (cons x (filter f xs))]
                   [(false) (filter f xs)])]))
