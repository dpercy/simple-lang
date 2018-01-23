#lang s-exp "sl.rkt"


(struct (empty))
(struct (cons first rest))

(def (first lst)
  (match lst
    [(cons x xs) x]))

(def (rest lst)
  (match lst
    [(cons x xs) xs]))

(def (length lst)
  (match lst
    [(empty) 0]
    [(cons x xs) (+ 1 (length xs))]))

(def (list-ref lst i)
  (match i
    [0 (first lst)]
    [i (list-ref (rest lst) (- i 1))]))

(def (take n lst)
  (match n
    [0 (empty)]
    [i (match lst
         [(empty) (error "take: ran out of elements")]
         [(cons x xs) (cons x (take (- n 1) xs))])]))

(def (drop n lst)
  (match n
    [0 lst]
    [n (match lst
         [(empty) (error "drop: ran out of elements")]
         [(cons x xs) (drop (- n 1) xs)])]))

(def (map f lst)
  (match lst
    [(empty) (empty)]
    [(cons x xs) (cons (f x)
                       (map f xs))]))

(def (filter f lst)
  (match lst
    [(empty) (empty)]
    [(cons x xs) (match (f x)
                   [#true (cons x (filter f xs))]
                   [#false (filter f xs)])]))

(def (reverse lst)
  (revappend lst (empty)))

(def (revappend lst onto)
  (match lst
    [(empty) onto]
    [(cons x xs) (revappend xs (cons x onto))]))

(def (andmap f lst)
  (match lst
    [(empty) #true]
    [(cons x xs) (match (f x)
                   [#false #false]
                   [#true (andmap f xs)])]))

(def (append lst onto)
  (match lst
    [(empty) onto]
    [(cons x xs) (cons x (append xs onto))]))

(def (contains? lst item)
  (match lst
    [(empty) #false]
    [(cons x xs) (match (equal? item x)
                   [#true #true]
                   [#false (contains? xs item)])]))

(def (set-add base item)
  (match (contains? base item)
    [#true base]
    [#false (cons item base)]))

(def (set-union base more)
  (match more
    [(empty) base]
    [(cons x xs) (set-union (set-add base x)
                            xs)]))
