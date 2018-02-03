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
    [(cons x xs) (int.+ 1 (length xs))]))

(def (list-ref lst i)
  (match i
    [0 (first lst)]
    [i (list-ref (rest lst) (int.- i 1))]))

(def (take n lst)
  (match n
    [0 (empty)]
    [i (match lst
         [(empty) (error "take: ran out of elements")]
         [(cons x xs) (cons x (take (int.- n 1) xs))])]))

(def (drop n lst)
  (match n
    [0 lst]
    [n (match lst
         [(empty) (error "drop: ran out of elements")]
         [(cons x xs) (drop (int.- n 1) xs)])]))

(def (map f lst)
  (match lst
    [(empty) (empty)]
    [(cons x xs) (cons (f x)
                       (map f xs))]))

(def (filter f lst)
  (match lst
    [(empty) (empty)]
    [(cons x xs) (if (f x)
                     (cons x (filter f xs))
                     (filter f xs))]))

(def (reverse lst)
  (revappend lst (empty)))

(def (revappend lst onto)
  (match lst
    [(empty) onto]
    [(cons x xs) (revappend xs (cons x onto))]))

(def (andmap f lst)
  (match lst
    [(empty) #true]
    [(cons x xs) (if (f x) ; TODO use and
                     (andmap f xs)
                     #false)]))

(def (append lst onto)
  (match lst
    [(empty) onto]
    [(cons x xs) (cons x (append xs onto))]))

(def (contains? lst item)
  (match lst
    [(empty) #false]
    [(cons x xs) (if (primitives.equal? item x)
                     #true
                     (contains? xs item))]))

(def (set-add base item)
  (if (contains? base item)
      base
      (cons item base)))

(def (set-union base more)
  (match more
    [(empty) base]
    [(cons x xs) (set-union (set-add base x)
                            xs)]))

(def (foldr f init lst)
  (match lst
    [(empty) init]
    [(cons x xs) (f x (foldr f init xs))]))
