#lang racket

(provide

 ; primitives for dealing with booleans.
 boolean?

 ; primitives for dealing with integers.
 ; these all fail if the result is not a fixnum.
 ; TODO what if different platforms have different fixnum sizes?
 ;   - then a cross-compiler might constant-fold incorrectly
 (rename-out [exact-integer? int?]
             [+ +]
             [- -]
             [* *]
             [quotient /]
             [< <]
             [= =])

 ; primitives for dealing with strings:
 ; note these all depend on ints and booleans,
 ; but not lists.
 string?
 string=?
 string-append
 string-length
 (rename-out [sl:substring substring])
 ord
 chr

 ; fancy primitive for comparing *all*kinds*of*data*
 ; TODO should equal? be primitive?
 ; TODO should = and string=? be removed since we have equal?
 ;    - they could be like equal? wrapped in a contract,
 equal?

 ;;
 )

(define (ord s)
  (match (string->list s)
    [(list c) (char->integer c)]))

(define (chr i)
  (list->string (list (integer->char i))))

(define (sl:substring s start end)
  (substring s start end))
