#lang racket

#|

This Racket module implements #lang s-expr "sl.rkt",
which you can use to run SL programs on the Racket runtime.

Goal: concicse bootstrapping

Anti-Goal: integration with Racket ecosystem

|#

(require racket/provide)

(provide #%module-begin
         #%app
         match
         #%top-interaction
         (rename-out [sl:struct struct]
                     [define def]
                     [sl:error error]
                     [sl:#%datum #%datum])

         ; TODO figure out how modules should actually work.
         ; For now, provide textual include,
         ; so SL programs can be broken up without accidentally
         ; depending on Racket libraries.
         include

         ; efficient special cases for certain constructors
         (rename-out [sl:true true]
                     [sl:false false]
                     [sl:empty empty]
                     [sl:cons cons]
                     [sl:Z Z]
                     [sl:S S])

         ;;
         )

(define-syntax-rule (sl:struct (cname args ...))
  (struct cname (args ...) #:prefab))


(define-syntax (sl:error stx)
  (syntax-case stx ()
    [(_ . v) (string? (syntax-e #'v)) #'(error v)]))

(define-syntax (sl:#%datum stx)
  (syntax-case stx ()
    [(_ . v) (string? (syntax-e #'v)) #'(quote v)]
    [(_ . i) (exact-nonnegative-integer? (syntax-e #'i)) #'(quote i)]))


; smart constructors for "built-in" (but still struct-y) data types
; - booleans
(define-match-expander sl:true
  (syntax-rules () [(_) #true])
  (syntax-rules () [(_) #true]))
(define-match-expander sl:false
  (syntax-rules () [(_) #false])
  (syntax-rules () [(_) #false]))
; - lists
(define-match-expander sl:empty
  (syntax-rules () [(_) '()])
  (syntax-rules () [(_) '()]))
(define-match-expander sl:cons
  (syntax-rules () [(_ x xs) (list* x xs)])
  (syntax-rules () [(_ x xs) (list* x xs)]))
; - nats
(define-match-expander sl:Z
  (syntax-rules () [(_) 0])
  (syntax-rules () [(_) 0]))
(define-match-expander sl:S
  (syntax-rules () [(_ n) (? exact-positive-integer?
                             (app sub1 n))])
  (syntax-rules () [(_ n) (add1 n)]))
