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
                     [sl:def def]
                     [sl:error error]
                     [sl:#%datum #%datum])

         ; TODO figure out how to make this more portable?
         ;  don't want to accidentally depend on Racket libraries.
         (rename-out [require import])

         ; efficient special cases for certain constructors
         (rename-out [sl:true true]
                     [sl:false false]
                     [sl:empty empty]
                     [sl:cons cons]
                     [sl:list list]
                     [sl:Z Z]
                     [sl:S S])
         ; primitives for dealing with strings
         string?
         string=?
         string-append
         string-length
         (rename-out [sl:substring substring])
         ord
         chr



         ;;
         )

(define-syntax-rule (sl:struct (cname args ...))
  (struct cname (args ...) #:prefab))

(define-syntax sl:def
  (syntax-rules ()
    [(_ (name params ...) body)  (begin
                                   (provide name)
                                   (define (name params ...) body))]
    [(_ name expr)  (begin
                      (provide name)
                      (define name expr))]))

(define-syntax (sl:error stx)
  (syntax-case stx ()
    [(_ v) (string? (syntax-e #'v)) #'(error v)]))

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
(define-match-expander sl:list
  (syntax-rules () [(_ args ...) (list args ...)])
  (syntax-rules () [(_ args ...) (list args ...)]))
; - nats
(define-match-expander sl:Z
  (syntax-rules () [(_) 0])
  (syntax-rules () [(_) 0]))
(define-match-expander sl:S
  (syntax-rules () [(_ n) (? exact-positive-integer?
                             (app sub1 n))])
  (syntax-rules () [(_ n) (add1 n)]))

(define (ord s)
  (match (string->list s)
    [(list c) (char->integer c)]))
(define (chr i)
  (list->string (list (integer->char i))))

(define (sl:substring s start end)
  (substring s start end))
