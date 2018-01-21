#lang racket

#|

This Racket module implements #lang s-expr "sl.rkt",
which you can use to run SL programs on the Racket runtime.

Goal: concicse bootstrapping

Anti-Goal: integration with Racket ecosystem

|#

(require racket/provide)
(require racket/splicing)
(require racket/fixnum)

(provide #%module-begin
         #%app
         #%top-interaction
         (rename-out [sl:struct struct]
                     [sl:def def]
                     [sl:error error]
                     [sl:#%datum #%datum]
                     [sl:match match]
                     ; syntax sugar for repeated cons and empty constructors
                     [sl:list list])

         ; TODO figure out how to make this more portable?
         ;  don't want to accidentally depend on Racket libraries.
         (rename-out [require import])

         ; primitives for dealing with booleans.
         boolean?

         ; primitives for dealing with integers.
         ; these all fail if the result is not a fixnum.
         ; TODO what if different platforms have different fixnum sizes?
         ;   - then a cross-compiler might constant-fold incorrectly
         (rename-out [fixnum? int?]
                     [fx+ +]
                     [fx- -]
                     [fx* *]
                     [fxquotient /]
                     [fx< <]
                     [fx= =])

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



         ;;
         )

(define-syntax-rule (sl:struct (cname args ...))
  (begin
    (provide cname)
    (struct cname (args ...) #:prefab)))

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
    [(_ . i) (exact-nonnegative-integer? (syntax-e #'i)) #'(quote i)]
    [(_ . b) (boolean? (syntax-e #'b)) #'(quote b)]))


; Racket's match has special cases for certain identifiers,
; like empty, cons, list... and others, such that
; even if a prefab struct is defined, it will prefer the
; built-in definitions of empty, cons, list.
; So to get around this, we can insert a wrapper such that
; (wrap (cons x xs)) means (struct cons (x xs)).
(define-syntax-rule (sl:match scrut [pat expr] ...)
  (match scrut
    [(sl:pat pat) expr] ...))

(define-match-expander sl:pat
  (syntax-rules (sl:list)
    [(_ (sl:list args ...))   (sl:list (sl:pat args) ...)]
    [(_ (cname args ...))  (struct cname [(sl:pat args) ...])]
    [(_ pat) pat]))

(splicing-local [(struct empty () #:prefab)
                 (struct cons (head tail) #:prefab)]
  (define-match-expander sl:list
    (syntax-rules ()
      [(_)  (struct empty ())]
      [(_ x xs ...) (struct cons [x (sl:list xs ...)])])
    (syntax-rules ()
      [(_)  (empty)]
      [(_ x xs ...) (cons x (sl:list xs ...))])))

(define (ord s)
  (match (string->list s)
    [(list c) (char->integer c)]))
(define (chr i)
  (list->string (list (integer->char i))))

(define (sl:substring s start end)
  (substring s start end))
