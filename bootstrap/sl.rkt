#lang racket

#|

This Racket module implements #lang s-expr "sl.rkt",
which you can use to run SL programs on the Racket runtime.

Goal: concicse bootstrapping

Anti-Goal: integration with Racket ecosystem

Non-Goal: correctness for programs other than compiler.sl

|#

(require racket/provide)
(require racket/splicing)
(require racket/fixnum)
(require (only-in racket/syntax format-symbol))
(require (for-syntax racket/match
                     racket/string))

(provide #%app
         #%top-interaction
         (rename-out [sl:#%module-begin #%module-begin]
                     [sl:struct struct]
                     [sl:def def]
                     [sl:error error]
                     [sl:#%datum #%datum]
                     [sl:match match]
                     ; syntax sugar for repeated cons and empty constructors
                     [sl:list list]
                     ; global variable handler - for imports
                     [sl:#%top #%top]
                     ; if: sugar for match #true #false
                     [sl:if if]
                     ; short-circuiting and, or: sugar for repeated sl:if
                     [sl:and and]
                     [sl:or or]
                     )

         ;;
         )

(define (sl-print value port)
  (match value
    [(or (? string?)
         (? boolean?)
         (? number?))  (write value port)]
    [(app struct->vector (vector _ args ...))
     (begin
       (write-string "(" port)
       (write (object-name value) port)
       (for ([a args])
         (write-string " " port)
         (sl-print a port))
       (write-string ")" port))]))
(module+ util
  (provide sl-print))

(define-syntax-rule (sl:#%module-begin forms ...)
  (#%module-begin
   (global-port-print-handler sl-print)
   (print-boolean-long-form #true)
   forms ...))

(define-syntax (sl:#%top stx)
  (syntax-case stx ()
    [(_ . id) (identifier? #'id)

     (match (string-split (symbol->string (syntax-e #'id))
                          ".")
       [(list unqualified)  #'(#%top . id)]
       [(list prefix suffix) (with-syntax ([mod (match prefix
                                                  ["primitives" "primitives.rkt"]
                                                  [_ (string-append prefix ".sl")])]
                                           [suffix (string->symbol suffix)])
                               #'(let ()
                                   (local-require (only-in mod suffix))
                                   suffix))]
       [_ #'(#%top . id)])]))

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
    [(_ . i) (exact-integer? (syntax-e #'i)) #'(quote i)]
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
    [(_ (cname args ...))
     (app struct->vector
          (vector (== (format-symbol "struct:~a" (object-name cname)))
                  (sl:pat args) ...))]
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

(define-syntax-rule (sl:if test consq alt)
  (sl:match test
            [#true consq]
            [#false alt]))
(define-syntax sl:and
  (syntax-rules ()
    [(_) #true]
    [(_ x xs ...) (sl:if x
                         (sl:and xs ...)
                         #false)]))
(define-syntax sl:or
  (syntax-rules ()
    [(_) #false]
    [(_ x xs ...) (sl:if x
                         #true
                         (sl:or xs ...))]))
