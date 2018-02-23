#lang racket

(provide (rename-out [sl:#%module-begin #%module-begin]
                     [sl:#%app #%app]
                     )
         #%datum
         )

(require (for-syntax syntax/parse))

(define-syntax sl:#%module-begin
  (syntax-parser
    #:datum-literals (|;|)
    [(_
      ; Grab "fragments" (consecutive readable non-semicolons),
      ; separated by semicolons.
      ; Parse each fragment as a statement.
      |;| ...
      (~seq (~seq (~and (~not |;|) fragment) ...)
            |;| ...)
      ...)

     #'(#%module-begin
        (statement fragment ...) ...)]))

(define-syntax statement
  (syntax-parser
    #:datum-literals (= struct)
    [(_ x:id = expr)  #'(define x expr)]
    [(_ f:id x:id ... = expr)  #'(define (f x ...) expr)]
    [(_ expr) #'(#%expression expr)]))


(define-syntax sl:#%app
  (syntax-parser
    #:datum-literals (|;|)
    ; Filter out all semicolons; they have no significance inside parens.
    [(_ |;| ...
        (~seq (~and (~not |;|) expr)
              |;| ...)
        ...)
     #'(curried-app expr ...)]))
(define-syntax curried-app
  (syntax-parser
    ; Unary parens are fine: they're just grouping.
    [(_ x) #'x]
    ; One or more arguments is curried application.
    [(_ f x0 xs ...) #'(curry f x0 xs ...)]))
