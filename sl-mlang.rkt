#lang racket

(provide (rename-out [sl:#%module-begin #%module-begin]

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
            |;|
            |;| ...)
      ...)

     #'(#%module-begin
        (statement fragment ...) ...)]))

(define-syntax statement
  (syntax-parser
    #:datum-literals (= struct)
    [(_ x:id = expr)  #'(define x expr)]
    [(_ expr) #'(#%expression expr)]))
