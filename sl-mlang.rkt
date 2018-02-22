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
      ; Any number of semicolons is fine to start
      |;| ...
      ; Then we've got a sequence of top-stmts.
      (~seq expr
            ; Each expr must be followed by one semicolon.
            |;|
            ; And then extras are fine too.
            |;| ...)
      ...)

     #'(#%module-begin
        expr ...)]))
