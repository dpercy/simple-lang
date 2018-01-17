#lang racket

#|

This Racket module implements #lang s-expr "sl.rkt",
which you can use to run SL programs on the Racket runtime.

This is intended to be a concise way to bootstrap.

|#

(require racket/provide)

(provide #%module-begin
         #%app
         match
         #%top-interaction
         (rename-out [sl:struct struct]
                     [define def]))

(define-syntax-rule (sl:struct (cname args ...))
  (struct cname (args ...) #:prefab))
