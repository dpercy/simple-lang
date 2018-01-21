#lang racket

(require (only-in "compiler.sl" compile-program))


(module+ main

  ; skip over the "#lang ..." line
  (define hash-lang "#lang ")
  (when (equal? hash-lang (peek-string (string-length hash-lang) 0))
    (void (read-line)))

  ; read compile and print the rest of the input
  (void (write-string (compile-program (port->string)))))
