#lang racket

(require (only-in "compiler.sl" compile-program))


(module+ main

  (write-string (compile-program (port->string)))

  )
