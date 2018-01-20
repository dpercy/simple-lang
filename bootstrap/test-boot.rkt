#lang racket

(require (only-in "compiler.sl" compile-program))


(module+ main

  (void (write-string (compile-program (port->string))))

  )
