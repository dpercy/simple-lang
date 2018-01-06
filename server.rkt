#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/http/redirect
         )
(require net/rfc6455)
(require json)
(module+ test (require rackunit))

(require "core-syntax.rkt")
(require "surface-syntax.rkt")
(require "semantics.rkt")
(require "concurrent-runner.rkt")

(define (run-eval-server!)
  (define shutdown-ws-server! (run-ws-server!))
  (serve/servlet (lambda (request)
                   (redirect-to "/ui/notebook.html" temporarily))
                 #:servlet-path "/"
                 #:launch-browser? #false
                 #:extra-files-paths (list ".")
                 )
  (shutdown-ws-server!))

(module+ main (run-eval-server!))


(define (run-ws-server!)
  (ws-serve
   #:port 8001
   (lambda (conn req)
     #|

     Each new connection handler is like its own little eval server.
     The initial request contents aren't important.

     Each client message is an entire source program.
     This kicks off the computation on the server.
     As soon as each Result is computed, the server sends it to the client.

     |#

     (define t (thread (lambda () (void))))
     (for ([msg (in-producer ws-recv eof-object? conn)])
       (kill-thread t)
       (thread-wait t)
       (set! t (thread
                (lambda ()
                  ; TODO update the client to accumulate results properly
                  (define results '())
                  (ws-send! conn (format-results results))
                  (for ([r (run! msg)])
                    (set! results (cons r results))
                    (ws-send! conn (format-results results))))))))))

(define (format-results results)
  (jsexpr->string
   (for/list ([r results])
     (match r
       [(Result endline name value) (hash 'endline endline
                                          'text (~v value))]))))

(define/contract (run! program-string) (-> string? (sequence/c Result?))
  (define program-sexprs (read-all-string program-string))
  (define program-stmts (parse-program program-sexprs))
  (define program-blocks (eval-program program-stmts))
  (run-program/concurrent program-blocks globals))

(define prims (list
               +
               -
               <
               =
               *
               ;;
               ))
(define globals (for/hash ([prim prims])
                  (values (object-name prim) prim)))

(define (read-all-string str)
  (with-input-from-string str
    (lambda ()
      (port-count-lines! (current-input-port))
      (sequence->list (in-producer read-syntax eof-object?)))))
(module+ test
  (check-equal? (read-all-string "() 1 a")
                '(() 1 a)))
