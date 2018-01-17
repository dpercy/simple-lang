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

     (define shutdown-previous-computation! (lambda () (void)))

     (for ([msg (in-producer ws-recv eof-object? conn)])
       (shutdown-previous-computation!)

       (define cust (make-custodian))
       (define t (parameterize ([current-custodian cust])
                   (thread
                    (lambda ()
                      (define results '())
                      (ws-send! conn (format-results results))
                      (for ([r (run! msg)])
                        (set! results (cons r results))
                        (ws-send! conn (format-results results)))))))
       (set! shutdown-previous-computation!
             (lambda ()
               (custodian-shutdown-all cust)
               (thread-wait t)))))))

(define (format-results results)
  (jsexpr->string
   (for/list ([r results])
     (match r
       [(ResultValue endline name value) (hash 'endline endline
                                               'name (if name
                                                         (symbol->string name)
                                                         'null)
                                               'valueText (~v value))]
       [(ResultError endline name msg) (hash 'endline endline
                                             'name (if name
                                                       (symbol->string name)
                                                       'null)
                                             'errorText msg)]))))

(define/contract (run! program-string) (-> string? (sequence/c Result?))
  (define program-sexprs (read-all-string program-string))
  (define program-stmts (parse-program program-sexprs))
  (define program-blocks (eval-program program-stmts))
  (run-program/sequential program-blocks globals))

(define prims (list
               ; numbers
               +
               -
               <
               =
               *
               /
               ; strings
               string-split
               substring
               string->number
               string-length
               ; generic
               equal?

               ; functions
               curry
               (procedure-rename curryr 'curryr)

               ;;
               ))
(define globals (for/hash ([prim prims])
                  (values (object-name prim) prim)))

(define (read-all-string str)
  (apply
   append
   (for/list ([c (pad-chunks (split-chunks str))])

     (read-all-string/norecover c))))

(define (split-chunks str)
  ; Chunks are delimited by a line with non-whitespace in column 1.
  (regexp-split #rx"\n(?=[^ \t])" str))
(module+ test
  (check-equal? (split-chunks "x")
                '("x"))
  (check-equal? (split-chunks "x\ny\nz")
                '("x" "y" "z"))
  (check-equal? (split-chunks "x\n  y\nz")
                '("x\n  y" "z"))
  (check-equal? (split-chunks "x\n  y\n\n\nz")
                '("x\n  y" "" "" "z")))

(define (pad-chunks chunks)
  (define lines 0)
  (for/list ([c chunks])
    (define c* (string-append (apply string-append (make-list lines "\n"))
                              c))
    (set! lines (+ lines
                   (let ([newlines (or (regexp-match* "\n" c)
                                       '())])
                     (+ 1 (length newlines)))))
    c*))
(module+ test
  (check-equal? (pad-chunks '("x"))
                '("x"))
  (check-equal? (pad-chunks '("x" "y"))
                '("x" "\ny"))
  (check-equal? (pad-chunks '("x\n  y" "z"))
                '("x\n  y" "\n\nz"))
  (check-equal? (pad-chunks '("x\n  y" "" "" "z"))
                '("x\n  y" "\n\n" "\n\n\n" "\n\n\n\nz"))


  (check-equal? (pad-chunks (split-chunks "x\n a\n b\n c\ny"))
                '("x\n a\n b\n c" "\n\n\n\ny")))


(define (read-all-string/norecover str)
  (with-input-from-string str
    (lambda ()
      (port-count-lines! (current-input-port))
      (sequence->list (in-producer read-syntax/safe eof-object?)))))
(define (read-syntax/safe)
  (with-handlers ([exn:fail:read? (lambda (exn)
                                    (match-define
                                      (exn:fail:read _ _
                                                     (list
                                                      (srcloc file line col pos span)))
                                      exn)
                                    (datum->syntax #f
                                                   (list '#:error (exn-message exn))
                                                   (list file line col pos span)))])
    (read-syntax)))
(module+ test
  ; simple case
  (check-equal? (map syntax->datum (read-all-string "() 1 a"))
                '(() 1 a))
  ; ignore bad input at end
  (check-match (map syntax->datum (read-all-string "() 1 a ("))
               (list '()
                     1
                     'a
                     _))
  ; ignore bad input in middle
  ; (the recovery point is ^[^ ])
  (check-match (map syntax->datum (read-all-string "() 1 a (\n  q\nz"))
               (list '()
                     1
                     'a
                     _
                     'z)))
