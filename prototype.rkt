#lang racket
(require racket/generator)
(require (prefix-in racket: racket))
(module+ test (require rackunit))
(require "core-syntax.rkt")
(require "surface-syntax.rkt")


(define-syntax match
  (syntax-rules ()
    [(_ scrutinee cases ...)
     (let ([tmp scrutinee])
       (racket:match tmp
                     cases ...
                     [_ (error 'no-case
                               "No case for ~v => ~v"
                               #'scrutinee
                               tmp)]))]))


#|

Goal: produce an executable implementation for the notebook UI.
- when you "run" an expression you get a Racket computation, which can
.   - halt with a value
.   - diverge
.   - raise an error
- when you evaluate a program you get a set of thunks
.   - some thunks might depend on each other
.   - some thunks might throw or diverge
.   - the caller can use futures to force thunks carefully

|#




(module+ test

  (define prog-src
    '[
      (struct Zero 0)
      (struct Succ 1)
      (def (plus x y)
        (match x
          [(Zero) y]
          [(Succ xx) (Succ (plus xx y))]))
      (def one (Succ (Zero)))
      (def two (Succ one))
      (def irrelevant (Succ (Zero)))
      (def irrelevant-fail (plus Succ Succ))
      (plus two one)
      ;
      ])
  (define prog (map parse prog-src))
  (check-equal? prog
                (list
                 (DefStruct 'Zero 0)
                 (DefStruct 'Succ 1)
                 (DefFun 'plus '(x y)
                   (Match
                    (Local 'x)
                    (list
                     (Case (PatCtor 'Zero '()) (Local 'y))
                     (Case (PatCtor 'Succ (list (PatHole 'xx)))
                           (Call (Global 'Succ)
                                 (list (Call (Global 'plus)
                                             (list (Local 'xx)
                                                   (Local 'y)))))))))
                 (DefVal 'one (Call (Global 'Succ)
                                    (list (Call (Global 'Zero) '()))))
                 (DefVal 'two (Call (Global 'Succ)
                                    (list (Global 'one))))
                 (DefVal 'irrelevant (Call (Global 'Succ)
                                           (list (Call (Global 'Zero) '()))))
                 (DefVal 'irrelevant-fail (Call (Global 'plus)
                                                (list (Global 'Succ)
                                                      (Global 'Succ))))
                 (Call (Global 'plus)
                       (list (Global 'two) (Global 'one)))))

  (check-equal? (map render prog)
                prog-src)

  ;;
  )

(struct Process (name thunk) #:transparent)
(define runnable-statement? (or/c DefFun? DefStruct? Process?))
(define runnable-program? (listof runnable-statement?))
(define/contract (eval-program stmts) (-> (listof Stmt?) runnable-program?)
  (define defs (for/hash ([s stmts]
                          #:when (Def? s))
                 (values (Def-name s) s)))
  (define program
    (for/list ([s stmts])
      ; TODO pass in only the relevant defs
      (eval-stmt s defs)))
  program)

(define (memoize f #:hash h)
  (procedure-rename
   (lambda args
     (hash-ref! h args (lambda () (apply f args))))
   (string->symbol (format "memoized:~s" (object-name f)))))

(define current-eval-stmt (make-parameter #f))
(define/contract eval-stmt (-> Stmt? (hash/c symbol? Stmt?) runnable-statement?)
  (memoize
   ; TODO manage the size of this memo table
   #:hash (make-hash)
   (lambda (stmt relevant-defs)
     (match stmt
       [(? DefFun?) stmt]
       [(? DefStruct?) stmt]
       [(DefVal name expr)  (Process name
                                     (lazy
                                      (delay/sync
                                       (run-expr expr (hash) relevant-defs))))]
       [(? Expr? expr)  (Process #false
                                 (lazy
                                  (delay/sync
                                   (run-expr expr (hash) relevant-defs))))]))))


(define/contract (run-expr expr locals defs) (-> Expr?
                                                 (hash/c symbol? Expr?)
                                                 (hash/c symbol? Def?)
                                                 any/c)
  (match expr
    [(Quote _) expr]
    [(Error msg) (error msg)]
    ; locals map to a value
    [(Local name) (hash-ref locals name)]
    ; globals can map to:
    ; - a running DefVal  (a Process)
    ; - a static DefStruct
    ; - a static DefFun
    [(Global name) (match (hash-ref defs name)
                     [(DefStruct _ _) expr]
                     [(DefFun _ _ _) expr]
                     [(? DefVal? stmt)
                      (match (eval-stmt stmt defs)
                        [(Process _ thunk) (force thunk)])])]
    [(Call func args) (match (run-expr* (cons func args) locals defs)
                        [(cons (Global name) args)
                         (match (hash-ref defs name)
                           [(DefStruct _ arity) #:when (= arity (length args))
                            (Call (Global name) args)]
                           [(DefFun name params body)
                            (parameterize ([current-eval-stmt name])
                              (run-expr body
                                        (for/fold ([locals locals]) ([p params]
                                                                     [a args])
                                          (hash-set locals p a))
                                        defs))])])]
    [(Match scrutinee cases) (let ([value (run-expr scrutinee locals defs)])
                               (apply-cases value cases locals defs))]))
(define (run-expr* exprs locals defs)
  (for/list ([e exprs])
    (run-expr e locals defs)))
(define (apply-cases value cases locals defs)
  (match cases
    ['() (error 'pattern-matching "~s: no case for: ~v" (current-eval-stmt) value)]
    [(cons (Case pat rhs) cases)
     (match (try-destructure pat value)
       [#false (apply-cases value cases locals defs)]
       [new-locals (run-expr rhs
                             (hash-append new-locals locals)
                             defs)])]))
(define (try-destructure pat value)
  (match pat
    [(PatLitr v) (if (equal? (Quote v) value)
                     (hash)
                     #false)]
    [(PatHole name) (hash name value)]
    [(PatCtor cname argpats)
     (match value
       [(Call (Global (== cname)) args) #:when (= (length args)
                                                  (length argpats))
        (try-destructure* argpats args)]
       [_ #false])]))
(define (try-destructure* pats vals)
  (match* {pats vals}
    [{'() '()} (hash)]
    [{(cons pat0 pats)
      (cons val0 vals)} (match (try-destructure pat0 val0)
                          [#false #false]
                          [h1 (match (try-destructure* pats vals)
                                [#false #false]
                                [h2 (hash-append h1 h2)])])]))
(define (hash-append h1 h2)
  (for/fold ([h2 h2])
            ([{k v} (in-hash h1)])
    (hash-set h2 k v)))

(define (quote->value q)
  (match q
    [(Call (Global 'Cons) (list x y)) (cons (quote->value x)
                                            (quote->value y))]
    [(Call (Global 'Empty) '()) '()]
    [(Quote v) v]))
(define (value->quote q)
  (match q
    [(cons x y) (Call (Global 'Cons) (list (value->quote x)
                                           (value->quote y)))]
    ['() (Call (Global 'Empty) '())]
    [(? string? s) (Quote s)]
    [(? number? n) (Quote n)]
    [(? boolean? b) (Quote b)]))

(define (render-running term)
  (match term
    [(Process (? name? name) thunk) `(def ,name ,(render-running (Process #false thunk)))]
    [(Process #false thunk)
     (cond
       [(promise-forced? thunk) (match (with-handlers ([exn:fail? (lambda (e) e)])
                                         (force thunk))
                                  [(? exn:fail? exn) `(#:fail
                                                       ,(exn-message exn))]
                                  [(? Expr? expr) (render expr)])]
       [(promise-running? thunk) '#:running]
       [else '#:not-started])]
    [_ (render term)]))

(module+ test

  (define runnable-prog (eval-program prog))


  (check-equal? (map render-running runnable-prog)
                '[
                  (struct Zero 0)
                  (struct Succ 1)
                  (def (plus x y)
                    (match x
                      [(Zero) y]
                      [(Succ xx) (Succ (plus xx y))]))
                  (def one #:not-started)
                  (def two #:not-started)
                  (def irrelevant #:not-started)
                  (def irrelevant-fail #:not-started)
                  #:not-started
                  ;
                  ])

  ; force the last part to run
  (check-equal?
   (force (Process-thunk (last runnable-prog)))
   (Call (Global 'Succ)
         (list
          (Call (Global 'Succ)
                (list
                 (Call (Global 'Succ)
                       (list
                        (Call (Global 'Zero) '()))))))))

  (check-equal? (map render-running runnable-prog)
                '[
                  (struct Zero 0)
                  (struct Succ 1)
                  (def (plus x y)
                    (match x
                      [(Zero) y]
                      [(Succ xx) (Succ (plus xx y))]))
                  (def one (Succ (Zero)))
                  (def two (Succ (Succ (Zero))))
                  (def irrelevant #:not-started)
                  (def irrelevant-fail #:not-started)
                  (Succ (Succ (Succ (Zero))))
                  ;
                  ])

  (start-program! runnable-prog)
  ; Even with a sleep this small, we can't observe a #:running state.
  ; This will have to wait until programs are longer.
  (sleep 0.00000000001)
  (check-equal? (map render-running runnable-prog)
                '[
                  (struct Zero 0)
                  (struct Succ 1)
                  (def (plus x y)
                    (match x
                      [(Zero) y]
                      [(Succ xx) (Succ (plus xx y))]))
                  (def one (Succ (Zero)))
                  (def two (Succ (Succ (Zero))))
                  (def irrelevant (Succ (Zero)))
                  (def irrelevant-fail (#:fail "pattern-matching: plus: no case for: (Global 'Succ)"))
                  (Succ (Succ (Succ (Zero))))
                  ;
                  ])


  ;;
  )

(define/contract (start-program! prog) (-> runnable-program? (listof thread?))
  ; start a background thread for each Process in prog
  (for/list ([s prog]
             #:when (Process? s))
    (start-process! s)))

(define/contract start-process! (-> Process? thread?)
  (memoize
   ; TODO need ephemerons here?
   #:hash (make-weak-hasheq)
   (lambda (s)
     (match s
       [(Process _ thunk)  (thread (lambda ()
                                     (force thunk)))]))))

(define (program-done? prog)
  (for/and ([p (in-list prog)]
            #:when (Process? p))
    (match p
      [(Process _ thunk) (promise-forced? thunk)])))

; TODO instead of sleeping, get the thunks to notify when they complete
(define/contract (in-rendered-evaluation stmts #:sleep [sleep-seconds 0.1]) (-> (listof Stmt?) (sequence/c any/c))
  (define prog (eval-program stmts))
  (start-program! prog)
  (in-generator
   (let loop ()
     ; check done before yielding:
     ; you want to make sure to yield the final value.
     (if (program-done? prog)
         (yield (map render-running prog))
         (begin
           (yield (map render-running prog))
           (sleep sleep-seconds)
           (loop))))))

(define (read-all-string str)
  (with-input-from-string str
    (lambda ()
      (sequence->list (in-producer read eof-object?)))))

(define (show-all-string sexprs)
  (with-output-to-string
    (lambda ()
      (for ([s sexprs])
        (println s (current-output-port) 1)))))

(module+ main
  (require web-server/servlet
           web-server/servlet-env
           web-server/http/redirect
           )
  (require net/rfc6455)

  (define (safe-parse sexpr)
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (Error (exn-message exn)))])
      (parse sexpr)))

  (render-abbreviate-defun #true)

  ; start a websocket server:
  ; - async, returns a close! function
  (define shutdown-websocket-server!
    (ws-serve
     #:port 8001
     (lambda (conn req)
       ; There's always one thread computing something.
       (define replier #false)
       (for ([msg (in-producer ws-recv eof-object? conn)])
         (when replier
           (kill-thread replier)
           (thread-wait replier))
         (set! replier (thread (lambda ()
                                 ; The replier thread replies with a sequence of responses
                                 ; to the one request.
                                 (match (with-handlers ([exn:fail?
                                                         (lambda (exn)
                                                           (ws-send! conn (~v exn))
                                                           #false)])
                                          (define sexpr (read-all-string msg))
                                          (map safe-parse sexpr))
                                   [#false (void)]
                                   [stmts
                                    (for ([result (in-rendered-evaluation stmts)])
                                      (ws-send! conn (show-all-string result))
                                      (sleep 1))]))))))))

  ; start a normal web server
  ; - blocks until killed
  (serve/servlet (lambda (request)
                   (redirect-to "/ui/notebook.html" temporarily))
                 #:servlet-path "/"
                 #:launch-browser? #false
                 #:extra-files-paths (list ".")
                 )

  (shutdown-websocket-server!)




  ;;
  )
