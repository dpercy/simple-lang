#lang racket
(require (only-in racket/exn exn->string))
(require racket/generator)
(module+ test (require rackunit))

(module helpers racket/base
  (provide (all-defined-out))
  (require (prefix-in racket: racket))

  (define-syntax match
    (syntax-rules ()
      [(_ scrutinee cases ...)
       (let ([tmp scrutinee])
         (racket:match tmp
                       cases ...
                       [_ (error 'no-case
                                 "No case for ~v => ~v"
                                 #'scrutinee
                                 tmp)]))])))
(require 'helpers)

#|

syntax:

<stmt> := <expr> | <def>

<def> :=
| (def <id> <expr>)
| (def (<id> <id> ...) <expr>)
| (struct (<id> <id> ...))

<expr> :=
| <global-ref>
| <param-ref>
| (<expr> <expr> ...)
| (match <expr> [<pat> <expr>] ...)

<pat> :=
| <id>
| (<constructor> <pat> ...)
|#

(struct Stmt () #:transparent)
(struct Def Stmt (name) #:transparent)
(struct DefVal Def (expr) #:transparent)
(struct DefFun Def (params body) #:transparent)
(struct DefStruct Def (arity) #:transparent)

(struct Expr Stmt () #:transparent)
(struct Local Expr (name) #:transparent)
(struct Global Expr (name) #:transparent)
(struct Call Expr (func args) #:transparent)


(struct Match Expr (scrutinee cases) #:transparent)

(struct Case (pat expr) #:transparent)

(struct Pat () #:transparent)
(struct PatHole (name) #:transparent)
(struct PatCtor (name args) #:transparent)

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

(define kw? (or/c 'def 'struct 'match))
(define name? (and/c symbol? (not/c kw?)))
(define (parse sexpr) ; -> stmt
  (match sexpr
    [`(def ,(? name? name) ,v) (DefVal name (parse-expr v (set)))]
    [`(def (,(? name? name) ,(? name? params) ...) ,body)
     (DefFun name params (parse-expr body (list->set params)))]
    [`(struct ,(? name? name) ,(? number? arity))
     (DefStruct name arity)]
    [_ (parse-expr sexpr (set))]))
(define (parse-expr sexpr locals)
  (match sexpr
    [(? name? name) (if (set-member? locals name)
                        (Local name)
                        (Global name))]
    [(cons (? (not/c kw?) func) args)
     (Call (parse-expr func locals)
           (for/list ([a args])
             (parse-expr a locals)))]
    [`(match ,scrutinee ,cases ...)
     (Match (parse-expr scrutinee locals)
            (for/list ([c cases])
              (parse-case c locals)))]))
(define (parse-case case locals)
  (match case
    [(list lhs rhs) (let ([pat (parse-pat lhs)])
                      (Case pat
                            (parse-expr rhs
                                        (set-union (pat-holes pat)
                                                   locals))))]))
(define (parse-pat pat)
  (match pat
    [(? name? name) (PatHole name)]
    [(list* (? name? name) args) (PatCtor name (map parse-pat args))]))
(define (pat-holes pat)
  (match pat
    [(PatHole name) (set name)]
    [(PatCtor _ args) (foldr set-union (set) (map pat-holes args))]))

(define (render term)
  (define r render)
  (match term
    [(DefVal name val) `(def ,name ,(r val))]
    [(DefFun name ps b) `(def (,name ,@ps) ,(r b))]
    [(DefStruct name arity) `(struct ,name ,arity)]
    [(Local name) name]
    [(Global name) name]
    [(Call func args) (cons (r func) (map r args))]
    [(Match scr cases) `(match ,(r scr) ,@(map r cases))]
    [(Case pat expr) `[,(r pat) ,(r expr)]]
    [(PatHole name) name]
    [(PatCtor name args) `(,name ,@(map r args))]
    ; cheating a bit here by rendering a Process
    [(Process (? name? name) thunk) `(def ,name ,(r (Process #false thunk)))]
    [(Process #false thunk)
     (cond
       [(promise-forced? thunk) (match (with-handlers ([exn:fail? (lambda (e) e)])
                                         (force thunk))
                                  [(? exn:fail? exn) `(#:fail
                                                       ,(exn-message exn))]
                                  [(? Expr? expr) (render expr)])]
       [(promise-running? thunk) '#:running]
       [else '#:not-started])]))

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
(define runnable-globals? (hash/c symbol? runnable-statement?))
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

(define/contract eval-stmt (-> Stmt? (hash/c symbol? Stmt?) runnable-statement?)
  (memoize
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
                           [(DefFun _ params body)
                            (run-expr body
                                      (for/fold ([locals locals]) ([p params]
                                                                   [a args])
                                        (hash-set locals p a))
                                      defs)])])]
    [(Match scrutinee cases) (let ([value (run-expr scrutinee locals defs)])
                               (apply-cases value cases locals defs))]))
(define (run-expr* exprs locals defs)
  (for/list ([e exprs])
    (run-expr e locals defs)))
(define (apply-cases value cases locals defs)
  (match cases
    ['() (error 'pattern-matching "no case for: ~v" value)]
    [(cons (Case pat rhs) cases)
     (match (try-destructure pat value)
       [#false (apply-cases value cases locals defs)]
       [new-locals (run-expr rhs
                             (hash-append new-locals locals)
                             defs)])]))
(define (try-destructure pat value)
  (match pat
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

(module+ test

  (define runnable-prog (eval-program prog))


  (check-equal? (map render runnable-prog)
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

  (check-equal? (map render runnable-prog)
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
  (check-equal? (map render runnable-prog)
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
                  (def irrelevant-fail (#:fail "pattern-matching: no case for: (Global 'Succ)"))
                  (Succ (Succ (Succ (Zero))))
                  ;
                  ])


  ;;
  )

(define/contract (start-program! prog) (-> runnable-program? void?)
  ; start a background thread for each Process in prog
  (for ([s prog])
    (match s
      [(Process _ thunk)  (thread (lambda ()
                                    (force thunk)))]
      [_ (void)])))

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
         (yield (map render prog))
         (begin
           (yield (map render prog))
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
                                          (map parse sexpr))
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
