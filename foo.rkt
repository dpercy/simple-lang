#lang racket


; value:
; number
; procedure (prim)
(struct Lambda (params body) #:transparent)
(struct Closure (func args) #:transparent)

; expr:
; number
; symbol
(struct App (f x) #:transparent)

; continuation
; 'done
(struct ArgK (f args env k) #:transparent)

; states
(struct StateInterp (expr args env k) #:transparent)
(struct StateApply (f args k) #:transparent)
(struct StateDone (v) #:transparent)

(define top-env
  (hash 'add (lambda (x y) (+ x y))
        'double (Lambda '(x) (App (App 'add 'x) 'x))
        'twice (Lambda '(f x) (App 'f (App 'f 'x)))
        ))


#|

conceptually, interp, apply*, and ret all tail-call each other.
but actually, they are trampolined:
they return a State telling you which one to call next.

step takes a state and gives you a new state.
it could be a done state.
|#
(define (step state)
  (match state
    [(StateInterp expr args env k) (interp expr args env k)]
    [(StateApply f args k) (apply* f args k)]))
(define (interp expr args env k)
  (match expr
    [(? number? n) (StateApply n args k)]
    [(? symbol? x) (StateApply (hash-ref env x) args k)]
    [(App f x) (StateInterp x '() env
                            (ArgK f args env k)
                            )]))
(define (apply* f args k)
  (match args
    ['() (ret k f)]
    [_
     (match f
       [(Closure cl-f cl-args) (StateApply cl-f (append cl-args args) k)]
       [_ #:when (> (arity f) (length args))
          (ret k (Closure f args))]
       [(? procedure?) (ret k (apply f args))]
       [(Lambda params body) (StateInterp body
                                          (drop args (length params))
                                          (for/fold ([h top-env]) ([p params] [a args])
                                            (hash-set h p a))
                                          k)])]))
(define (ret k v)
  (match k
    ['done (StateDone v)]
    [(ArgK f args env k) (StateInterp f (cons v args) env k)]))
(define (arity f)
  (match f
    [(? procedure?) (procedure-arity f)]
    [(Lambda params _) (length params)]))


(define (run expr)
  (run* (StateInterp expr '() top-env 'done)))
(define (run* state)
  (match (step state)
    [(StateDone v) v]
    [state (run* state)]))


(module+ test
  (require rackunit)

  (check-equal? (run (App (App 'add 23) 1)) 24)
  (check-equal? (run (App 'double 23)) 46)
  (check-equal? (run (App (App 'twice (App 'add 3)) 2)) 8)
  )
