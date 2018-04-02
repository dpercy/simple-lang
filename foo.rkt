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

(define top-env
  (hash 'add (lambda (x y) (+ x y))
        'double (Lambda '(x) (App (App 'add 'x) 'x))
        'twice (Lambda '(f x) (App 'f (App 'f 'x)))
        ))


#|

interp, apply*, and ret all tail-call each other.
could try this:
- make them each return a new "state" (like a trampoline)
- wrap them in a single state-transition function
- curry, to compile each expression to an action on the state???
.   - maybe not worth it if JS closures are expensive
.   - instead could translate ArgK as an object with a ret method
|#
(define (interp expr args env k)
  (match expr
    [(? number? n) (apply* n args k)]
    [(? symbol? x) (apply* (hash-ref env x) args k)]
    [(App f x) (interp x '() env
                       (ArgK f args env k)
                       )]))
(define (apply* f args k)
  (match args
    ['() (ret k f)]
    [_
     (match f
       [(Closure cl-f cl-args) (apply* cl-f (append cl-args args) k)]
       [_ #:when (> (arity f) (length args))
          (ret k (Closure f args))]
       [(? procedure?) (ret k (apply f args))]
       [(Lambda params body) (interp body
                                     (drop args (length params))
                                     (for/fold ([h top-env]) ([p params] [a args])
                                       (hash-set h p a))
                                     k)])]))
(define (ret k v)
  (match k
    ['done v]
    [(ArgK f args env k) (interp f (cons v args) env k)]))
(define (arity f)
  (match f
    [(? procedure?) (procedure-arity f)]
    [(Lambda params _) (length params)]))

(define (run expr)
  (interp expr '() top-env 'done))

(module+ test
  (require rackunit)

  (check-equal? (run (App (App 'add 23) 1)) 24)
  (check-equal? (run (App 'double 23)) 46)
  (check-equal? (run (App (App 'twice (App 'add 3)) 2)) 8)
  )
