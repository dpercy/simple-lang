#lang racket
(require (prefix-in racket: racket))
(require "core-syntax.rkt")
(module+ test (require rackunit))

(define-syntax match
  (syntax-rules ()
    [(_ scrutinee cases ...)
     (let ([tmp scrutinee])
       (racket:match tmp
                     cases ...
                     [_ (error 'semantics
                               "No case for ~v => ~v\n  at ~v"
                               'scrutinee
                               tmp
                               #'scrutinee)]))]))


#|

Goal:
- explain the meaning of each program phrase
- avoid relying on Racket's side effects
- be simple


Let's say the meaning of a phrase is a Racket lambda (a "runnable").
That way, the meaning is a value.
When run, this lambda will either
- terminate
- crash
- ... yield somehow?
- ... request a global somehow?

|#

(struct Denot (fv comp) #:transparent)

(define/contract (eval expr) (-> Expr? Denot?)
  (match expr
    [(Quote v) (Denot '() (lambda () v))]
    [(Error msg) (Denot '() (lambda () (error msg)))]
    [(Local name) (Denot (list name)
                         (lambda (x) x))]
    [(Global name) (error 'TODO "globals")]
    [(Call func args)
     (match (map eval (cons func args))
       [(cons func args)
        (match (combine-denots (cons func args))
          [(Denot fv (cons func args))
           (Denot fv
                  (apply/fv fv func args))])])]))

(define/contract (run denot) (-> Denot? any/c)
  (match denot
    [(Denot '() comp) (comp)]
    [(Denot fv _) (error 'run "this expr is not closed: ~v" fv)]))

(define (combine-denots denots)
  (match denots
    [(list (Denot fv-lists _) ...)
     (let* ([all-fv (remove-duplicates (apply append fv-lists))]
            [funcs (for/list ([denot denots])
                     (permute-params denot all-fv))])
       (Denot all-fv funcs))]))
(define (permute-params ed new-fv)
  (match ed
    [(Denot old-fv func)
     (compose func
              (value-rearranger new-fv old-fv))]))
(define/contract (value-rearranger input-shape output-shape)
  (-> (listof symbol?) (listof symbol?) procedure?)
  (racket:eval #`(#%plain-lambda #,input-shape (#%plain-app values #,@output-shape))))
(module+ test


  (check-equal? ((value-rearranger '(x y z) '(x)) 'a 'b 'c)
                'a)
  (check-equal? ((value-rearranger '(x y z) '(y)) 'a 'b 'c)
                'b)
  (check-equal? ((value-rearranger '(x y z) '(z)) 'a 'b 'c)
                'c)

  ; permute-params lets you add and reorder parameters to a function
  (check-equal? ((permute-params (Denot '(a x b)
                                        (lambda (a x b) x))
                                 '(x y a b))
                 'one 'two 'red 'blue)
                'one)
  (check-equal? ((permute-params (Denot '(a x b)
                                        (lambda (a x b) x))
                                 '(q x a b))
                 'one 'two 'red 'blue)
                'two)

  ; you can't remove parameters
  (check-exn exn:fail?
             (permute-params (Denot '(x)
                                    (lambda (x) x))
                             '(q x)))


  ;;
  )

(define (apply/fv fv func args)
  (racket:eval (with-syntax ([func func]
                             [(args ...) args])
                 #`(#%plain-lambda (#,@fv)
                                   (#%plain-app
                                    (#%plain-app func #,@fv)
                                    (#%plain-app args #,@fv) ...)))))
(module+ test
  (check-equal? ((apply/fv '(a b c)
                           ; (- b 7) with fv=[a b c]
                           (lambda (a b c) -)
                           (list (lambda (a b c) b)
                                 (lambda (a b c) 7)))
                 ; instantiate fv as [100 10 1]
                 100 10 1)
                ; result should be (- 10 7)
                3))

(module+ test

  (check-equal? (run (eval (Quote 17)))
                17)
  (check-equal? (run (eval (Call (Quote *) '())))
                1)
  (check-equal? (run (eval (Call (Quote -) (list (Quote 10) (Quote 7)))))
                3)


  )
