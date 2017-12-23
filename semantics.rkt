#lang racket
(require (prefix-in racket: racket))
(require "core-syntax.rkt")
(module+ test (require rackunit))

; When generating code at run-time, we want to use the simplest
; versions of lambda and #%app, to avoid doing a dynamic-require
; of the fancy, keyword-supporting lambda and #%app.
(require (for-template
          (only-in racket
                   [#%plain-app #%app]
                   [#%plain-lambda lambda])))

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
- avoid relying on Racket's side effects, because they make it
.  - hard to understand
.  - hard to port (to JS, say)
- be simple
.  - easy to understand
.  - easy to port to another host language
- keep semantics separate from execution
.  - semantics shouldn't know about threads!


Let's say the meaning of a phrase is a Racket lambda (a "runnable").
The runnable itself is a pure value.

*Running* the runnable produces some limited side effects:
- it can crash
- it can diverge

But there are many more side effects it canNOT do:
- no gensym
- no mutation
- no filesystem
- no clock measurement
- no spawning threads


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
                  (apply/fv fv func args))])])]
    [(Match test
            (list (Case (PatLitr #true) consq)
                  (Case (PatLitr #false) alt)))
     ; first ensure all 3 subexprs have the same free vars
     (match (combine-denots (list (eval test)
                                  (eval consq)
                                  (eval alt)))
       [(Denot fv (list test consq alt))
        ; then wire them together under a binder
        (Denot fv (if/fv fv test consq alt))])]
    [(? Match?) (error 'TODO "for now only (match _ [#t _] [#f _]) works")]))

(define/contract (run denot) (-> Denot? any/c)
  (match denot
    [(Denot '() comp) (comp)]
    [(Denot fv _) (error 'run "this expr is not closed: ~v" fv)]))

(define/contract (run/args denot args) (-> Denot? (hash/c symbol? any/c) any/c)
  (match denot
    [(Denot fvs comp)
     (apply comp
            (for/list ([fv fvs])
              (hash-ref args fv)))]))

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
  (racket:eval #`(lambda #,input-shape (values #,@output-shape))))
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
                 #`(lambda (#,@fv)
                     ((func #,@fv)
                      (args #,@fv) ...)))))
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

(define (if/fv fv test consq alt)
  (racket:eval #`(lambda (#,@fv)
                   (if (#,test #,@fv)
                       (#,consq #,@fv)
                       (#,alt #,@fv)))))

(module+ test

  (check-equal? (run (eval (Quote 17)))
                17)
  (check-equal? (run (eval (Call (Quote *) '())))
                1)
  (check-equal? (run (eval (Call (Quote -) (list (Quote 10) (Quote 7)))))
                3)

  (check-equal? (run (eval (Match (Quote #false)
                                  (list (Case (PatLitr #true) (Quote 5))
                                        (Case (PatLitr #false) (Quote 7))))))
                7)

  (check-equal? (run/args (eval (Local 'x))
                          (hash 'x 123))
                123)

  (check-equal? (run/args (eval (Local 'x))
                          (hash 'x 123))
                123)

  (check-equal? (run/args (eval (Match (Local 'test)
                                       (list (Case (PatLitr #true) (Local 'consq))
                                             (Case (PatLitr #false) (Local 'alt)))))
                          (hash 'test #true
                                'consq 1
                                'alt 2))
                1)
  (check-equal? (run/args (eval (Match (Local 'test)
                                       (list (Case (PatLitr #true) (Local 'consq))
                                             (Case (PatLitr #false) (Local 'alt)))))
                          (hash 'test #false
                                'consq 1
                                'alt 2))
                2)

  ;;
  )
