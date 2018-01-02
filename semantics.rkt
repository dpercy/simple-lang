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
(require (only-in "graph.rkt" find-sccs))

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

; The meaning of an expression is a Racket computation.
; The computation (comp) is represented as a procedure that accepts free variables,
; and then executes (either to a halt, to a crash, or forever).
; A DenotExpr struct tracks the names of the free variables alongside the procedure.
(struct DenotExpr (fv comp) #:transparent)

(define/contract (eval expr) (-> Expr? DenotExpr?)
  (match expr
    [(Quote v) (DenotExpr '() (lambda () v))]
    [(Error msg) (DenotExpr '() (lambda () (error msg)))]
    [(or (Global name) (Local name)) (DenotExpr (list name)
                                                (lambda (x) x))]
    [(Call func args)
     (match (map eval (cons func args))
       [(cons func args)
        (match-let-values ([{fv (cons func args)} (combine-denots (cons func args))])
          (DenotExpr fv
                     (apply/fv fv func args)))])]
    [(Match test
            (list (Case (PatLitr #true) consq)
                  (Case (PatLitr #false) alt)))
     ; first ensure all 3 subexprs have the same free vars
     (match-let-values ([{fv (list test consq alt)}
                         (combine-denots (list (eval test)
                                               (eval consq)
                                               (eval alt)))])
       ; then wire them together under a binder
       (DenotExpr fv (if/fv fv test consq alt)))]
    [(? Match?) (error 'TODO "for now only (match _ [#t _] [#f _]) works")]))

(define/contract (run denot) (-> DenotExpr? any/c)
  (match denot
    [(DenotExpr '() comp) (comp)]
    [(DenotExpr fv _) (error 'run "this expr is not closed: ~v" fv)]))

(define/contract (close/1 denot name val) (-> DenotExpr? symbol? any/c DenotExpr?)
  (define old-fv (DenotExpr-fv denot))
  (unless (member name old-fv)
    (error 'close/1 "~v not free in ~v" name old-fv))
  (define new-fv (remove name old-fv))
  (define comp (permute-params denot (cons name new-fv)))
  (DenotExpr new-fv
             (lambda args (apply comp val args))))
(define/contract (close denot args) (-> DenotExpr? (hash/c symbol? any/c) DenotExpr?)
  (for/fold ([denot denot]) ([{name val} (in-hash args)])
    (if (member name (DenotExpr-fv denot))
        (close/1 denot name val)
        denot)))

(define/contract (run/args denot args) (-> DenotExpr? (hash/c symbol? any/c) any/c)
  (run (close denot args)))

(define/contract (combine-denots denots) (-> (listof DenotExpr?)
                                             (values (listof symbol?) (listof procedure?)))
  (match denots
    [(list (DenotExpr fv-lists _) ...)
     (let* ([all-fv (remove-duplicates (apply append fv-lists))]
            [funcs (for/list ([denot denots])
                     (permute-params denot all-fv))])
       (values all-fv funcs))]))
(define (permute-params ed new-fv)
  (match ed
    [(DenotExpr old-fv func)
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
  (check-equal? ((permute-params (DenotExpr '(a x b)
                                            (lambda (a x b) x))
                                 '(x y a b))
                 'one 'two 'red 'blue)
                'one)
  (check-equal? ((permute-params (DenotExpr '(a x b)
                                            (lambda (a x b) x))
                                 '(q x a b))
                 'one 'two 'red 'blue)
                'two)

  ; you can't remove parameters
  (check-exn exn:fail?
             (permute-params (DenotExpr '(x)
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


  (check-equal? (run/args (DenotExpr '(x y z) (lambda (x y z) (list x y z)))
                          (hash 'x 1 'y 2 'z 3))
                '(1 2 3))
  (check-equal? (run/args (close/1 (DenotExpr '(x y z) (lambda (x y z) (list x y z)))
                                   'x 1)
                          (hash 'y 2 'z 3))
                '(1 2 3))
  (check-equal? (run/args (close/1 (close/1 (DenotExpr '(x y z) (lambda (x y z) (list x y z)))
                                            'y 2)
                                   'z 3)
                          (hash 'x 1))
                '(1 2 3))

  ;;
  )


#|

What do you get when you eval a whole program?
A sequence of "blocks".
A "block" can be:
- some struct declarations
- some function declarations
- a defval to run
- an anonymous expr to run (just like a defval)

Except where one block explicitly depends on another,
they can be run independently.
defstruct and deffun always succeed

|#
(struct Block () #:transparent)
(struct BlockDecl Block (decl) #:transparent) ; a single defstruct
(struct BlockFix Block (funcs) #:transparent) ; functions
(struct BlockVal Block (name val) #:transparent) ; single, maybe-named expr

(define/contract (block-deps block) (-> Block? (listof symbol?))
  (match block
    ; defstruct never depends on another statement
    [(BlockDecl _) '()]
    ; a toplevel expression block depends on its free variables
    [(BlockVal _ v) (DenotExpr-fv v)]
    ; a letrec block depends on its functions' free variables,
    ; minus the functions themselves.
    [(BlockFix funcs) (let ([fvs (foldr set-union '()
                                        (map DenotExpr-fv (hash-values funcs)))])
                        (set-subtract fvs (hash-keys funcs)))]))
(define (block-names block)
  (match block
    [(BlockDecl (DefStruct name _)) (list name)]
    [(BlockFix funcs) (hash-keys funcs)]
    [(BlockVal #f _) '()]
    [(BlockVal name _) (list name)]))



(define/contract (eval-program stmts) (-> (listof Stmt?) (listof Block?))
  ; Calling eval-statement on each statement gives you one block per statement.
  ; But eval-program must produce an acyclic list of blocks.
  ; So we also need to find strongly-connected components and merge them.
  (define per-stmt-blocks (map eval-statement stmts))

  (define blocks-by-name (for/hash ([b per-stmt-blocks]
                                    #:when (cons? (block-names b)))
                           (values (only (block-names b))
                                   b)))
  ; We reverse the SCCs because find-sccs sorts with the arrows going forward,
  ; but when sorting by dependencies we want the most pointed-to items
  ; earlier in the list.
  (define sccs (reverse
                (find-sccs (for/hash ([{name b} (in-hash blocks-by-name)])
                             (values name (block-deps b))))))
  (define named-blocks (for/list ([scc sccs])
                         (define blocks (for/list ([name scc])
                                          (hash-ref blocks-by-name name)))
                         (foldr1 merge-blocks blocks)))
  (define anonymous-expr-blocks (for/list ([b per-stmt-blocks]
                                           #:when (and (BlockVal? b)
                                                       (false? (BlockVal-name b))))
                                  b))
  (append named-blocks
          anonymous-expr-blocks))
(define (only lst)
  (match lst
    [(list x) x]))

(define/contract (merge-blocks b1 b2) (-> Block? Block? Block?)
  (when (BlockVal? b1)
    (error 'eval-program "defval ~s depends on itself" (BlockVal-name b1)))
  (when (BlockVal? b2)
    (error 'eval-program "defval ~s depends on itself" (BlockVal-name b2)))
  (merge-fixes b1 b2))
(define/contract (merge-fixes b1 b2) (-> BlockFix? BlockFix? BlockFix?)
  (match-define (BlockFix funcs1) b1)
  (match-define (BlockFix funcs2) b2)
  (BlockFix (for/fold ([h funcs1]) ([{k v} funcs2])
              (hash-set h k v))))

(define (foldr1 f lst)
  (match lst
    [(list x) x]
    [(cons x xs) (f x (foldr1 f xs))]))
(module+ test
  (check-equal? (foldr1 append '((a b c) () (x) (y)))
                '(a b c x y))
  (check-equal? (foldr1 list '(a b c x y z))
                '(a (b (c (x (y z)))))))


(define/contract (eval-statement stmt) (-> Stmt? Block?)
  (match stmt
    [(? Expr?) (BlockVal #f (eval stmt))]
    [(DefVal name expr) (BlockVal name (eval expr))]
    [(DefFun name params body) (BlockFix (hash name (make-function params (eval body))))]
    [(? DefStruct?) (BlockDecl stmt)]))
(define (make-function params denot-func)
  ; denot-func is the denotation of a function body
  ; (so its free variables are a combination of globals and parameters).
  ; make-function returns the denotation of the whole function.
  (match-define (DenotExpr old-fvs comp) denot-func)
  (define new-fvs (for/list ([fv old-fvs]
                             #:when (not (member fv params)))
                    fv))
  (DenotExpr new-fvs
             (racket:eval #`(lambda #,new-fvs
                              (lambda #,params
                                ((quote #,comp) #,@old-fvs))))))
(module+ test
  (check-equal? ((run/args (make-function '(y)
                                          (DenotExpr '(x y z)
                                                     (lambda (x y z) (list x y z))))
                           (hash 'x 'xx 'z 'zz))
                 'yy)
                '(xx yy zz)))


(module+ test

  ; example program
  (define prog1
    (list (DefVal 'a (Quote 5))
          (DefFun 'f '(n) (Call (Global 'g) (list (Global 'a))))
          (DefFun 'g '(n) (Call (Global 'f) (list (Global 'n))))
          (DefVal 'x (Call (Global 'f) (list (Quote 0))))
          (Global 'x)))
  (check-match (eval-program prog1)
               (list (BlockVal 'a (DenotExpr '() _))
                     (BlockFix (hash-table ['f (DenotExpr '(g a) _)]
                                           ['g (DenotExpr '(f) _)]))
                     (BlockVal 'x (DenotExpr '(f) _))
                     (BlockVal #f (DenotExpr '(x) _))))

  ;;
  )


(define/contract (run-program/sequential blocks) (-> (listof Block?) (listof any/c))
  (define globals (hash))
  (for ([b blocks])
    (run-block/args b globals)
    ))

(struct Result (name val) #:transparent)
(define/contract (run-block/args block args) (-> Block? (hash/c symbol? any/c) (listof Result?))
  (match block
    [(BlockDecl (DefStruct name arity)) (error 'TODO "make a new constructor")]
    [(BlockVal name val) (list (Result name (run/args val args)))]
    [(BlockFix funcs)
     (define closed-fixed-funcs (run/fix (for/hash ([{name val} funcs])
                                           (values name (close val args)))))
     (for/hash ([{name val} closed-fixed-funcs])
       (values name (run val)))]))

#|

"fix" is a fixpoint combinator for records of functions:

|   y f = f (y f)
|   (run/fix funcs) ===
|       (for/hash ([{name func} funcs])
|         (values name
|                 (run/args func (run/fix funcs))))

It feeds the closed version of the thing into the open version of the thing.
That way the open version can call the closed version directly.

|#
(define (Y f)
  ; spec:
  '(f (Y f))
  ; eta expand:
  '(f (lambda args (apply (Y f) args)))
  ; use sharing to call Y only once:
  (letrec ([result (f (lambda args (apply result args)))])
    result))
(define (run/fix funcs)
  ; specification of the result:
  '(for/hash ([{name func} funcs])
     (values name
             (run/args func (run/fix funcs))))
  ; eta expand:
  (for/hash ([{name func} funcs])
    (values name
            (lambda args
              (apply (run/args func (run/fix funcs))
                     args))))
  ; use sharing to call fix only once:
  (letrec ([result
            (for/hash ([{name func} funcs])
              (values name
                      (lambda args
                        (apply (run/args func result)
                               args))))])
    result))
(module+ test

  (define (fact-open fact)
    (lambda (n)
      (if (= n 0)
          1
          (* n (fact (- n 1))))))

  (check-equal? (map (Y fact-open) '(0 1 2 3 4))
                '(1 1 2 6 24))

  (define fact-record (hash 'fact (DenotExpr '(fact) fact-open)))
  (define fact-record-fixed (run/fix fact-record))
  (define fact (hash-ref fact-record-fixed 'fact))
  (check-equal? (map fact '(0 1 2 3 4))
                '(1 1 2 6 24))


  (define even/odd-record (hash 'even (DenotExpr '(odd)
                                                 (lambda (odd)
                                                   (lambda (n)
                                                     (if (= n 0)
                                                         #true
                                                         (odd (- n 1))))))
                                'odd (DenotExpr '(even)
                                                (lambda (even)
                                                  (lambda (n)
                                                    (if (= n 0)
                                                        #false
                                                        (even (- n 1))))))))
  (define even/odd-record-fixed (run/fix even/odd-record))
  (define even (hash-ref even/odd-record-fixed 'even))
  (define odd  (hash-ref even/odd-record-fixed 'odd))
  (check-equal? (map even '(0 1 2 3 4))
                '(#t #f #t #f #t))
  (check-equal? (map odd '(0 1 2 3 4))
                '(#f #t #f #t #f))

  #|

  next steps:
  - create a new file for a parallel runner
  - create another file for a websocket server,
  and make the front-end work with it.
  .   - ui should put values below expressions on a line widget
  .       - requires line numbers for each anonymous expr

  |#)
