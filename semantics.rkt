#lang racket

; high-level interface
(provide eval-program
         run-program/sequential
         (struct-out Result)
         (struct-out ResultValue)
         (struct-out ResultError))
; low-level interface
(provide Block?
         block-deps
         block-names
         run-block/args)

(require (prefix-in racket: racket))
(require racket/generator)
(require racket/struct)
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
- no waiting for other threads

|#

; The meaning of an expression is a Racket computation.
; The computation (comp) is represented as a procedure that accepts free variables,
; and then executes (either to a halt, to a crash, or forever).
; A DenotExpr struct tracks the names of the free variables alongside the procedure.
(struct DenotExpr (fv comp) #:transparent)

(define (closed? v) (empty? (DenotExpr-fv v)))
(define closed-DenotExpr? (and/c DenotExpr? closed?))

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

    [(Match test cases)
     (match-let-values ([{fv (list arg func)}
                         (combine-denots (list (eval test)
                                               (eval-cases cases)))])
       (DenotExpr fv (apply/fv fv func (list arg))))]))

(define/contract (run denot) (-> closed-DenotExpr? any/c)
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
(define/contract (close* denot args) (-> DenotExpr? (hash/c symbol? any/c) DenotExpr?)
  (for/fold ([denot denot]) ([{name val} (in-hash args)])
    (if (member name (DenotExpr-fv denot))
        (close/1 denot name val)
        denot)))
(define/contract (close denot args) (->i ([denot DenotExpr?]
                                          [args (hash/c symbol? any/c)])
                                         #:pre (denot args) (for/and ([key (DenotExpr-fv denot)])
                                                              (hash-has-key? args key))
                                         [result closed-DenotExpr?])
  (close* denot args))

(define/contract (run/args denot args) (-> DenotExpr? (hash/c symbol? any/c) any/c)
  (for ([fv (DenotExpr-fv denot)])
    (unless (hash-has-key? args fv)
      (error 'run/args "unbound global: ~s" fv)))
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


(struct DenotPat (binders comp) #:transparent)
(define/contract (eval-cases cases) (-> (listof Case?) DenotExpr?)
  (match cases
    ['() (DenotExpr '() (lambda ()
                          (lambda (val)
                            (error 'match "no case for ~v" val))))]
    [(cons (Case pat expr) cases)

     (define PAT pat)
     (match-let* ([(DenotPat binders pat) (eval-pat pat)]
                  [expr (make-function binders (eval expr))]
                  [cases (eval-cases cases)])
       (match-let-values ([{fv (list expr cases)} (combine-denots (list expr cases))])
         (DenotExpr fv
                    (lambda fv
                      (lambda (val)
                        (match (pat val)
                          [#false ((apply cases fv) val)]
                          [bound-values (apply (apply expr fv) bound-values)]))))))]))
(define/contract (eval-pat pat) (-> Pat? DenotPat?)
  (match pat
    [(PatLitr v) (DenotPat '()
                           (lambda (val)
                             (if (equal? val v)
                                 '()
                                 #false)))]
    [(PatHole name) (DenotPat (list name)
                              (lambda (val) (list val)))]
    [(PatCtor cname argpats)
     (define-values {make-foo foo? foo-ref} (make-prefab cname (length argpats)))
     (match-define (DenotPat binders argpats-comp) (eval-pats argpats))
     (DenotPat binders
               (lambda (val)
                 (if (foo? val)
                     (argpats-comp (struct->list val))
                     #false)))]))
(define/contract (eval-pats pats) (-> (listof Pat?) DenotPat?)
  (match pats
    ['() (DenotPat '()
                   (lambda (val) (list)))]
    [(cons pat0 pats) (match-let ([(DenotPat pat0-binders pat0-comp) (eval-pat pat0)]
                                  [(DenotPat pats-binders pats-comp) (eval-pats pats)])
                        (match (set-intersect pat0-binders pats-binders)
                          [(cons x _) (error "dup pattern variable: ~v" x)]
                          ['()
                           (DenotPat (append pat0-binders pats-binders)
                                     (lambda (val)
                                       (match (pat0-comp (first val))
                                         [#false #false]
                                         [pat0-vs
                                          (match (pats-comp (rest val))
                                            [#false #false]
                                            [pats-vs
                                             (append pat0-vs pats-vs)])])))]))]))
(define (make-prefab name arity)
  (define-values {foo/type make-foo foo? foo-ref foo-set!}
    (make-struct-type name
                      #f
                      arity
                      0
                      #f
                      '()
                      'prefab
                      #f
                      (build-list arity values)))
  (values make-foo foo? foo-ref))
(module+ test

  (struct point (x y) #:prefab)
  (match-let ([(DenotPat binders comp) (eval-pat (PatCtor 'point
                                                          (list
                                                           (PatHole 'a)
                                                           (PatHole 'b))))])
    (check-equal? binders '(a b))
    (check-equal? (comp (point 1 2)) (list 1 2))
    (check-equal? (comp (point "spam" "eggs")) (list "spam" "eggs"))
    (check-equal? (comp "foo") #false)))



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
(struct BlockVal Block (endline name val) #:transparent) ; single, maybe-named expr

(define/contract (block-deps block) (-> Block? (listof symbol?))
  (match block
    ; defstruct never depends on another statement
    [(BlockDecl _) '()]
    ; a toplevel expression block depends on its free variables
    [(BlockVal _ _ v) (DenotExpr-fv v)]
    ; a letrec block depends on its functions' free variables,
    ; minus the functions themselves.
    [(BlockFix funcs) (let ([fvs (foldr set-union '()
                                        (map DenotExpr-fv (hash-values funcs)))])
                        (set-subtract fvs (hash-keys funcs)))]))
(define (block-names block)
  (match block
    [(BlockDecl (? DefStruct? ds)) (list (Def-name ds))]
    [(BlockFix funcs) (hash-keys funcs)]
    [(BlockVal _ #f _) '()]
    [(BlockVal _ name _) (list name)]))



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
  (define dep-graph (for/hash ([{name b} (in-hash blocks-by-name)])
                      (values name
                              ; Ignore dependencies on things not defined here.
                              ; These are imports, which will be provided to
                              ; run-program/sequential.
                              (for/list ([name (block-deps b)]
                                         #:when (hash-has-key? blocks-by-name name))
                                name))))
  (define sccs (reverse (find-sccs dep-graph)))
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
    [(ToplevelExpr el expr) (BlockVal el #f (eval expr))]
    [(DefVal el name expr) (BlockVal el name (eval expr))]
    [(DefFun _ name params body) (BlockFix (hash name (make-function params (eval body))))]
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

(define (make-constructor name arity)
  (define args (for/list ([i (in-range arity)])
                 (string->symbol (format "x~v" i))))
  (define proc
    (racket:eval #`(lambda #,args
                     (make-prefab-struct (quote #,name) #,@args))))
  (procedure-rename proc name))
(module+ test
  (check-equal? (object-name (make-constructor 'foo 3)) 'foo)
  (check-equal? (procedure-arity (make-constructor 'foo 3)) 3)
  (check-equal? (let ([foo (make-constructor 'foo 3)])
                  (foo 'a 'b 'c))
                (make-prefab-struct 'foo 'a 'b 'c)))


(module+ test

  ; example program
  (define prog1
    (list (DefVal 42 'a (Quote 5))
          (DefFun #f 'f '(n) (Call (Global 'g) (list (Global 'a))))
          (DefFun #f 'g '(n) (Call (Global 'f) (list (Global 'n))))
          (DefVal 64 'x (Call (Global 'f) (list (Quote 0))))
          (ToplevelExpr 99 (Global 'x))))
  (check-match (eval-program prog1)
               (list (BlockVal 42 'a (DenotExpr '() _))
                     (BlockFix (hash-table ['f (DenotExpr '(g a) _)]
                                           ['g (DenotExpr '(f) _)]))
                     (BlockVal 64 'x (DenotExpr '(f) _))
                     (BlockVal 99 #f (DenotExpr '(x) _))))

  ;;
  )


(struct Result (endline name) #:transparent)
(struct ResultValue Result (value) #:transparent)
(struct ResultError Result (msg) #:transparent)
(define/contract (run-block/args block args) (-> Block? (hash/c symbol? any/c) (listof Result?))
  (match block
    [(BlockDecl (DefStruct el name arity))
     (list (ResultValue el name (make-constructor name arity)))]
    [(BlockVal el name val)

     (with-handlers ([values (lambda (exn)
                               (list (ResultError el name (exn-message exn))))])
       (list (ResultValue el name (run/args val args))))]
    [(BlockFix funcs)
     (define closed-funcs (for/hash ([{name val} funcs])
                            (values name (close* val args))))
     (define closed-fixed-funcs (run/fix closed-funcs))
     (for/list ([{name val} closed-fixed-funcs])
       (ResultValue #f name val))]))

(define/contract (run-program/sequential blocks globals) (-> (listof Block?) (hash/c symbol? any/c) (sequence/c Result?))
  (in-generator
   (for ([b blocks])
     (define results (run-block/args b globals))
     (for ([r results])
       (match r
         [(ResultValue _ name val) (begin
                                     (when name
                                       (set! globals (hash-set globals name val)))
                                     (yield r))]
         ; TODO what happens to things that depended on this name?
         [(ResultError _ name msg) (yield r)])))))

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
(define/contract (run/fix funcs) (-> (hash/c symbol? DenotExpr?) (hash/c symbol? procedure?))
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
  - create a new file for a concurrent (not parallel!) runner
  - create another file for a websocket server,
  and make the front-end work with it.
  .   - ui should put values below expressions on a line widget
  .       - requires line numbers for each anonymous expr

  |#)


(module+ test

  (define even/odd-prog (list (DefFun #f 'even '(n)
                                (Match (Call (Global '=) (list (Local 'n) (Global 'zero)))
                                       (list
                                        (Case (PatLitr #true) (Quote #true))
                                        (Case (PatLitr #false) (Call (Global 'odd)
                                                                     (list
                                                                      (Call (Global '-)
                                                                            (list (Local 'n)
                                                                                  (Quote 1)))))))))
                              (DefVal #f 'zero (Quote 0))
                              (DefFun #f 'odd '(n)
                                (Match (Call (Global '=) (list (Local 'n) (Global 'zero)))
                                       (list
                                        (Case (PatLitr #true) (Quote #false))
                                        (Case (PatLitr #false) (Call (Global 'even)
                                                                     (list
                                                                      (Call (Global '-)
                                                                            (list (Local 'n)
                                                                                  (Quote 1)))))))))

                              (DefVal #f 'seven-odd  (Call (Global 'odd)  (list (Quote 7))))
                              (DefVal #f 'seven-even (Call (Global 'even) (list (Quote 7))))
                              (ToplevelExpr
                               #f
                               (Call (Global '-)
                                     (list (Quote 50)
                                           (Quote 8))))
                              (ToplevelExpr
                               #f
                               (Call (Global '+)
                                     (list (Quote 2) (Quote 3))))))

  (define even/odd-denot (eval-program even/odd-prog))
  (define even/odd-results (run-program/sequential even/odd-denot
                                                   (hash '- -
                                                         '= =
                                                         '+ +)))
  (check-match (sequence->list even/odd-results)
               (list-no-order (ResultValue _ 'zero 0)
                              (ResultValue _ 'even _)
                              (ResultValue _ 'odd _)
                              (ResultValue _ 'seven-odd #true)
                              (ResultValue _ 'seven-even #false)
                              (ResultValue _ #f 42)
                              (ResultValue _ #f 5)))

  ;;
  )
