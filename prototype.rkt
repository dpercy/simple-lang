#lang racket
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

#;(
   #|

   What's the meaning of an expression?
   Not just the final value--some expressions don't have one!
   It's the whole evaluation sequence.
   It's not just a flat sequence either: there's a top-level evalsequence,
   but you can expand individual function calls to ask why (f 1 2 3) ==> 4.

   So there are two functions:
   - eval :: globals -> expression -> value
   - evalseq :: globals -> expression -> evaluation-sequence
   Evalseq uses apply to produce the individual steps.
   Eval could use evalseq and throw away the intermediate results:
   but a more efficient implementation should just compute the final result directly.

   Fine, so what operations / expressions are supported?
   - literals are easy
   - primops are pretty easy
   - application is easy
   - if/cond is easy, but hard to typecheck (Typed Racket!)
   .    - also: htdp says "cond is the most complicated expression form in this book"
   - match adds a scoping rule
   .    - more complicated than cond!
   - functions-as-cases seems hard to explain - some functions don't use cases



   TODO:
   - group the env with the expr
   .   - smart printer can decide which to subst, which to define in a margin
   - eval-program gives a bunch of thunks (or callables)
   .   - if one of them diverges but is unused, it doesn't affect anything else
   .   - separately, the caller can run each thunk in a future to get all the terminating ones
   .   - maybe tracing is an optional side effect of the callables?
   .        - this viewpoint is neat because you can always just rerun with more tracing enabled


   |#


   )

(struct Stmt () #:transparent)
(struct Def Stmt () #:transparent)
(struct DefVal Def (name expr) #:transparent)
(struct DefFun Def (name params body) #:transparent)
(struct DefStruct Def (name arity) #:transparent)

(struct Expr Stmt () #:transparent)
(struct Local Expr (name) #:transparent)
(struct Global Expr (name) #:transparent)
(struct Call Expr (func args) #:transparent)
(struct Named Expr (name value) #:transparent)


(struct Match Expr (scrutinee cases) #:transparent)

(struct Case (pat expr) #:transparent)

(struct Pat () #:transparent)
(struct PatHole (name) #:transparent)
(struct PatCtor (name args) #:transparent)


#|

Dag ops:

- peel back the outer layer of substitutions so you can break out the children
- use a dag as an argument in a new term

- traverse the dag (for example to replace the first (+ 1 1) you find with 2),
.  preserving sharing (so that

|#

(define globals? (hash/c symbol? Def?))
(define (last seq)
  (for/last ([elem seq])
    elem))

(define/contract (eval expr globals) (-> Expr? globals? Expr?)
  ; TODO eval would be faster as a big-step interpreter.
  (last (evalseq expr globals)))
(define/contract (evalseq expr globals) (-> Expr? globals? (sequence/c Expr?))
  (match (step expr globals)
    ['value (stream expr)]
    [expr* (stream-cons expr (evalseq expr* globals))]))
(define/contract (step expr globals) (-> Expr? globals? (or/c 'value 'constructor Expr?))
  (match expr
    [(Local name) (error 'step "unbound local ~v" name)]
    [(Global name) (match (hash-ref globals name)
                     [(? DefStruct?) 'constructor]
                     [(? DefFun?) 'value]
                     [(DefVal _ expr) expr])]
    [(Call func args)  (match (step func globals)
                         ; if func can step, it steps
                         [(? Expr? func*)  (Call func* args)]
                         [value-or-constructor
                          (match (step* args globals)
                            ; if any arg can step, it steps
                            [(? list? args*)  (Call func args*)]
                            ['all-values
                             (match value-or-constructor
                               ; if all args are values and func is a constructor,
                               ; the Call is a value (a construction).
                               ['constructor 'value]
                               ; if all args are values and func is a non-constructor value,
                               ; the Call is a redex.
                               ['value  (apply-exprs func args globals)])])])]
    [(Named name value) (match (step value globals)
                          ['value 'value]
                          ['constructor 'constructor]
                          [(? Expr?) (error 'step "got a named non-value: ~v = ~v" name value)])]
    [(Match expr cases) (error 'TODO "impl step match")]
    [_ #false]))
(define/contract (step* exprs globals) (-> (listof Expr?) globals? (or/c 'all-values (listof Expr?)))
  (match exprs
    ['() 'all-values]
    [(cons e es) (match (step e globals)
                   [(or 'value 'constructor) (match (step* es globals)
                                               ['all-values 'all-values]
                                               [es* (cons e es*)])]
                   [(? Expr? e*) (cons e* es)])]))
(define/contract (apply-exprs func args globals) (-> Expr? (listof Expr?) globals? Expr?)
  (match func
    [(Global func-name) (match (hash-ref globals func-name)
                          [(DefFun name params body) #:when (= (length params) (length args))
                           (let ([expr (subst body (make-subst/named params args))])
                             (eval expr globals))])]))

(define subst? (hash/c symbol? Expr?))
(define/contract (make-subst/named params args) (-> (listof symbol?) (listof Expr?) subst?)
  ; Create a substitution that maps each param to a "named" node.
  ; Later, while pretty-printing, any named nodes that are identical (equal?)
  ; can be displayed as shared.
  (make-subst params
              (for/list ([p params]
                         [a args])
                (make-named p a))))
(define (make-named name value)
  (match value
    [(Named _ v) (make-named name v)]
    [v (Named name v)]))
(define/contract (make-subst params args) (-> (listof symbol?) (listof Expr?) subst?)
  (for/hash ([p params]
             [a args])
    (values p a)))
(define/contract (subst expr h) (-> Expr? subst? Expr?)
  (match expr
    [(Local name) (hash-ref h name (lambda () expr))]
    [(Global _) expr]
    [(Call f args) (Call (subst f h)
                         (for/list ([a args])
                           (subst a h)))]
    [(Match e cases)
     (Match e (for ([c cases])
                (subst-case c h)))]))
(define/contract (subst-case case h) (-> Case? subst? Case?)
  (match case
    [(Case pat expr)  (Case pat
                            (subst expr
                                   (hash-remove* h (pat-binders pat))))]))
(define (hash-remove* h keys)
  (for/fold ([h h]) ([k keys])
    (hash-remove h k)))

(define/contract (pat-binders pat) (-> Pat? (set/c symbol?))
  (match pat
    [(PatHole name) (set name)]
    [(PatCtor _ args) (foldr set-union (set) args)]))

(module+ test

  (define env (hash 'Cons (DefStruct 'Cons 2)
                    'Empty (DefStruct 'Empty 0)
                    'double (DefFun 'double '(elem)
                              (Call (Global 'Cons)
                                    (list (Local 'elem)
                                          (Call (Global 'Cons)
                                                (list (Local 'elem)
                                                      (Call (Global 'Empty) '()))))))
                    'triple (DefFun 'double '(elem)
                              (Call (Global 'Cons)
                                    (list (Local 'elem)
                                          (Call (Global 'double) (list (Local 'elem))))))))
  (check-equal? (stream->list (evalseq (Call (Global 'triple) (list (Call (Global 'Empty) '())))
                                       env))
                (list (Call (Global 'triple) (list (Call (Global 'Empty) '())))
                      ; terminates in one step: one function application
                      (let ([elem (Named 'elem (Call (Global 'Empty) '()))])
                        (Call (Global 'Cons)
                              (list elem
                                    (Call (Global 'Cons)
                                          (list elem
                                                (Call (Global 'Cons)
                                                      (list elem
                                                            (Call (Global 'Empty) '()))))))))))
  (check-equal? (stream->list (evalseq (Call (Global 'double)
                                             (list (Call (Global 'double)
                                                         (list (Call (Global 'Empty) '())))))
                                       env))

                (list (Call (Global 'double)
                            (list (Call (Global 'double)
                                        (list (Call (Global 'Empty) '())))))
                      ; first step: reduce inner call
                      (Call (Global 'double)
                            (list (let ([elem (Named 'elem (Call (Global 'Empty) '()))])
                                    (Call (Global 'Cons)
                                          (list elem
                                                (Call (Global 'Cons)
                                                      (list elem
                                                            (Call (Global 'Empty) '()))))))))
                      ; second step: reduce outer call
                      (let ([elem2 (Named 'elem
                                          (let ([elem (Named 'elem (Call (Global 'Empty) '()))])
                                            (Call (Global 'Cons)
                                                  (list elem
                                                        (Call (Global 'Cons)
                                                              (list elem
                                                                    (Call (Global 'Empty) '())))))))])
                        (Call (Global 'Cons)
                              (list elem2
                                    (Call (Global 'Cons)
                                          (list elem2
                                                (Call (Global 'Empty) '()))))))))

  ;;
  )

(define (print-expr expr)
  (define where (hasheq)) ; named-expr -> printed
  (define renamed (hasheq)) ; named-expr -> new-name

  (define counter 0)
  (define (gensym name)
    (define n counter)
    (set! counter (+ counter 1))
    (string->uninterned-symbol (format "~s:~s" name n)))

  (define v
    (let recur ([expr expr])
      (match expr
        [(Local name) name]
        [(Global name) name]
        [(Call func args) (cons (recur func)
                                (map recur args))]
        [(Named name value)
         (if (hash-has-key? where expr)
             ; we already printed this one
             (hash-ref renamed expr)
             ; print it
             (let ()
               (define new-name (gensym name))
               (define v (recur value))
               (set! where
                     (hash-set where expr v))
               (set! renamed
                     (hash-set renamed expr new-name))
               new-name))]
        [(Match _ _) (error 'TODO "print-expr match")])))
  (if (hash-empty? where)
      v
      `[#:val ,v #:where ,@(for/list ([{k v} (in-hash where)])
                             `(def ,(hash-ref renamed k) ,v))]))

#|
TODO now produce an evaluation sequence for a whole program (list of statements):
- each statement produces its own separate stream
- one possible step is global lookup, which requires doing a "last" on a
.  possibly-nonterminating sequence
|#
