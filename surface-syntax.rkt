#lang racket
(require "core-syntax.rkt")

(provide kw? name? parse-program render render-abbreviate-defun)


(define kw? (or/c 'def 'struct 'match))
(define name? (and/c symbol? (not/c kw?)))
(define (parse-program sexprs) ; -> listof stmt
  (for/list ([sexpr sexprs])
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (ToplevelExpr (Error (exn-message exn))))])
      (parse-stmt sexpr))))
(define (parse-stmt sexpr) ; -> stmt
  (match sexpr
    [`(def ,(? name? name) ,v) (DefVal name (parse-expr v (set)))]
    [`(def (,(? name? name) ,(? name? params) ...) ,body)
     (DefFun name params (parse-expr body (list->set params)))]
    [`(struct ,(? name? name) ,(? number? arity))
     (DefStruct name arity)]
    [_ (ToplevelExpr (parse-expr sexpr (set)))]))
(define (parse-expr sexpr locals)
  (match sexpr
    [(? string? s) (Quote s)]
    [(? number? n) (Quote n)]
    [(? boolean? b) (Quote b)]
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
    [(? string? s) (PatLitr s)]
    [(? number? n) (PatLitr n)]
    [(? boolean? b) (PatLitr b)]
    [(? name? name) (PatHole name)]
    [(list* (? name? name) args) (PatCtor name (map parse-pat args))]))
(define (pat-holes pat)
  (match pat
    [(PatLitr _) (set)]
    [(PatHole name) (set name)]
    [(PatCtor _ args) (foldr set-union (set) (map pat-holes args))]))

(define render-abbreviate-defun (make-parameter #false))
(define (render term)
  (define r render)
  (match term
    [(DefVal name val) `(def ,name ,(r val))]
    [(DefFun name ps b) #:when (render-abbreviate-defun) `(def (,name ,@ps) ...)]
    [(DefFun name ps b) `(def (,name ,@ps) ,(r b))]
    [(DefStruct name arity) `(struct ,name ,arity)]
    [(Quote v) v]
    [(Local name) name]
    [(Global name) name]
    [(Call func args) (cons (r func) (map r args))]
    [(Match scr cases) `(match ,(r scr) ,@(map r cases))]
    [(Case pat expr) `[,(r pat) ,(r expr)]]
    [(PatLitr v) v]
    [(PatHole name) name]
    [(PatCtor name args) `(,name ,@(map r args))]
    [(Error msg) `(#:fail ,msg)]))
