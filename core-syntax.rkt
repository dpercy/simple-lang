#lang racket

(provide (all-defined-out))

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

(struct Stmt (endline) #:transparent)
(struct Def Stmt (name) #:transparent)
(struct DefVal Def (expr) #:transparent)
(struct DefFun Def (params body) #:transparent)
(struct DefStruct Def (arity) #:transparent)
(struct ToplevelExpr Stmt (expr) #:transparent)


(struct Expr () #:transparent)
(struct Quote Expr (value) #:transparent)
(struct Error Expr (msg) #:transparent)
(struct Local Expr (name) #:transparent)
(struct Global Expr (name) #:transparent)
(struct Call Expr (func args) #:transparent)


(struct Match Expr (scrutinee cases) #:transparent)

(struct Case (pat expr) #:transparent)

(struct Pat () #:transparent)
(struct PatLitr (value) #:transparent)
(struct PatHole (name) #:transparent)
(struct PatCtor (name args) #:transparent)
