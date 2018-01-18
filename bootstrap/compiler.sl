#lang s-exp "sl.rkt"

(include "prelude.sl")


; statements
(struct (DefVal name expr))
(struct (DefFun name params expr))
(struct (DefStruct name params))
(struct (ToplevelExpr expr))

; expressions
(struct (Quote val))
(struct (Error msg))
(struct (Local name))
(struct (Global name))
(struct (Call func args))
(struct (Match scrutinee cases))

; case - one arm of a match expression
(struct (Case pat expr))

; patterns - left-hand side of a case
(struct (PatLitr name))
(struct (PatHole name))
(struct (PatCtor cname args))


; read ...

(struct (VS value string))

#|
(def (read str) ; -> (VS one-s-expression remaining-string)
  (let* ([str (drop-whitespace str)]
         [c (substring str 0 1)])
    ))
(read "  (match lst [(cons x xs) \"hi\"])  ")
|#

; parse ...


; validate ...
; translate to JS ...
