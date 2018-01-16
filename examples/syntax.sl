; built-in lists
; (need to be declared manually for now)
(struct empty 0)
(struct cons 2)
(cons 1 (cons 2 (empty)))




(struct DefVal 3) ; endline name expr
(struct DefFun 4) ; endline name params body
(struct DefStruct 3) ; endline name arity
(struct ToplevelExpr 2) ; endline expr

(struct Quote 1) ; value
(struct Error 1) ; msg
(struct Local 1) ; name
(struct Global 1) ; name
(struct Call 2) ; func args

(struct Match 2) ; scrutinee cases

(struct Case 2) ; pat expr

(struct PatLitr 1) ; value
(struct PatHole 1) ; name
(struct PatCtor 2) ; cname args

