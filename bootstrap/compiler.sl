#lang s-exp "sl.rkt"

(include "prelude.sl")


; statements
(struct (DefVal name expr))
(struct (DefFun name params body))
(struct (DefStruct name params))
(struct (ToplevelExpr expr))

; expressions
(struct (Quote val))
(struct (Var name))
(struct (Error msg))
(struct (Call func args))
(struct (Match scrutinee cases))

; case - one arm of a match expression
(struct (Case pat expr))

; patterns - left-hand side of a case
(struct (PatLitr name))
(struct (PatHole name))
(struct (PatCtor cname args))


; read ...

; an sexpr is either:
; - (empty)
; - (cons sexpr sexpr)
; - a string, representing an identifier
; - (SelfQuoting value)

(struct (VS value string))
(struct (SelfQuoting value))

(def (read-one s)
  (match (read-all s)
    [(list v) v]
    [(empty) (error "no s-expressions in string")]
    [more (error "expected only one s-expression in string")]))

(def (read-all s) ; list of s-exprs
  (match (drop-whitespace s)
    ["" (empty)]
    [s
     (match (read s)
       [(VS first s)
        (match (read-all s)
          [rest
           (cons first rest)])])]))

(def (read s) ; -> (VS one-s-expression remaining-string)
  (match (drop-whitespace s)
    [s
     (match (substring s 0 1)
       ["(" (read-list ")" (substring* s 1))]
       ["[" (read-list "]" (substring* s 1))]
       [c
        (match (symbol-char? c)
          [(true) (read-symbol-or-natural s)]
          [(false)
           (match (= (ord c) (ord "\""))
             [(true) (match (read-string (substring* s 1))
                       [(VS t s) (VS (SelfQuoting t) s)])]
             [(false)
              (error "read: weird character")])])])]))

(def (drop-whitespace s)
  ; TODO handle comments
  (match s
    ["" ""]
    [s
     (match (whitespace? (substring s 0 1))
       [(true) (drop-whitespace (substring* s 1))]
       [(false) s])]))

(def (read-list end s)
  (match (drop-whitespace s)
    [s (match (= (ord end) (ord (substring s 0 1)))
         [(true) (VS (empty)
                     (substring* s 1))]
         [(false)
          (match (read s)
            [(VS head s)
             (match (read-list end s)
               [(VS tail s)
                (VS (cons head tail) s)])])])]))

(def (read-string s)
  (match (substring s 0 1)
    ["\"" (VS "" (substring* s 1))]
    ; TODO update emit-quoted-constant if we add escapes
    ["\\" (error "escapes are not supported")]
    ["\n" (error "multiline strings are not supported")]
    [c (match (read-string (substring* s 1))
         [(VS t s) (VS (string-append c t) s)])]))


(def (read-symbol-or-natural s)
  (match (read-token s)
    [(VS t s) (VS (convert-token t) s)]))

(def (read-token s)
  (match s
    ["" (VS "" s)]
    [s
     (match (substring s 0 1)
       [c
        (match (symbol-char? c)
          [(false) (VS "" s)]
          [(true) (match (read-token (substring* s 1))
                    [(VS t s)
                     (VS (string-append c t)
                         s)])])])]))

(def (convert-token t)
  (match (andmap digit? (string-chars t))
    [(true) (SelfQuoting (string->natural t))]
    [(false) t]))



(def (string->natural s)
  (rev-digits->natural (map digit-value (reverse (explode s)))))

(def (natural->string n)
  (match (string-append* (reverse (natural->rev-digits n)))
    ; An "empty natural literal" isn't a thing,
    ; so replace it with zero.
    ["" "0"]
    [s s]))

(def (digit-value c)
  (- c (ord "0")))

(def (digit val)
  (chr (+ val (ord "0"))))

(def (rev-digits->natural revdigits)
  (match revdigits
    [(empty) 0]
    [(cons lowdigit higherdigits)
     (+ (* 10 (rev-digits->natural higherdigits))
        lowdigit)]))

(def (natural->rev-digits n)
  (match n
    [(Z) (empty)]
    [n
     (cons (digit (mod n 10))
           (natural->rev-digits (div n 10)))]))

(def (symbol-char? c)
  (and3 (graphical? c)
        (not (delimiter? c))
        (not (unsupported? c))))

(def (delimiter? c)
  (or3 (whitespace? c)
       (paren? c)
       (= (ord c) (ord ";"))))

(def (paren? c)
  (or4 (= (ord c) (ord "("))
       (= (ord c) (ord ")"))
       (= (ord c) (ord "["))
       (= (ord c) (ord "]"))))

(def (unsupported? c)
  (or5 (= (ord c) (ord "\\"))
       (= (ord c) (ord "|"))
       (= (ord c) (ord ","))
       (= (ord c) (ord "\""))
       (= (ord c) (ord "`"))))



; - read example
(read "( ()()((())) )  ")
(read "  (match lst [(cons x xs) hi])  ")
(read "  (match lst [(cons 12345) \"string literal\"])  ")
(read-all "1")
(read-all " 1 ")
(read-all " 1 2 ")
(read-all " 1 2 3")


; parse ...

(def (parse-program sexprs)
  (map parse-stmt sexprs))

(def (parse-stmt sexpr)
  (match sexpr
    [(list "def" (cons name params) body) (DefFun name params (parse-expr body))]
    [(list "def" name expr) (DefVal name (parse-expr expr))]
    [(list "struct" (cons name params)) (DefStruct name params)]
    [sexpr (ToplevelExpr (parse-expr sexpr))]))

(def (parse-expr sexpr)
  (match (string? sexpr)
    [(true)  (Var sexpr)]
    [(false)
     (match sexpr
       [(SelfQuoting v)  (Quote v)]
       [(list "error" (SelfQuoting msg))  (Error msg)]
       [(cons "match" (cons scrutinee cases))  (Match (parse-expr scrutinee)
                                                      (map parse-case cases))]
       [(cons func args)  (Call (parse-expr func)
                                (map parse-expr args))])]))

(def (parse-case sexpr)
  (match sexpr
    [(list pat expr)  (Case (parse-pat pat)
                            (parse-expr expr))]))

(def (parse-pat sexpr)
  (match (string? sexpr)
    [(true)  (PatHole sexpr)]
    [(false)
     (match sexpr
       [(SelfQuoting v)  (PatLitr v)]
       [(cons cname args) (PatCtor cname (map parse-pat args))])]))


; TODO validate ...



; desugar match??
; translate each pattern to an expression that returns (false) or a list


; translate to JS ...

(def (gen-program stmts)
  ; generate a sequence of JS statements, as one string
  (string-append* (map gen-stmt stmts)))

(def (gen-stmt stmt)
  ; generate a JS statement, as a string
  (match stmt
    [(ToplevelExpr e) (string-append (gen-expr e) ";\n")]
    [(DefVal name e) (string-append*
                      (list
                       "const "
                       (emit-name name)
                       " = "
                       (gen-expr e)
                       ";\n"))]
    [(DefFun name params body) (string-append*
                                (list
                                 "function "
                                 (emit-name name)
                                 "("
                                 (commas params)
                                 ") { return "
                                 (gen-expr body)
                                 "; }\n"))]
    [(DefStruct name params) (error "TODO structs")]))

(def (gen-expr expr)
  ; generate a JS expr, as a string
  (match expr
    [(Quote v) (emit-quoted-constant v)]
    [(Var name) (emit-name name)]
    [(Error msg) (emit-error msg)]
    [(Call func args) (emit-call (gen-expr func)
                                 (map gen-expr args))]
    [(Match test
            (list
             (Case (PatCtor "true" (empty)) consq)
             (Case (PatCtor "false" (empty)) alt)))
     (emit-if (gen-expr test)
              (gen-expr consq)
              (gen-expr alt))]))

(def (emit-quoted-constant v)
  (match (string? v)
    [(true) (emit-quoted-string v)]
    [(false)
     (match (natural? v)
       [(true) (emit-natural v)])]))

(def (emit-quoted-string s)
  ; emit a JS expression that evaluates to the same string as s.
  ; for now, escapes are not supported!
  (string-append* (list "\"" s "\"")))

(def (emit-natural v)
  ; represent nats as JS numbers.
  ; it's fine if they don't get too big.
  (natural->string v))

(def (emit-call func args)
  ; func and args are already JS expressions (strings).
  (string-append*
   (list "((1,"
         func
         ")("
         (commas args)
         "))")))

(def (commas strings)
  (match strings
    [(empty) ""]
    [(list last) last]
    [(cons x xs) (string-append* (list x ", " (commas xs)))]))

(def (emit-name name)
  (match (andmap alpha? (string-chars name))
    [(true) name]
    [(false) (error "TODO escape names to JS ids")]))

(def (emit-error msg)
  (string-append*
   (list "( (() => { throw " (emit-quoted-string msg) "; })() )")))

(def (emit-if test consq alt)
  (string-append* (list "(" test "?" consq ":" alt ")")))



(gen-expr (parse-expr (read-one "  (add 2 3)  ")))
(gen-expr (parse-expr (read-one "  (error \"ouch\")  ")))
(gen-expr (parse-expr (read-one "  (match (f x) [(true) ok] [(false) bad])")))

(gen-program
 (parse-program
  (read-all

   "(def x 1) (def y 2) (add x y) (def (add x y) (plus x y))"

   )))
