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
  (match s
    ["" ""]
    [s
     (match (substring s 0 1)
       [";" (drop-whitespace (drop-comment s))]
       [c
        (match (whitespace? c)
          [(true) (drop-whitespace (substring* s 1))]
          [(false) s])])]))

(def (drop-comment s)
  (match s
    ["" ""]
    [s
     (match (substring s 0 1)
       ["\n" (substring* s 1)]
       [c (drop-comment (substring* s 1))])]))

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
    ["\\" (read-string/escape (substring* s 1))]
    ["\n" (error "multiline strings are not supported")]
    [c (match (read-string (substring* s 1))
         [(VS t s) (VS (string-append c t) s)])]))

(def (read-string/escape s)
  (match (match (substring s 0 1)
           ; keep me in sync with char-escape
           ["n" "\n"]
           ["t" "\t"]
           ["\"" "\""]
           ["\\" "\\"]
           [c (error "unrecognized escape")])
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
;(read "( ()()((())) )  ")
;(read "  (match lst [(cons x xs) hi])  ")
;(read "  (match lst [(cons 12345) \"string literal\"])  ")
;(read-all "1")
;(read-all " 1 ")
;(read-all " 1 2 ")
;(read-all " 1 2 3")


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


; translate to JS ...

(def (gen-program stmts)
  ; generate a sequence of JS statements, as one string
  (string-append* (map gen-stmt stmts)))

(def (gen-stmt stmt)
  ; generate a JS statement, as a string
  (match stmt
    [(ToplevelExpr e)
     (string-append* (list "console.log(" (gen-expr e) ");\n"))]
    [(DefVal name e) (string-append*
                      (list
                       "export const "
                       (emit-name name)
                       " = "
                       (gen-expr e)
                       ";\n"))]
    [(DefFun name params body) (string-append*
                                (list
                                 "export function "
                                 (emit-name name)
                                 "("
                                 (commas (map emit-name params))
                                 ") { return "
                                 (gen-expr body)
                                 "; }\n"))]

    [(DefStruct name params)
     (match (map emit-name (cons name params))
       [(cons name params)
        (string-append*
         (list
          "export function " name "(" (commas params) ") {\n"
          "  if (!(this instanceof " name ")) return new " name "(" (commas params) ");\n"
          (emit-constructor-body params 0)
          "}\n"))])]))

(def (emit-constructor-body params idx)
  (match params
    [(empty) ""]
    [(cons p params)
     (string-append*
      (list
       "  this["
       (natural->string idx)
       "] = "
       p
       ";\n"
       (emit-constructor-body params (+ idx 1))))]))

(def (gen-expr expr)
  ; generate a JS expr, as a string
  (match expr
    [(Quote v) (emit-quoted-constant v)]
    [(Var name) (emit-name name)]
    [(Error msg) (emit-error msg)]
    [(Call func args) (emit-call (gen-expr func)
                                 (map gen-expr args))]
    [(Match scrut cases)
     (string-append*
      (list
       "( (() => {\n"
       "const scrut = " (gen-expr scrut) ";\n"
       (string-append* (map gen-case cases))

       (gen-expr (Error "match: no case matched")) ";\n"

       "})() );\n"))]))

(def (gen-case case)
  (match case
    [(Case pat expr)
     (string-append*
      (list
       ; The whole case is wrapped in a do{}while(0),
       ; so whenever a pattern fails you can use `break` to jump to the next case.
       "do {\n"
       (gen-pat "scrut" pat)
       "return " (gen-expr expr) ";\n"
       "} while(0);\n"
       ))]))

(def (gen-pat scrut pat)
  (match pat
    [(PatHole name)
     (string-append*
      (list
       "const " (emit-name name) " = " scrut ";\n"
       ;;
       ))]
    [(PatLitr v)
     (string-append*
      (list
       "if (" scrut " !== " (emit-quoted-constant v) ") break;\n"
       ;;
       ))]
    [(PatCtor cname args)
     (string-append*
      (list
       "if (!(" scrut " instanceof " (emit-name cname) ")) break;\n"
       (gen-pat-args scrut 0 args)
       ;;
       ))]))

(def (gen-pat-args scrut idx pats)
  (match pats
    [(empty) ""]
    [(cons pat pats)
     (string-append
      (gen-pat (string-append* (list scrut "[" (natural->string idx) "]")) pat)
      (gen-pat-args scrut (+ 1 idx) pats))]))

(def (emit-quoted-constant v)
  (match (string? v)
    [(true) (emit-quoted-string v)]
    [(false)
     (match (natural? v)
       [(true) (emit-natural v)])]))

(def (emit-quoted-string s)
  ; emit a JS expression that evaluates to the same string as s.
  ; for now, escapes are not supported!
  (string-append* (list "\""
                        (string-append* (map char-escape (string-chars s)))
                        "\"")))

(def (char-escape c)
  (match c
    ; keep me in sync with read-string/escape
    ["\n" "\\n"]
    ["\t" "\\t"]
    ["\"" "\\\""]
    ["\\" "\\\\"]
    [c c]))

(def (emit-natural v)
  ; represent nats as JS numbers.
  ; it's fine if they don't get too big.
  (natural->string v))

(def (emit-call func args)
  ; func and args are already JS expressions (strings).
  (string-append*
   (list func
         "("
         (commas args)
         ")")))

(def (commas strings)
  (match strings
    [(empty) ""]
    [(list last) last]
    [(cons x xs) (string-append* (list x ", " (commas xs)))]))

(def (emit-name name)
  ; TODO safer escaping
  ; 1. prefix all generated names with $. this ensures:
  ;   - no generated name starts with a digit
  ;   - no generated name collides with a keyword
  ;   - any name not starting with a $ is reserved for the compiler
  ; 2. replace any non-alphanumeric character with an escape code
  (string-append* (cons "$" (map identifier-char-escape (string-chars name)))))

(def (identifier-char-escape c)
  (match c
    ["-" "_"]
    [c
     (match (alphanumeric? c)
       [(true) c]
       [(false)
        (string-append* (list "$" (natural->string (ord c)) "$"))])]))

(def (emit-error msg)
  (string-append*
   (list "( (() => { throw " (emit-quoted-string msg) "; })() )")))



;(gen-expr (parse-expr (read-one "  (add 2 3)  ")))
;(gen-expr (parse-expr (read-one "  (error \"ouch\")  ")))
;(gen-expr (parse-expr (read-one "  (match (f x) [(true) ok] [(false) bad])")))

;(gen-program (parse-program (read-all "(def x 1) (def y 2) (add x y) (def (add x y) (plus x y))")))


(def (compile-program s)
  (gen-program
   (parse-program
    (read-all s))))
