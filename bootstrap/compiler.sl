#lang s-exp "sl.rkt"

(def mod int.mod)

(def and3 bool.and3)
(def not bool.not)
(def or3 bool.or3)
(def or4 bool.or4)
(def or5 bool.or5)

(def empty list.empty)
(def cons list.cons)
(def map list.map)
(def andmap list.andmap)
(def reverse list.reverse)
(def set-union list.set-union)
(def length list.length)


; statements
(struct (DefVal name expr))
(struct (DefFun name params body))
(struct (DefStruct name params))
(struct (ToplevelExpr expr))

; expressions
(struct (Quote val))
; TODO distinguish Global from Label?
; - Label would be for functions
; - Global would be a redex; Label a value
;   - avoids you needing an env for "value?"-check
(struct (Global modname name))
(struct (Var name))
(struct (Error msg))
(struct (Call func args))
(struct (Match scrutinee cases))

; TODO change these "smart constructors" into real structs
(def (If test consq alt)
  (Match test
         (list (Case (PatLitr #true) consq)
               (Case (PatLitr #false) alt))))

; case - one arm of a match expression
(struct (Case pat expr))

; patterns - left-hand side of a case
(struct (PatLitr name))
(struct (PatHole name))
(struct (PatCtor ctor args))


; helpers for dealing with core syntax

; empty-case : a
; append-case : (a, a) -> a
; base-case : form -> (a | #false)
; form : form
(def (core-fold empty-case append-case base-case form)
  (match (base-case form)
    ; When base-case is #false, try the other cases.
    [#false (match form

              ; list of things
              [(empty) empty-case]
              [(cons form0 forms) (append-case
                                   (core-fold empty-case append-case base-case form0)
                                   (core-fold empty-case append-case base-case forms))]

              ; statements
              [(DefVal name expr)  (core-fold empty-case append-case base-case expr)]
              [(DefFun name params body)  (core-fold empty-case append-case base-case body)]
              [(DefStruct name params)  empty-case]
              [(ToplevelExpr expr)  (core-fold empty-case append-case base-case expr)]

              ; expressions
              [(Quote val)  empty-case]
              [(Global modname name)  empty-case]
              [(Var name)  empty-case]
              [(Error msg)  empty-case]
              [(Call func args)  (core-fold empty-case append-case base-case (cons func args))]
              [(Match scrut cases)  (core-fold empty-case append-case base-case (cons scrut cases))]

              ; case - one arm of a match expression
              [(Case pat expr)  (append-case
                                 (core-fold empty-case append-case base-case pat)
                                 (core-fold empty-case append-case base-case expr))]

              ; patterns - left-hand side of a case
              [(PatLitr name)  empty-case]
              [(PatHole name)  empty-case]
              [(PatCtor ctor args)  (core-fold empty-case append-case base-case (cons ctor args))])]

    ; When base-case is non-#false, return that value and stop traversing.
    [something something]))


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
     (match (string.slice s 0 1)
       ["(" (read-list ")" (string.slice* s 1))]
       ["[" (read-list "]" (string.slice* s 1))]
       [c
        (if (symbol-char? c)
            (read-symbol-or-int s)
            (if (= (ord c) (ord "\""))
                (match (read-string (string.slice* s 1))
                  [(VS t s) (VS (SelfQuoting t) s)])
                ; hack for error reporting: (match c) complains about the value c
                (match c)))])]))

(def (drop-whitespace s)
  (match s
    ["" ""]
    [s
     (match (string.slice s 0 1)
       [";" (drop-whitespace (drop-comment s))]
       [c
        (if (string.whitespace? c)
            (drop-whitespace (string.slice* s 1))
            s)])]))

(def (drop-comment s)
  (match s
    ["" ""]
    [s
     (match (string.slice s 0 1)
       ["\n" (string.slice* s 1)]
       [c (drop-comment (string.slice* s 1))])]))

(def (read-list end s)
  (match (drop-whitespace s)
    [s (if (= (ord end) (ord (string.slice s 0 1)))
           (VS (empty)
               (string.slice* s 1))
           (match (read s)
             [(VS head s)
              (match (read-list end s)
                [(VS tail s)
                 (VS (cons head tail) s)])]))]))

(def (read-string s)
  (match (string.slice s 0 1)
    ["\"" (VS "" (string.slice* s 1))]
    ["\\" (read-string/escape (string.slice* s 1))]
    ["\n" (error "multiline strings are not supported")]
    [c (match (read-string (string.slice* s 1))
         [(VS t s) (VS (string.append c t) s)])]))

(def (read-string/escape s)
  (match (match (string.slice s 0 1)
           ; keep me in sync with char-escape,
           ; and with primitives.mjs showString...
           ["0" "\0"]
           ["r" "\r"]
           ["n" "\n"]
           ["t" "\t"]
           ["\"" "\""]
           ["\\" "\\"]
           ;[c (error "unrecognized escape")]
           )
    [c (match (read-string (string.slice* s 1))
         [(VS t s) (VS (string.append c t) s)])]))


(def (read-symbol-or-int s)
  (match (read-token s)
    [(VS t s) (VS (convert-token t) s)]))

(def (read-token s)
  (match s
    ["" (VS "" s)]
    [s
     (match (string.slice s 0 1)
       [c
        (if (not (symbol-char? c))
            (VS "" s)
            (match (read-token (string.slice* s 1))
              [(VS t s)
               (VS (string.append c t)
                   s)]))])]))

(def (convert-token t)
  (if (andmap string.digit? (string.chars t))
      ; non-signed integer literal
      (SelfQuoting (string->int t))
      (match (list (string.slice t 0 1)
                   (andmap string.digit? (string.chars (string.slice* t 1)))
                   (< 1 (string-length t)))
        ; explicit positive integer literal
        [(list "+" #true #true)  (SelfQuoting (string->int (string.slice* t 1)))]
        ; negative integer literal
        [(list "-" #true #true)  (SelfQuoting (- 0 (string->int (string.slice* t 1))))]
        [otherwise
         ; not an integer literal
         (match t
           ["#true" (SelfQuoting #true)]
           ["#t" (SelfQuoting #true)]
           ["#false" (SelfQuoting #false)]
           ["#f" (SelfQuoting #false)]
           [t t])])))



(def (string->int s)
  (rev-digits->int (map digit-value (reverse (string.chars s)))))

(def (int->string n)
  (if (< n 0)
      (string.append "-" (nat->string (- 0 n)))
      (nat->string n)))

(def (nat->string n)
  (match (string.append* (reverse (int->rev-digits n)))
    ; An "empty int literal" isn't a thing,
    ; so replace it with zero.
    ["" "0"]
    [s s]))

(def (digit-value c)
  (- (ord c) (ord "0")))

(def (digit val)
  (chr (+ val (ord "0"))))

(def (rev-digits->int revdigits)
  (match revdigits
    [(empty) 0]
    [(cons lowdigit higherdigits)
     (+ (* 10 (rev-digits->int higherdigits))
        lowdigit)]))

(def (int->rev-digits n)
  (match n
    [0 (empty)]
    [n
     (cons (digit (mod n 10))
           (int->rev-digits (/ n 10)))]))

(def (symbol-char? c)
  (and3 (string.graphical? c)
        (not (delimiter? c))
        (not (unsupported? c))))

(def (delimiter? c)
  (or3 (string.whitespace? c)
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
  (if (string? sexpr)
      (parse-var sexpr)
      (match sexpr
        [(SelfQuoting v)  (Quote v)]
        [(list "error" (SelfQuoting msg))  (Error msg)]
        [(cons "match" (cons scrutinee cases))  (Match (parse-expr scrutinee)
                                                       (map parse-case cases))]
        [(list "if" test consq alt) (If (parse-expr test)
                                        (parse-expr consq)
                                        (parse-expr alt))]

        ; syntax sugar for "list"
        [(list "list")  (parse-expr (list "empty"))]
        [(cons "list" (cons x xs))  (parse-expr (list "cons" x (cons "list" xs)))]


        ; function call case must come last
        [(cons func args)  (Call (parse-expr func)
                                 (map parse-expr args))])))

(def (parse-var str)
  (match (string.split str ".")
    [(list name) (Var name)]
    [(list modname name) (Global modname name)]
    [parts (error "identifier has too many dots")]))

(def (parse-case sexpr)
  (match sexpr
    [(list pat expr)  (Case (parse-pat pat)
                            (parse-expr expr))]))

(def (parse-pat sexpr)
  (if (string? sexpr)
      (PatHole sexpr)
      (match sexpr
        [(SelfQuoting v)  (PatLitr v)]

        ; syntax sugar for "list"
        [(list "list") (parse-pat (list "empty"))]
        [(cons "list" (cons x xs)) (parse-pat (list "cons" x (cons "list" xs)))]

        [(cons ctor args) (PatCtor (parse-expr ctor) (map parse-pat args))])))


; TODO validate ...


; translate to JS ...

(def (gen-program stmts)
  ; generate a sequence of JS statements, as one string
  (string.append* (list
                   (prelude)
                   (string.append* (map gen-import (find-imports stmts)))
                   (string.append* (map gen-stmt stmts)))))

(def prelude-names
  (list
   "boolean?"
   "int?"
   "+"
   "-"
   "*"
   "/"
   "<"
   "="
   "string?"
   "string=?"
   "string-append"
   "string-length"
   "substring"
   "ord"
   "chr"
   "equal?"
   ;;
   ))

(def (prelude)
  (string.append*
   (list
    "import { toplevel, bigInt, isA, "
    (commas (map emit-name prelude-names))
    " } from \"./primitives.mjs\";\n")))

(def (gen-import modname)
  (string.append*
   (list
    "import * as " (emit-name modname)
    " from " (emit-quoted-string (string.append* (list "./" modname ".mjs")))
    ";\n")))

(def (find-imports form)
  (core-fold (empty)
             set-union
             find-imports/base-case
             form))
(def (find-imports/base-case form)
  (match form
    [(Global modname name) (list modname)]
    [_ #false]))

(def (gen-stmt stmt)
  ; generate a JS statement, as a string
  (match stmt
    [(ToplevelExpr e)
     (string.append* (list "toplevel(false, () => " (gen-expr e) ");\n"))]
    [(DefVal name e) (string.append*
                      (list
                       "export const "
                       (emit-name name)
                       " = toplevel("
                       (emit-quoted-string name)
                       ", () => " (gen-expr e) ");\n"))]
    [(DefFun name params body) (string.append*
                                (list
                                 "export function "
                                 (emit-name name)
                                 "("
                                 (commas (map emit-name params))
                                 ") {\n"
                                 (emit-arity-check name (length params))
                                 "return " (gen-expr body) ";\n"
                                 "}\n"
                                 (emit-name name)
                                 ".schemeName = " (emit-quoted-string name) ";\n"))]

    [(DefStruct name params)
     (match (cons name (map emit-name (cons name params)))
       [(cons orig-name (cons name params))
        (string.append*
         (list
          "export function " name "(" (commas params) ") {\n"
          ; Arity check before `this` check,
          ; because the inner call using `new` always has the correct arity.
          (emit-arity-check orig-name (length params))
          "  if (!(this instanceof " name ")) return new " name "(" (commas params) ");\n"
          (emit-constructor-body params 0)
          "}\n"
          name ".schemeName = " (emit-quoted-string orig-name) ";\n"))])]))

(def (emit-arity-check name expected-num-args)
  (string.append*
   (list
    "  if (arguments.length !== " (int->string expected-num-args) ")"
    " throw (" (emit-quoted-string (string.append* (list name
                                                         ": expected "
                                                         (int->string expected-num-args)
                                                         " arguments, but got ")))
    " + arguments.length);\n")))

(def (emit-constructor-body params idx)
  (match params
    [(empty) ""]
    [(cons p params)
     (string.append*
      (list
       "  this["
       (int->string idx)
       "] = "
       p
       ";\n"
       (emit-constructor-body params (+ idx 1))))]))

(def (wrap-check-undefined name js-expr)
  ; TODO try something more like "fixing letrec", or dep-graph sorting, for efficiency
  (string.append* (list
                   "(undefined === "
                   js-expr
                   "?"
                   (gen-expr (Error (string.append*
                                     (list name " is not defined"))))
                   ":"
                   js-expr
                   ")")))

(def (gen-expr expr)
  ; generate a JS expr, as a string
  (match expr
    [(Quote v) (emit-quoted-constant v)]
    [(Var name) (wrap-check-undefined name (emit-name name))]
    [(Global mod name) (wrap-check-undefined
                        name
                        (string.append*
                         (list (emit-name mod) "." (emit-name name))))]
    [(Error msg) (emit-error msg)]
    [(Call func args) (emit-call (gen-expr func)
                                 (map gen-expr args))]
    [(Match scrut cases)
     (string.append*
      (list
       "( (() => {\n"
       "const scrut = " (gen-expr scrut) ";\n"
       (string.append* (map gen-case cases))

       (gen-expr (Error "match: no case matched")) ";\n"

       "})() )"))]))

(def (gen-case case)
  (match case
    [(Case pat expr)
     (string.append*
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
     (string.append*
      (list
       "const " (emit-name name) " = " scrut ";\n"
       ;;
       ))]
    [(PatLitr v)
     (string.append*
      (list
       "if (!$equal$63$(" scrut ", " (emit-quoted-constant v) ")) break;\n"
       ;;
       ))]
    [(PatCtor ctor args)
     ; TODO prevent PatHole from shadowing a PatCtor!
     ; - instead, compile a pattern into 2 phases:
     ;   1. a check phase: one big "&&" expression on "scrut[1][0][2]" paths
     ;   2. a bind phase: sequence of "const x = ..." statements
     (string.append*
      (list
       "if (!(isA(" scrut ", " (int->string (length args)) ", " (gen-expr ctor) "))) break;\n"
       (gen-pat-args scrut 0 args)
       ;;
       ))]))

(def (gen-pat-args scrut idx pats)
  (match pats
    [(empty) ""]
    [(cons pat pats)
     (string.append
      (gen-pat (string.append* (list scrut "[" (int->string idx) "]")) pat)
      (gen-pat-args scrut (+ 1 idx) pats))]))

(def (emit-quoted-constant v)
  (if (string? v)
      (emit-quoted-string v)
      (if (int? v)
          (emit-int v)
          (if (boolean? v)
              (emit-bool v)
              (match v)))))

(def (emit-quoted-string s)
  ; emit a JS expression that evaluates to the same string as s.
  ; for now, escapes are not supported!
  (string.append* (list "\""
                        (string.append* (map char-escape (string.chars s)))
                        "\"")))

(def (char-escape c)
  (match c
    ; keep me in sync with read-string/escape,
    ; and with primitives.mjs showString...
    ["\0" "\\0"]
    ["\r" "\\r"]
    ["\n" "\\n"]
    ["\t" "\\t"]
    ["\"" "\\\""]
    ["\\" "\\\\"]
    [c c]))

(def (emit-int v)
  ; represent ints as bigInt instances
  (string.append* (list "bigInt(" (emit-quoted-string (int->string v)) ")")))

(def (emit-bool b)
  (if b
      "true"
      "false"))

(def (emit-call func args)
  ; func and args are already JS expressions (strings).
  (string.append*
   (list func
         "("
         (commas args)
         ")")))

(def (commas strings)
  (match strings
    [(empty) ""]
    [(list last) last]
    [(cons x xs) (string.append* (list x ", " (commas xs)))]))

(def (emit-name name)
  ; 1. prefix all generated names with $. this ensures:
  ;   - no generated name starts with a digit
  ;   - no generated name collides with a keyword
  ;   - any name not starting with a $ is reserved for the compiler
  ; 2. replace any non-alphanumeric character with an escape code
  (string.append* (cons "$" (map identifier-char-escape (string.chars name)))))

(def (identifier-char-escape c)
  (match c
    ["-" "_"]
    [c
     (if (string.alphanumeric? c)
         c
         (string.append* (list "$" (int->string (ord c)) "$")))]))

(def (emit-error msg)
  (string.append*
   (list "( (() => { throw " (emit-quoted-string msg) "; })() )")))



;(gen-expr (parse-expr (read-one "  (add 2 3)  ")))
;(gen-expr (parse-expr (read-one "  (error \"ouch\")  ")))
;(gen-expr (parse-expr (read-one "  (match (f x) [#true ok] [#false bad])")))

;(gen-program (parse-program (read-all "(def x 1) (def y 2) (add x y) (def (add x y) (plus x y))")))


(def (compile-program s)
  (gen-program
   (parse-program
    (read-all s))))
