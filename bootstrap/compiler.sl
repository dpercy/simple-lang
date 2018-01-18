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

; an sexpr is either:
; - (empty)
; - (cons sexpr sexpr)
; - a string, representing an identifier
; - (SelfQuoting value)

(struct (VS value string))
(struct (SelfQuoting value))

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
  (match (whitespace? (substring s 0 1))
    [(true) (drop-whitespace (substring* s 1))]
    [(false) s]))

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

(def (digit-value c)
  (- c (ord "0")))

(def (rev-digits->natural revdigits)
  (match revdigits
    [(empty) 0]
    [(cons lowdigit higherdigits)
     (+ (* 10 (rev-digits->natural higherdigits))
        lowdigit)]))

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


; parse ...


; validate ...
; translate to JS ...
