#lang racket

(provide (rename-out [sl:#%module-begin #%module-begin]
                     [sl:parens #%app]
                     )
         #%datum
         )

(require (for-syntax syntax/parse
                     racket/syntax))
(require racket/splicing
         racket/require
         )

(define-syntax sl:#%module-begin
  (syntax-parser
    #:datum-literals (|;|)
    [(_
      ; Grab "fragments" (consecutive readable non-semicolons),
      ; separated by semicolons.
      ; Parse each fragment as a statement.
      |;| ...
      (~seq (~seq (~and (~not |;|) fragment) ...)
            |;| ...)
      ...)

     #'(#%module-begin
        (statement fragment ...) ...)]))


(define-syntax statement
  (syntax-parser
    #:datum-literals (= struct import #%braces)
    [(_ import name:id) #'(sl:import name)]
    [(_ struct name:id field:id ...) #'(sl:struct name field ...)]
    [(_ x:id = expr)  #'(begin
                          (provide x)
                          (define x expr))]
    [(_ f:id x:id ... = expr)  #'(define (f x ...) expr)]
    [(_ expr ...) #'(#%expression (sl:parens expr ...))]))


(define-syntax sl:import
  (syntax-parser
    [(_ name:id)
     '''(with-syntax ([filename (datum->syntax #'name (format "~a.sl" (syntax-e #'name)))]
                      [prefix (format-id #'name "~a." #'name)])
          #'(require ;;(prefix-in prefix (path-up filename))
             (path-up filename)
             ))
     (let ([filename (format "~a.sl" (syntax-e #'name))]
           [prefix (format-id #'name "~a." #'name)]
           )
       (datum->syntax #'name `(,#'require (,#'prefix-in "lib."                                                                                           (,#'path-up ,filename)
                                                        ))))]))


(define-syntax sl:parens
  (syntax-parser
    #:datum-literals (|;|)
    ; Filter out all semicolons; they have no significance inside parens.
    [(_ |;| ...
        (~seq (~and (~not |;|) expr)
              |;| ...)
        ...)
     #'(parens-no-semicolons expr ...)]))
(define-syntax parens-no-semicolons
  (syntax-parser
    #:datum-literals (match)
    [(_ match form ...) #'(sl:match form ...)]
    ; Unary parens are fine: they're just grouping.
    [(_ x) #'x]
    ; One or more arguments is curried application.
    [(_ f x0 xs ...) #'(curry f x0 xs ...)]))

(define-syntax sl:struct
  (syntax-parser
    [(_ name:id)
     #'(begin
         (define name (splicing-local [(struct name () #:transparent
                                         #:constructor-name make-it)]
                        (make-it)))
         (provide name))]
    [(_ name:id field:id ...)

     #'(begin
         (struct name (field ...) #:transparent)
         (provide (struct-out name)))]))

(define-syntax sl:match
  (syntax-parser
    #:datum-literals (#%braces => |;|)
    [(_ arg-forms ...
        {#%braces
         |;|...
         (~seq (~and lhs-forms (~not =>) (~not |;| )) ...
               =>
               |;| ...
               (~and rhs-forms (~not =>) (~not |;|)) ...
               |;| ...)
         ...})

     #'(match (sl:parens arg-forms ...)
         [(sl:pattern lhs-forms ...) (sl:parens rhs-forms ...)] ...
         )]))

(define-match-expander sl:pattern
  (syntax-parser
    ; grouping
    [(_ (pat ...)) #'(sl:pattern pat ...)]

    ; structs
    [(_ C:id) #:when (identifier-capitalized? #'C)
     ; since nullary structs are singletons, just use eq
     #'(== C eq?)]
    [(_ C:id x ...) #:when (identifier-capitalized? #'C)
     #'(C (sl:pattern x) ...)]

    ; constants
    [(_ (~or v:string v:boolean v:number)) #'v]

    ; variable / catchall
    [(_ x:id) #:when (not (identifier-capitalized? #'x))
     #'x]))
(begin-for-syntax
  (require (only-in racket/list last)
           (only-in racket/string string-split))
  (define (identifier-capitalized? x)
    (define str (symbol->string (syntax-e x)))
    (define toks (string-split str "."))
    (define lasttok (last toks))
    (char-upper-case? (string-ref lasttok 0))))
