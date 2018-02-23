#lang racket

(provide (rename-out [sl:#%module-begin #%module-begin]
                     [sl:parens #%app]
                     )
         #%datum
         )

(require (for-syntax syntax/parse))
(require racket/splicing)

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
    #:datum-literals (= struct)
    [(_ struct name:id field:id ...) #'(sl:struct name field ...)]
    [(_ x:id = expr)  #'(define x expr)]
    [(_ f:id x:id ... = expr)  #'(define (f x ...) expr)]
    [(_ expr ...) #'(#%expression (sl:parens expr ...))]))


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
         (splicing-local [(struct name () #:transparent
                            #:constructor-name make-it)]
           (define name (make-it))))]
    [(_ name:id field:id ...)

     #'(struct name (field ...) #:transparent)]))

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
  (define (identifier-capitalized? x)
    (char-upper-case? (string-ref (symbol->string (syntax-e x)) 0))))
