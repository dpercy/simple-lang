#lang racket

(provide (rename-out [sl:#%module-begin #%module-begin]
                     [sl:parens #%app]
                     [true True]
                     [false False]
                     [empty Empty])
         Cons
         add
         equal
         less
         #%datum
         (struct-out Done)
         (struct-out Perform)
         perform
         capture)

(require (for-syntax syntax/parse
                     racket/syntax))
(require racket/splicing
         racket/require
         racket/control
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
    #:datum-literals (= ! struct import #%braces)
    [(_ import name:id) #'(sl:import name)]
    [(_ struct name:id field:id ...) #'(sl:struct name field ...)]
    [(_ x:id = expr ...)  #'(define/pub x (sl:parens expr ...))]
    [(_ f:id x:id ... ! = expr ...)

     (with-syntax ([f-body (format-symbol "~a-body" #'f)])
       #'(define/pub f
           (lambda (x ...)
             (Proc (local [(define (f-body)
                             (sl:parens expr ...))]
                     f-body)))))

     ]
    [(_ f:id x:id ... = expr ...)  #'(define/pub f
                                       (lambda (x ...)
                                         (sl:parens expr ...)))]
    [(_ expr ...) #'(#%expression (sl:parens expr ...))]))

(define-syntax define/pub
  (syntax-parser
    [(_ name:id val) #'(begin
                         (provide name)
                         (define name val))]))


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
    #:datum-literals (match !)
    ; TODO enforce calls only happen in def proc?
    [(_ form ... !) #'(Proc-call! (parens-no-semicolons form ...))]
    [(_ match form ...) #'(sl:match form ...)]
    ; Unary parens are fine: they're just grouping.
    [(_ x) #'x]
    ; One or more arguments is curried application.
    [(_ f x0 xs ...) #'(parens-no-semicolons (app f x0) xs ...)]))

(define/contract (app f arg) (-> procedure? any/c any/c)
  (match (sub1 (procedure-arity f))
    [0 (f arg)]
    [(? positive-integer? new-arity)
     (procedure-reduce-arity (curry f arg) new-arity)]
    [_ (error 'app "bad func: ~v ; arg: ~v" f arg)]))

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


(struct Done (value) #:transparent)
(struct Perform (effect continuation) #:transparent)

(struct Proc (code) #:transparent)

(define/contract (Proc-call! proc) (-> Proc? any/c)
  (match proc
    [(Proc code) (code)]))

(define (perform effect)
  (Proc (lambda ()
          (fcontrol effect))))

(define (continuation->proc k)
  ; k is a 1-arg Racket callable.
  (lambda (v)
    (Proc (lambda () (k v)))))

(define/contract (capture proc) (-> Proc? any/c)
  ; If running the code returns normally,
  ; wrap the result in a Done.
  ; If the code calls "perform",
  ; then a Perform is returned instead.
  (let/ec return
    ; Done goes outside the prompt to avoid
    ; capturing it as part of the continuation.
    (Done (% (Proc-call! proc)
             (lambda (eff k)
               (return (Perform eff (continuation->proc k))))))))


(define-match-expander Cons
  (syntax-rules () [(_ x xs) (cons x xs)])
  (make-rename-transformer #'cons))

(define (add x y) (+ x y))
(define (equal x y) (equal? x y))
(define (less x y) (< x y))
