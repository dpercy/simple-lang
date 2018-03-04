#lang racket

(provide (rename-out [sl:#%module-begin #%module-begin]
                     [sl:parens #%app])
         Cons
         True
         False
         Empty
         Void
         add
         sub
         mul
         equal
         less
         lessEq
         ord
         chr
         slice
         strlen
         strcat
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
    #:datum-literals (\;)
    [(_
      ; Grab "fragments" (consecutive readable non-semicolons),
      ; separated by semicolons.
      ; Parse each fragment as a statement.
      \; ...
      (~seq (~and (~not \;) fragments) ...+ \; ...+) ...
      )

     #'(#%module-begin
        (statement fragments ...) ...
        )]))


(define-syntax (statement stx)
  (syntax-parse stx
    #:datum-literals (= ! struct import check)
    [(_ import name:id) #'(sl:import name)]
    [(_ struct name:id field:id ...) #'(sl:struct name field ...)]
    [(_ check left ...+ = right ...+)
     (let ([the-check #'(check-equal? (sl:parens left ...)
                                      (sl:parens right ...))])
       ; use the caller's srcloc on the check
       (with-syntax ([the-check
                      (datum->syntax #'()
                                     (syntax-e the-check)
                                     ; note we have to really destructure the input manually,
                                     ; can't do #'check to access that keyword,
                                     ; because that would capture a *new* srcloc
                                     (cadr
                                      (syntax->list stx)))])
         #'(module+ test
             (require rackunit)
             the-check)))]
    [(_ x:id = expr ...)  #'(define/pub x (sl:parens expr ...))]
    [(_ f:id x:id ...+ ! = expr ...)

     (with-syntax ([f-body (format-symbol "~a-body" #'f)])
       #'(define/pub f
           (lambda (x ...)
             (Proc (local [(define (f-body)
                             (sl:parens expr ...))]
                     f-body)))))

     ]
    [(_ f:id ! = expr ...)

     (with-syntax ([f-body (format-symbol "~a-body" #'f)])
       #'(define/pub f
           (Proc (local [(define (f-body)
                           (sl:parens expr ...))]
                   f-body))))

     ]
    [(_ f:id x:id ...+ = expr ...)  #'(define/pub f
                                        (lambda (x ...)
                                          (sl:parens expr ...)))]
    [(_ expr ...) #'(#%expression (sl:parens expr ...))]))

(define-syntax inner-statement
  (syntax-parser
    #:datum-literals (=)
    ; the only local statements are defval and expr:
    ; no struct, import, provide, func, proc...
    [(_ x:id = expr ...)  #'(define x (sl:parens expr ...))]
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
       (datum->syntax #'name `(,#'require (,#'prefix-in ,prefix                                                                                           (,#'path-up ,filename)
                                                        ))))]))


(define-syntax sl:parens
  (syntax-parser
    #:datum-literals (|;|)
    ; Filter out all semicolons; they have no significance inside parens.
    [(_ (~alt |;| fragment) ...)
     #'(parens-no-semicolons fragment ...)]))
(define-syntax parens-no-semicolons
  (syntax-parser
    ; TODO better syntax for: if, and, or?
    #:datum-literals (match ! |;| #%braces if and or)
    [(_ (#%braces
         ; Grab "fragments" (consecutive readable non-semicolons),
         ; separated by semicolons.
         ; Parse each fragment as a statement.
         \; ...
         (~seq (~and (~not \;) fragments) ...+ \; ...+) ...
         ))

     #'(let ()
         ; similar to sl:#%module-begin, we're just using semicolons to
         ; delimit fragments of code and group them into statements.
         (inner-statement fragments ...) ...
         )]

    ; TODO enforce calls only happen in def proc?
    [(_ form0 form ... !) #'(Proc-call! (parens-no-semicolons form0 form ...))]
    [(_ match form ...) #'(sl:match form ...)]
    [(_ if test consq alt)
     #'(sl:match test {#%braces \; True => consq \; False => alt \; })]
    [(_ and x y) #'(parens-no-semicolons if x y False)]
    [(_ or x y) #'(parens-no-semicolons if x True y)]
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
         \; ...
         (~describe "case"
                    (~seq (~describe "left-hand-side"
                                     (~seq (~and (~not \;) (~not =>) lhs-forms) ...+))
                          =>
                          \; ...
                          (~describe "right-hand-side"
                                     (~seq (~and (~not \;) (~not =>) rhs-forms) ...+))
                          \; ...))
         ...})

     #'(match (sl:parens arg-forms ...)
         [(sl:pattern lhs-forms ...) (sl:parens rhs-forms ...)] ...
         )]
    [(_ arg-forms ... {#%braces stuff ...})
     (raise-syntax-error 'sl:match
                         "malformed stuff in braces"
                         #'(stuff ...)
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
(define True #true)
(define False #false)
(define Empty '())
(define Void (void))

(define (add x y) (+ x y))
(define (sub x y) (- x y))
(define (mul x y) (* x y))
(define (equal x y) (equal? x y))
(define (less x y) (< x y))
(define (lessEq x y) (<= x y))

(define (ord s)
  (match (string->list s)
    [(list c) (char->integer c)]))

(define (chr i)
  (list->string (list (integer->char i))))

(define (slice s start end) (substring s start end))
(define (strlen s) (string-length s))
(define (strcat x y) (string-append x y))
