#lang racket

;; read-square-bracket-with-tag  #%brackets
;; read-curly-brace-with-tag     #%braces
;; read-accept-cdot              foo.bar --> (#%dot foo bar)


(define (make-full-readtable orig-readtable)
  (let* ([rt orig-readtable]
         [rt (make-significant-semicolon-readtable rt)]
         [rt (make-significant-newline-readtable rt)]
         [rt (make-curly-block-readtable rt)]
         [rt (make-round-paren-readtable rt)])
    rt))
(module+ test
  (check-equal? (syntax->datum
                 (parameterize ([current-readtable (make-full-readtable (current-readtable))])
                   (call-with-input-string "{# foo\n{\n  1\n  2\n}\n(\n{}\n)}"
                                           (lambda (port) (read-syntax "in" port)))))
                '(\; (\; 1 \; 2 \;) \; (()))))


(define (make-significant-semicolon-readtable orig-readtable)
  (make-readtable orig-readtable
                  #\;
                  'terminating-macro
                  (lambda (char in src ln col pos)
                    ;; TODO include srcloc
                    '\;))
  )
(module+ test
  (require rackunit)
  (check-equal? (syntax->datum (parameterize ([current-readtable (make-significant-semicolon-readtable (current-readtable))])
                                 (call-with-input-string "(1;2;)"
                                                         (lambda (port) (read-syntax "in" port)))))
                '(1 \; 2 \;)))


(define (make-significant-newline-readtable orig-readtable)
  (make-readtable (make-readtable orig-readtable
                                  #\newline
                                  'terminating-macro
                                  (lambda (char in src ln col pos)
                                    ;; TODO include srcloc
                                    '\;))
                  ;; we also have to override # for comments
                  #\#
                  'terminating-macro
                  (lambda (char in src ln col pos)
                    (read-line in) ; discard
                    '\;)))
(module+ test
  (require rackunit)
  (check-equal? (syntax->datum (parameterize ([current-readtable (make-significant-newline-readtable (current-readtable))])
                                 (call-with-input-string "(\n1\n2\n)"
                                                         (lambda (port) (read-syntax "in" port)))))
                '(\; 1 \; 2 \;))

  ; how do significant newlines interact with comments?
  (check-equal? (syntax->datum (parameterize ([current-readtable (make-significant-newline-readtable (current-readtable))])
                                 (call-with-input-string "\n foo"
                                                         (lambda (port) (read-syntax "in" port)))))
                '\;)
  (check-equal? (syntax->datum (parameterize ([current-readtable (make-significant-newline-readtable (current-readtable))])
                                 (call-with-input-string "# comment\n foo"
                                                         (lambda (port) (read-syntax "in" port)))))
                '\;)
  (check-equal? (syntax->datum (parameterize ([current-readtable (make-significant-newline-readtable (current-readtable))])
                                 (call-with-input-string "(# comment\n foo)"
                                                         (lambda (port) (read-syntax "in" port)))))
                '(\; foo))
  )


(define (make-insignificant-newline-readtable orig-readtable)
  (make-readtable
   (make-readtable orig-readtable
                   #\newline
                   #\newline
                   #false)
   ; make # behave like plain ignored comments
   #\#
   #\;
   #false))
(module+ test
  (require rackunit)
  (check-equal? (syntax->datum (parameterize ([current-readtable (make-insignificant-newline-readtable
                                                                  (make-significant-newline-readtable
                                                                   (current-readtable)))])
                                 (call-with-input-string "(\n1\n#foo\n2\n)"
                                                         (lambda (port) (read-syntax "in" port)))))
                '(1 2)))


; defines round and square parens to ignore newlines
(define (make-round-paren-readtable orig-readtable)
  (define (proc char in src ln col pos)
    (read-syntax/recursive src in char
                           (make-insignificant-newline-readtable orig-readtable)))
  (let* ([rt orig-readtable]
         [rt (make-readtable rt
                             #\[
                             'terminating-macro
                             proc)]
         [rt (make-readtable rt
                             #\(
                             'terminating-macro
                             proc)])
    rt))


; defines curly parens to treat newlines as semicolons
(define (make-curly-block-readtable orig-readtable)
  (make-readtable orig-readtable
                  #\{
                  'terminating-macro
                  (lambda (char in src ln col pos)
                    ; TODO include srcloc of lst in result? or loc of ln col pos?
                    (read-syntax/recursive src
                                           in
                                           char
                                           (make-significant-newline-readtable orig-readtable)))))
