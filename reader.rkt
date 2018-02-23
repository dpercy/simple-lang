#lang racket

(provide (rename-out [read-curly-syntax read-syntax]
                     [read-curly read]))

(module+ test (require rackunit))


(define (read-curly-syntax src in)
  (parameterize ([current-readtable (make-full-readtable (current-readtable))]
                 [read-square-bracket-with-tag #true]
                 [read-curly-brace-with-tag #true]
                 [read-cdot #true])
    (read-syntax src in)))
(define (read-curly in)
  (syntax->datum (read-curly-syntax #false in)))
(module+ test
  (check-equal? (call-with-input-string "foo" read-curly)
                'foo)
  (check-equal? (call-with-input-string "(foo)" read-curly)
                '(foo))
  (check-equal? (call-with-input-string "(foo;)" read-curly)
                '(foo |;|))
  (check-equal? (call-with-input-string "{1\n2}" read-curly)
                '(#%braces 1 |;| 2))
  (check-equal? (call-with-input-string "{()\n2}" read-curly)
                '(#%braces () |;| 2)))

(define (make-full-readtable orig-readtable)
  (let* ([rt orig-readtable]
         [rt (make-significant-semicolon-readtable rt)]
         [rt (make-significant-newline-readtable rt)])
    rt))
(module+ test
  (check-equal? (syntax->datum
                 (parameterize ([current-readtable (make-full-readtable (current-readtable))])
                   (call-with-input-string "{# foo\n{\n  1\n  2\n}\n(\n{}\n)}"
                                           (lambda (port) (read-syntax "in" port)))))
                '(\; (\; 1 \; 2 \;) \; ( \; ()\;))))


(define (make-significant-semicolon-readtable orig-readtable)
  (make-readtable orig-readtable
                  #\;
                  'terminating-macro
                  (lambda (char in src ln col pos)
                    ;; TODO include srcloc
                    '\;))
  )
(module+ test
  (check-equal? (syntax->datum (parameterize ([current-readtable (make-significant-semicolon-readtable (current-readtable))])
                                 (call-with-input-string "(1;2;)"
                                                         (lambda (port) (read-syntax "in" port)))))
                '(1 \; 2 \;)))


(define (make-significant-newline-readtable orig-readtable)
  (make-readtable
   (make-readtable orig-readtable
                   #\newline
                   'terminating-macro
                   (lambda (char in src ln col pos)
                     ;; TODO include srcloc
                     '|;|))
   #\#
   'terminating-macro
   (lambda (char in src ln col pos)
     (read-line in)
     '|;|)))
(module+ test
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
  (check-equal? (syntax->datum (parameterize ([current-readtable (make-insignificant-newline-readtable
                                                                  (make-significant-newline-readtable
                                                                   (current-readtable)))])
                                 (call-with-input-string "(\n1\n#foo\n2\n)"
                                                         (lambda (port) (read-syntax "in" port)))))
                '(1 2)))
