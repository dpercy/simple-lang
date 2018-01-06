#lang racket

(provide run-program/concurrent)

(require racket/async-channel)

(require "core-syntax.rkt")
(require "semantics.rkt")

#|

run-program/sequential produces a sequence of results,
where each result is yielded as it is computed.

run-program/concurrent must also yield each result as it is computed,
but the results might come out in any order.

TODO handle missing globals not by hanging

TODO handle errors as a Result

|#
(define/contract (run-program/concurrent blocks globals) (-> (listof Block?) (hash/c symbol? any/c) (sequence/c Result?))
  #|

  Here's the strategy:
  - find all runnable blocks (a block is runnable when all its globals are known)
  - run them in parallel
  - whenever a block finishes:
  .   1. async yield the results
  .   2. update the globals
  .   3. start all the new runnable blocks
  -

  |#

  (define/contract results-out (async-channel/c (or/c 'done Result?)) (make-async-channel))
  (define/contract result-batches-done (async-channel/c (listof Result?)) (make-async-channel))

  ; main background thread: terminates once all worker threads have terminated
  (thread
   (lambda ()
     (define worker-threads '())
     (let start-blocks ([blocks blocks]
                        [globals globals])
       (unless (empty? blocks)
         (define-values {runnable-blocks non-runnable-blocks} (partition (block-runnable/with? globals)
                                                                         blocks))
         ; - if runnable-block is empty that's ok; you might just be waiting for some
         ;   threads to complete.
         ;  TODO explicitly track the threads to avoid hanging here?

         ; start the runnable blocks
         (for ([block runnable-blocks])
           ; each runnable block enqueues its results when it finishes.
           (define t (thread
                      (lambda ()
                        (define results (run-block/args block globals))
                        ; 1. send results to the main background thread so it can
                        ;    start other blocks
                        (async-channel-put result-batches-done results)
                        ; 2. send results to the final consumer
                        (for ([r results])
                          (async-channel-put results-out r)))))
           (set! worker-threads (cons t worker-threads)))
         ; whenever a block finishes,
         ; update the globals and continue starting runnable blocks.
         (define some-results (async-channel-get result-batches-done))
         (define new-globals (for/fold ([globals globals]) ([r some-results])
                               (match r
                                 [(ResultValue _ name val) (if name
                                                               (hash-set globals name val)
                                                               globals)]
                                 ; TODO what happens to things that depended on this name?
                                 [(ResultError _ name msg) globals])))
         (start-blocks non-runnable-blocks
                       new-globals)))
     ; once all blocks have been started,
     ; wait for all worker threads to terminate.
     (for ([t worker-threads])
       (thread-wait t))
     ; finally, send a special value to the output channel to indicate we're done.
     (async-channel-put results-out 'done)))

  ; wrap the output channel in a sequence
  (in-producer async-channel-get 'done results-out))

(define ((block-runnable/with? globals) block)
  ; is everything the block depends on covered by the available globals?
  (subset? (block-deps block) (hash-keys globals)))

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body ...
      (loop))))

(module+ main
  (require "surface-syntax.rkt")
  (displayln "hello")

  (define (slowly seconds value)
    (sleep seconds)
    value)
  (define prims (list
                 +
                 -
                 <
                 =
                 slowly
                 ;;
                 ))
  (define globals (for/hash ([prim prims])
                    (values (object-name prim) prim)))

  (displayln "compiling...")
  (define program-sexprs (sequence->list (in-producer read eof-object?)))
  (define program-stmts (parse-program program-sexprs))
  (define program-blocks (eval-program program-stmts))
  (displayln "running...")
  (for ([result (run-program/concurrent program-blocks globals)])
    (displayln result))


  ;;
  )
