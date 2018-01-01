#lang racket

(provide find-sccs)
(module+ test (require rackunit))

(module stack racket
  #|

  Implements a stack with constant-time membership checking.

  |#
  (provide Stack?
           list->stack
           stack->list
           push
           pop
           peek
           stack-contains?)

  (struct Stack (as-list as-set) #:transparent)
  (define (push elem stack)
    (match stack
      [(Stack l s) (if (set-member? s elem)
                       (error 'push "stack ~v already contains ~v"
                              stack
                              elem)
                       (Stack (cons elem l)
                              (set-add s elem)))]))
  (define (pop stack)
    (match stack
      [(Stack '() _)  (error 'pop "stack is empty")]
      [(Stack (cons x xs) s)
       (Stack xs (set-remove s x))]))
  (define (peek stack)
    (match stack
      [(Stack '() _)  (error 'peek "stack is empty")]
      [(Stack (cons x _) _) x]))

  (define (stack->list stack) (Stack-as-list stack))
  (define (list->stack lst)
    (foldr push (Stack '() (set)) lst))

  (define (stack-contains? stack elem)
    (match stack
      [(Stack _ s) (set-member? s elem)])))
(require 'stack)
(module+ test
  (check-equal? (stack->list (list->stack '(a b c d e)))
                '(a b c d e))
  (check-true (stack-contains? (list->stack '(a b c d e)) 'a))
  (check-equal? (stack->list (pop (list->stack '(a b c d e))))
                '(b c d e))
  (check-equal? (pop (list->stack '(a b c d e)))
                (list->stack '(b c d e))))

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body ...
      (loop))))


#|

Finds strongly-connected components of a graph.

Nodes are symbols,
and edges are a hash table that maps each node to its directed neighbors (its "successors").

The output is a list of SCCs, where each SCC is a list of nodes (symbols).

|#
(define/contract (find-sccs successors) (-> (hash/c symbol? (listof symbol?)) (listof (listof symbol?)))
  #|

  This is Tarjan's SCC algorithm.

  Think about the DFS tree of a graph.
  Every edge in the original graph is either:
  - a tree edge (it's part of the DFS tree)
  - a back edge (points from a node to one of its ancestors in the tree)
  - a forward edge (points from a node to one of its descendants in the tree)
  - a cross edge (points from one tree branch to another)

  You can define a "low value" for each node.
  I think all of these definitions end up being equivalent:
  low(v) =
  - the smallest index of v's SCC
  - the smallest index of all nodes in all cycles containing v
  - the smallest index of all tree-ancestors of v that are reachable from v
  - the smallest index of
  .    all tree-ancestors of v that have a back-edge from v's subtree
  - the smallest of
  .   - index(v)
  .   - min(low(w) for w in descendants(v))
  .   - min(index(w) for (_, w) in back_edges_from(v))

  Back edges are the starting points:
  SCCs are made of cycles, and a cycle requires at least one back edge.

  ---

  This algorithm maintains a stack, but unlike normal DFS,
  it doesn't just contain the tree-ancestors of the current node.
  It contains the current node's ancestors, plus any node
  with a path to some ancestor.





  |#
  (define nodes (hash-keys successors))

  (define index (make-hash))
  (define low (make-hash))
  (define stack (list->stack '()))
  (define result '())
  (define counter 0)

  (define (visit node)
    ; set index
    (hash-set! index node counter)
    (set! counter (+ 1 counter))
    (set! stack (push node stack))
    ; init low
    (hash-set! low node (hash-ref index node))

    (for ([nn (hash-ref successors node)])
      (cond
        [(not (hash-has-key? index nn))
         ; successor nn not yet visited,
         ; so this is a tree edge.
         (visit nn)
         (hash-set! low node (min (hash-ref low node)
                                  (hash-ref low nn)))]
        [(stack-contains? stack nn)
         ; successor nn is still on the stack,
         ; which means there is a path from nn back to node.
         ; That means node and nn are in the same SCC.
         ; nn's low value may not be known yet (because
         ; we may still be in the process of visting nn).

         ; This means the "low" value we compute here is NOT
         ; "the lowest ancestor index reachable from this node".
         ; It's an approximation so you don't chase your tail
         ; computing the min of a cycle.
         ; But what IS true is that "low == index" iff
         ; you are the min index of your SCC.
         ; (Wikipedia glosses over this.)
         ; It's really the min of "lowpt" and "lowvine"...
         (hash-set! low node (min (hash-ref low node)
                                  (hash-ref index nn)))]
        [else
         ; Otherwise, nn is already visited,
         ; and this edge does not contribute a cycle.
         (void)]))
    ; after visiting the successors of a node,
    ; we check if the node is the head of a SCC,
    ; and if so pop the SCC off the stack.
    (when (= (hash-ref index node)
             (hash-ref low node))
      (define-values {scc new-stack} (pop-through node stack))
      (set! stack new-stack)
      ; add the scc to the result
      (set! result (cons scc result))))

  (for ([node nodes])
    (when (not (hash-has-key? index node))
      (visit node)))
  result)

(module+ test

  (check-match (remove '(x) (find-sccs (hash 'a '()
                                             'f '(g)
                                             'g '(f a)
                                             'x '())))
               (list (list-no-order 'f 'g)
                     (list 'a)))

  (check-match (find-sccs (hash 'a '()
                                'b '()
                                'c '()
                                'z '(a b c)))
               ; 'z comes first because it preceeds-in-the-graph each other node.
               (cons '(z) (list-no-order '(a) '(b) '(c)))))


(define (pop-through node stack) ; -> {listof node, stack}
  (if (equal? node (peek stack))
      (values (list (peek stack)) (pop stack))
      (let ()
        (define first (peek stack))
        (define-values {rest stack*} (pop-through node (pop stack)))
        (values (cons first rest) stack*))))
(module+ test
  (check-equal? (call-with-values (lambda ()
                                    (pop-through 'c (list->stack '(a b c d e f))))
                                  list)
                (list '(a b c)
                      (list->stack '(d e f)))))
