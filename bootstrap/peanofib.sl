; racket test-boot.rkt < peanofib.sl > pf.mjs && node --experimental-modules pf.mjs

(struct (Zero))
(struct (Succ n))

(def (add x y)
  (match x
    [(Zero) y]
    [(Succ x) (Succ (add x y))]))

(def (fib n)
  (match n
    [(Zero) (Zero)]
    [(Succ (Zero)) (Succ (Zero))]
    [(Succ (Succ n)) (add (fib n) (fib (Succ n)))]))

(fib (Zero))
(fib (Succ (Zero)))
(fib (Succ (Succ (Zero))))
(fib (Succ (Succ (Succ (Zero)))))
(fib (Succ (Succ (Succ (Succ (Zero))))))
(fib (Succ (Succ (Succ (Succ (Succ (Zero)))))))
(fib (Succ (Succ (Succ (Succ (Succ (Succ (Zero))))))))
"yay"
