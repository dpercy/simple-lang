(def + primitives.+)
(def * primitives.*)
(def / primitives./)
(def - primitives.-)
(def < primitives.<)
(def = primitives.=)

123
4294967296 ; 2^32
(+ 1 4294967296)
18446744073709551616 ; 2^64
(+ 1 18446744073709551616)
(def (fact n)
  (match (< n 2)
    [#true n]
    [#false (* n (fact (- n 1)))]))
(fact 50)
(/ 3 0)
(- 23 100)
-5
-0
(- 1 1)
(- 0 0)
(= 0 -0)
(/ 3 -0)
