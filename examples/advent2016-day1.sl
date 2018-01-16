
(def input "L1, L5, R1, R3, L4, L5, R5, R1, L2, L2, L3, R4, L2, R3, R1, L2, R5, R3, L4, R4, L3, R3, R3, L2, R1, L3, R2, L1, R4, L2, R4, L4, R5, L3, R1, R1, L1, L3, L2, R1, R3, R2, L1, R4, L4, R2, L189, L4, R5, R3, L1, R47, R4, R1, R3, L3, L3, L2, R70, L1, R4, R185, R5, L4, L5, R4, L1, L4, R5, L3, R2, R3, L5, L3, R5, L1, R5, L4, R1, R2, L2, L5, L2, R4, L3, R5, R1, L5, L4, L3, R4, L3, L4, L1, L5, L5, R5, L5, L2, L1, L2, L4, L1, L2, R3, R1, R1, L2, L5, R2, L3, L5, L4, L2, L1, L2, R3, L1, L4, R3, R3, L2, R5, L1, L3, L3, L3, L5, R5, R1, R2, L3, L2, R4, R1, R1, R3, R4, R3, L3, R3, L5, R2, L2, R4, R5, L4, L3, L1, L5, L1, R1, R2, L1, R3, R4, R5, R2, R3, L2, L1, L5")

(struct Posn 2)

(def (add p1 p2)
  (match p1 [(Posn x1 y1)
  (match p2 [(Posn x2 y2)
    (Posn (+ x1 x2) (+ y1 y2))])]))
(def (scale p a)
  (match p
    [(Posn x y) (Posn (* a x) (* a y))]))
(scale (Posn 3 4) 2)

(add (Posn 1 2) (Posn 3 4))

(def (turn-left heading)
  (match heading
    ["N" "W"]
    ["W" "S"]
    ["S" "E"]
    ["E" "N"]))
(def (turn-right heading)
  (turn-left (turn-left (turn-left heading))))
(turn-right "N")

(struct State 2) ; posn heading

(def initial-state (State (Posn 0 0) "N"))

(struct Inst 2) ; angle magnitude

(def tokens (string-split input ", "))


; for now we need to declare structs for lists
(struct cons 2)
(struct empty 0)
(cons 1 (empty))

(def (first c) (match c [(cons x xs) x]))
(def (rest c)  (match c [(cons x xs) xs]))


(def (map f lst)
  (match lst
    [(empty)  (empty)]
    [(cons x xs) (cons (f x) (map f xs))]))

(def (p s)
  (Inst (substring s 0 1)
        (string->number (substring s 1))))
        
(def instructions (map p tokens))

(def (step state inst)
  (match inst
    [(Inst angle mag) (forward mag (turn angle state))]))
(def (turn angle state)
  (match state
    [(State posn heading)
     (match angle
        ["L" (State posn (turn-left heading))]
        ["R" (State posn (turn-right heading))])]))
(def (forward mag state)
  (match state
    [(State posn heading) (State (add posn (scale (unit heading) mag)) heading)]))

(def (unit heading)
  (match heading
    ["N" (Posn 0 -1)]
    ["S" (Posn 0 +1)]
    ["W" (Posn -1 0)]
    ["E" (Posn +1 0)]))
    
(turn "L" initial-state)
initial-state

(step initial-state (first instructions))
ef
(match initial-state
  [(State posn heading) heading])
  
(def (trace-all instrs state)
  (match instrs
    [(empty) (cons state (empty))]
    [(cons i rest) (cons state (trace-all rest (step state i)))]))
    
(def (last lst)
  (match lst
    [(cons x (empty)) x]
    [(cons x xs) (last xs)]))

(def final-state (last (trace-all instructions initial-state)))

(def (abs x)
  (match (< x 0)
    [#true (- x)]
    [#false x]))
(abs -7)
(abs 0)
(abs 3)
 
 
(def (measure state)
  (match state
    [(State (Posn x y) h)
     (+ (abs x) (abs y))]))
(measure final-state)