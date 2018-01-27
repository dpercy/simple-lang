(struct (Derp))
(Derp)
Derp
(def d Derp)
(d)
d
(match (Derp)
  [(d) "renamed constructors are patterns too"]
  [otherwise "uh oh"])
(def (f)
  derp)
(def p (f))
(match (Derp)
  [(p) "computed constructors are patterns too"]
  [otherwise "hm"])
(Derp 999)
(struct (Triple x y z))
Triple
(Triple)
(Triple "a" 3 (Derp))
(Triple d f Triple)
(match (Triple 1 2 3)
  [(Triple) "bad"]
  [(Triple x y z) "good"])
