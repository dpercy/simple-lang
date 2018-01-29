
; defval with an error doesn't break other definitions
123
(def x (error "oops"))
456

; but it does necessarily break things that relied on that value
(+ x 1)
