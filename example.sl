
struct Z
struct S n

add x y =
    match x {
        Z -> y
        (S n) -> S (add n y)
    }


one = S Z
two = S one
three = S two
four = S three

check (add two two) = four


struct Empty
struct Cons head tail

struct True
struct False

map f lst =
    match lst {
        Empty -> Empty
        Cons h t -> Cons (f h) (map f t)
    }

filter f lst =
    match lst {
        Empty -> Empty
        Cons h t ->
            match f h {
                True -> Cons h (filter f t)
                False -> filter f t
            }
    }

check map S (Cons one (Cons three Empty)) = Cons two (Cons four Empty)
