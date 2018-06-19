struct Cons 2
struct Empty 0

map f xs = match xs {
    Empty -> Empty
    Cons x xs -> Cons (f x) (map f xs)
}

(Cons 2 (Cons 14 Empty))
map (add 1) (Cons 2 (Cons 14 Empty))
