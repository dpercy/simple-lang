
map f xs = match xs {
    Empty -> Empty
    Cons x xs -> Cons (f x) (map f xs)
}

map (add 1) (Cons 2 (Cons 14 Empty))
