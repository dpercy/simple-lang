#lang reader "sl.rkt"


map f lst = match lst {
  Empty => Empty
  Cons x xs => Cons (f x) (map f xs)
}
check map (add 1) [1; 2; 3] = [2; 3; 4]

foldr1 f lst = match lst {
  Cons x Empty => x
  Cons x xs => f x (foldr1 f xs)
}
check foldr1 sub [8; 4; 2] = 6

foldr f init lst = match lst {
  Empty => init
  Cons x xs => f x (foldr f init xs)
}
check foldr Cons [3] [1; 2] = [1; 2; 3]
