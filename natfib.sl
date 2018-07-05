struct S 1
struct Z 0


one = S Z
two = S one
three = S two
four = S three
five = S four


plus x y = match x {
  Z -> y
  S n -> S (plus n y)
}

plus two three

fib n = match n {
  S (S n) -> plus (fib n) (fib (S n))
  n -> n
}


fib (S five)