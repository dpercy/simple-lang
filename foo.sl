

data Nat = Zero | Succ Nat
data NatList = Nil | Cons Nat NatList

length :: NatList -> Nat
length Nil = Zero
length (Cons x xs) = Succ (length xs)


one :: Nat
one = Succ Zero

two :: Nat
two = Succ one

three :: Nat
three = Succ two

four :: Nat
four = Succ three


length Nil
length (Cons one Nil)
length (Cons one (Cons two Nil))
length (Cons one (Cons two (Cons three Nil)))
length (Cons one (Cons two (Cons three (Cons four Nil))))
