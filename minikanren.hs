{-# OPTIONS_GHC -Wall #-}

{-

A "Goal" is something you're trying to prove.

A goal might result in:
- false: the goal is a false statement. Like trying to prove (cons x y) = 7.
- true(subst): we found a substitution that makes the goal true. Like (cons x 2) = (cons 1 y)  =>  { x: 1, y: 2 }
- true(subst1), true(subst2), true(subst3), ...: we found *several* ways to make it true. Like (or (= x 1) (= x 2))  =>  { x: 1 }, { x: 2 }

So in general, a goal returns a list of substitutions.


What about input?
You can "run" a goal in different contexts.
For example, I might want to prove (cons x y) = z while knowing z = 7,
and later try to prove it while knowing z = (cons 1 2).
So a goal's result depends on its context.

We also need to thread a piece of state through the goal:
a goal has to allocate new logic variables.
So we can just use an Int, and feed that through the input and *each* resulting substitution.


-}

type Goal = (Subst, Int) -> [(Subst, Int)]

newtype Subst = Subst { unSubst :: [(Int, Term)] }
              deriving (Show)


-- TODO allow custom data structures instead
data Term = Var Int
          | Lit String
          | Cons Term Term
          deriving (Show, Eq)


runGoal :: Goal -> [Subst]
runGoal g = map fst (g startState)
  where startState = (Subst [], 1)

-- true always succeeds, binding no variables.
true :: Goal
true (s, i) = [(s, i)]

-- false always fails.
false :: Goal
false _ = []

infixr 2 |||
infixr 3 &&&
infix 4 ===

-- "or" succeeds when either goal succeeds.
-- It's complete: neither stream of results is "starved",
-- because results are interleaved.
(|||) :: Goal -> Goal -> Goal
(|||) g h start = interleave (g start) (h start)

interleave :: [a] -> [a] -> [a]
interleave []     ys = ys
interleave (x:xs) ys = x:(interleave ys xs)

-- "and" succeeds when both arguments succeed.
-- If one argument succeeds multiple times, "and" succeeds multiple times.
-- If both succeed multiple times, "and" does the cross product.
-- If either fails, "and" fails.
-- The second goal is only run if the first goal succeeded,
-- and any variables bound by the first goal are in effect for the second goal.
(&&&) :: Goal -> Goal -> Goal
(&&&) g h start =
  -- gSuccesses is a possibly-infinite stream.
  let gSuccesses = g start in
  -- resultStreams is a stream-of-streams.
  -- Each element can be infinite (if h succeeds infinite times),
  -- plus the container can be infinite (if g succeeds infinite times).
  let resultStreams = map h gSuccesses in
  foldr interleave [] resultStreams

(===) :: Term -> Term -> Goal
(===) t u (subst, i) =
  case (walk t subst, walk u subst) of
   (Lit x, Lit y) -> if x == y then [(subst, i)] else []
   (Var x, u') -> [(extend x u' subst, i)]
   (t', Var y) -> [(extend y t' subst, i)]
   (Cons a b, Cons x y) -> (a === x &&& b === y)(subst, i)
   (_, _) -> []

-- walk returns either a non-Var, or a var not bound in subst.
-- (The result can *contain* bound variables, but can't *be* one.)
walk :: Term -> Subst -> Term
walk (Var x) (Subst s) = case lookup x s of
  Nothing -> Var x
  Just x' -> walk x' (Subst s)
walk t _ = t

-- extend assumes the variable is free in the subst.
-- it adds a new subst entry.
-- TODO what if the subst describes infinite values?
extend :: Int -> Term -> Subst -> Subst
extend x t (Subst s) = Subst ((x, t) : s)
   

freshVar :: (Term -> Goal) -> Goal
freshVar cb = \(subst, i) -> (cb (Var i))(subst, i + 1)
