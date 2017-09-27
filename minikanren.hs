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

Every substitution also contains a "nextfree" variable name,
guaranteed not to collide with any existing bindings.

-}

newtype Goal term a = Goal { unGoal :: Subst term -> [(Subst term, a)] }

data Subst term = Subst { substNextfree :: Int
                        , substPairs :: [(Int, term)]
                        }
                deriving (Show)


-- TODO allow custom data structures instead
data Term = Var Int
          | Lit String
          | Cons Term Term
          deriving (Show, Eq)


runGoal :: Goal Term Term -> [Term]
runGoal (Goal g) = map (\(s, v) -> reify v s) $ g (Subst 0 [])

reify :: Term -> Subst Term -> Term
reify t s = case walk t s of
  Lit x -> Lit x
  Var x -> Var x -- because (walk t s) is not free in s
  Cons x y -> Cons (reify x s) (reify y s)

-- true always succeeds, binding no variables.
true :: Goal Term ()
true = Goal $ \s -> [(s, ())]

succeed :: a -> Goal term a
succeed v = Goal $ \s -> [(s, v)]

-- false always fails.
false :: Goal Term ()
false = Goal $ \_ -> []

infixr 2 |||
infixr 3 &&&
infix 4 ===
infixl 1 `bind`


-- "or" succeeds when either goal succeeds.
-- It's complete: neither stream of results is "starved",
-- because results are interleaved.
(|||) :: Goal term a -> Goal term a -> Goal term a
(|||) (Goal g) (Goal h) = Goal $ \s -> interleave (g s) (h s)

interleave :: [a] -> [a] -> [a]
interleave []     ys = ys
interleave (x:xs) ys = x:(interleave ys xs)

-- "and" succeeds when both arguments succeed.
-- If one argument succeeds multiple times, "and" succeeds multiple times.
-- If both succeed multiple times, "and" does the cross product.
-- If either fails, "and" fails.
-- The second goal is only run if the first goal succeeded,
-- and any variables bound by the first goal are in effect for the second goal.
(&&&) :: Goal term () -> Goal term () -> Goal term ()
(&&&) g h = g `bind` \() -> h

-- "bind" chains goals together.
-- It runs the first goal.
-- Then whenever that first goal succeeds, it runs the second goal on the result.
-- But it interleaves the results so that no subgoal is starved.
bind :: Goal term a -> (a -> Goal term b) -> Goal term b
bind g h = Goal $ \start ->
  -- gSuccesses is a possibly-infinite stream.
  let gSuccesses = unGoal g start in
  -- resultStreams is a stream-of-streams.
  -- Each element can be infinite (if h succeeds infinite times),
  -- plus the container can be infinite (if g succeeds infinite times).
  let resultStreams = flip map gSuccesses $ \(s, a) -> unGoal (h a) s
  in foldr interleave [] resultStreams

(===) :: Term -> Term -> Goal Term ()
(===) t u = Goal $ \subst ->
  case (walk t subst, walk u subst) of
   (Lit x, Lit y) -> if x == y then [(subst, ())] else []
   (Var x, u') -> [(extend x u' subst, ())]
   (t', Var y) -> [(extend y t' subst, ())]
   (Cons a b, Cons x y) -> unGoal  (a === x &&& b === y) subst
   (_, _) -> []

-- walk returns either a non-Var, or a var not bound in subst.
-- (The result can *contain* bound variables, but can't *be* one.)
walk :: Term -> Subst Term -> Term
walk (Var x) s = case lookup x (substPairs s) of
  Nothing -> Var x
  Just x' -> walk x' s
walk t _ = t

-- extend assumes the variable is free in the subst.
-- it adds a new subst entry.
-- TODO what if the subst describes infinite values?
extend :: Int -> term -> Subst term -> Subst term
extend x t s = s { substPairs = (x, t) : substPairs s }
   

fresh :: Goal Term Term
fresh = Goal $ \(Subst i s) -> [(Subst (i + 1) s, Var i)]


{-
Can it make sense for a "Goal" to return a value?
For example, freshVar could be a goal that always succeeds,
and returns a fresh variable (a term).

If a Goal returned a value, it would have to return a value each time it succeeds.
-}

instance Functor (Goal term) where
  fmap f gv = pure f <*> gv

instance Applicative (Goal term) where
  pure = succeed
  (<*>) gf gv = do
    f <- gf
    v <- gv
    return $ f v
  
instance Monad (Goal term) where
  (>>=) = bind
  return = succeed



{-

Now can this work with custom datatypes?
Maybe "term" is a second type parameter of Goal.
You also need a typeclass to define how to unify two terms.
Hopefully it's a convenient typeclass to write instances for.


What properties do terms need?
- can represent variables
- check if two nodes have same constructor
- get children
^-- 2 and 3 sound like Traversable or Repr or something?


-}

-- appendo x y xy  iff  x ++ y == xy
appendo :: Term -> Term -> Term -> Goal Term ()
appendo x y out = 
  (x === Lit "" &&& y === out)
  |||
  (do
      hd <- fresh
      tl <- fresh
      y' <- fresh
      Cons hd tl === x
      out === Cons hd y'
      -- Note we call appendo *last*.
      -- Somehow this prevents infinite recursion?
      appendo tl y y'
  )

example = runGoal $ do
  x <- fresh
  y <- fresh
  appendo x y (Cons (Lit "a") (Cons (Lit "b") (Cons (Lit "c") (Lit ""))))
  return x

example2 = runGoal $ do
  -- find all lists that contain "david"
  prefix <- fresh
  suffix <- fresh
  total <- fresh
  mid <- fresh
  midsuffix <- fresh
  
  mid === (Cons (Lit "david") (Lit ""))
  appendo mid suffix midsuffix
  appendo prefix midsuffix total

  return total