{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Termination (
  checkProgram,
  testTermination,
  ) where

import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Vector as Vector

import Data.Maybe

import Model

import Test.Hspec


--import Test.Hspec


{-

Based on "foetus - Termination Checker for Simple Functional Programs"
by Andreas Abel
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.44.3494&rep=rep1&type=pdf

-}


-- One value can be Less-than, Equal-to (or maybe less than), or Unknown with another.
data Rel = Lt | Eq | Uk deriving (Show, Eq, Ord)

instance Ord (Matrix Rel) where
  compare m1 m2 = compare (Matrix.toList m1) (Matrix.toList m2)
  


-- (*) Composing two relations:
--   if   x < y and y <= z then x < z, etc.
-- Combines two relations in serial.
-- Cases with the least information come first; Uk dominates.
composeRel :: Rel -> Rel -> Rel
composeRel Uk _ = Uk
composeRel _ Uk = Uk
composeRel Eq yz = yz
composeRel xy Eq = xy
composeRel Lt Lt = Lt


-- (+) Join two relations in parallel:
--    if x < y and x <= y then x < y
-- Combines parallel information.
-- Cases with the most information come first; Lt dominates.
parjoin :: Rel -> Rel -> Rel
parjoin Lt _ = Lt
parjoin _ Lt = Lt
parjoin Eq _ = Eq
parjoin _ Eq = Eq
parjoin Uk Uk = Uk


instance Num Rel where
  (*) = composeRel
  (+) = parjoin
  fromInteger 0 = Uk -- additive identity, dominates multiplication
  fromInteger 1 = Eq -- multiplicative identity
  fromInteger _ = undefined
  -- everything else is undefined
  abs = undefined
  signum = undefined
  negate = undefined

{-

Now you can use Matrix Rel to represent a particular function call.
If f x y calls g z, then you can make this matrix:

          f params
     g      x  y
  args  z [ ?, ? ]

You can also form a call graph, where nodes are function definitions
and edges are function calls. Decorate each edge with one of these
arg-dep matrices.

Paths through the graph correspond to matrix products.
"If g calls h with matrix B and f calls g with matrix A, then f
 indirectly calls h with matrix BA."


TODO confirm this:
ALL SELF-PATHS must agree on a lexical order.
- a self-path has a square matrix
- the empty self-path is the identity matrix: Eq diagonal in a Uk matrix.
- you can extract the diagonal of a self-path to see how each argument changes!

- special case: If there is only 1 nonempty self path, and some argument shrinks,
                then the function terminates.
- specialish case: If all nonempty self paths have the same argument shrinking, it terminates.
- specialish case: If there is an argument such that all self paths either shrink or preserve it,
                   the function terminates.
- less special: If there is a "key" of arguments (x, y), and every self path either
                  1. shrinks x
                  2. preserves x and shrinks y
                then the function terminates!

NOTE you can't just parjoin the paths together!!
Some functions might *have* a shrinking call,
but won't necessarily use it enough to terminate!
  f x = if ___ then f (x-1) else f (x + 999)

So can you just "compose" the things together?
- if some recursive call is all "?", then you've lost all information
- if some recursive call is all "=", then it doesn't all or lose info
- Another viewpoint: all traces must be built out of this "basis" of self-paths.
   - So if you smash the basis together and see that some argument shrinks,
     the function terminates!
- Are we throwing info away by ignoring the two triangles (by looking only at diagonals)?


interleave [] ys = ys
interleave (x:xs) ys = x:(interleave ys xs)

interleave has 1 nonempty self-path:

     x:xs   ys
ys [   ?    =  ]
xs [   <    ?  ]

what happens when we take the product closure?
I *think* it's

[  <  ?  ]
[  ?  <  ]

actually wait, it doesn't converge!
It alternates, because that flipped-diagonal keep permuting it.


OK!
"Completion of a call graph" means "turn all paths to edges".
I think this means you can keep stepping the graph: using edges to create extended paths.
Since each "path" actually only stores the call matrix (not the actual path of nodes taken),
this should converge.
So you're kind of doing a giant fixed point on this graph of matrices.


So in the interleave example above, the completion would be this set of calls:
(all of them are between interleave and interleave):

[ ? = ]
[ < ? ]

[ < ? ]
[ ? < ]

[ ? < ]
[ < ? ]

At this point, doing another step doesn't grow the graph at all, so you're done.


-}


converge :: Eq a => (a -> a) -> a -> a
converge step start = if next == start then start else converge step next
  where next = step start


newtype Multigraph v e = Multigraph (Set (v, v, e))
                       deriving (Show, Eq)

merge :: (Ord v, Ord e) => Multigraph v e -> Multigraph v e -> Multigraph v e
merge (Multigraph x) (Multigraph y) = Multigraph (Set.union x y)

merges :: (Ord v, Ord e) => [Multigraph v e] -> Multigraph v e
merges = foldr merge (Multigraph Set.empty)

emptyGraph :: Multigraph v e
emptyGraph = Multigraph Set.empty

singletonGraph :: (v, v, e) -> Multigraph v e
singletonGraph = Multigraph . Set.singleton


extendPaths :: Multigraph Uppercase (Matrix Rel) -> Multigraph Uppercase (Matrix Rel) -> Multigraph Uppercase (Matrix Rel)
extendPaths (Multigraph orig) (Multigraph g) = Multigraph . Set.fromList $ do
  edge <- Set.toList orig
  path <- Set.toList g
  path' <- maybeToList (composeCall edge path)
  return path'

-- (f, g, C)  means  f calls g
-- C has 1 row per arg of g
-- C has 1 col per param of f
type Call = (Uppercase, Uppercase, Matrix Rel)

composeCall :: Call -> Call -> Maybe Call
composeCall (g, h, b) (f, g', a) =
  if g == g'
  then Just (f, h, b * a)
  else Nothing


-- "completing" the call graph turns every path into an explicit edge.
complete :: Multigraph Uppercase (Matrix Rel) -> Multigraph Uppercase (Matrix Rel)
complete g = converge (completeIter g) g

completeIter :: Multigraph Uppercase (Matrix Rel) -> Multigraph Uppercase (Matrix Rel) -> Multigraph Uppercase (Matrix Rel)
completeIter orig g = (extendPaths orig g) `merge` g


recursionBehavior :: Multigraph Uppercase (Matrix Rel) -> Map Uppercase (Set [Rel])
recursionBehavior (Multigraph graph) =
  let calls :: [Call]
      calls = Set.toList graph
  in Map.fromListWith Set.union $ do
    (f, g, c) <- calls
    if f == g
      then return (f, Set.singleton (Vector.toList (Matrix.getDiag c)))
      else []


-- at the end you check each function, and if you can find a single argument that decreases in *all* self loops, you've proven it.
data NonterminationError = NonterminationError { name :: Uppercase
                                               , selfCalls :: Set [Rel]
                                               }
                         deriving (Show, Eq)

checkDef :: Map Uppercase (Set [Rel]) -> Def -> Either NonterminationError ()
checkDef _ (DefData{}) = return ()
checkDef env (DefVal name _ _) =
  case Map.lookup name env of
   Nothing -> return () -- no recursive calls (direct or indirect)
   Just selfCalls -> if hasDecreasingArg selfCalls
                     then return ()
                     else Left (NonterminationError { name, selfCalls })

hasDecreasingArg :: Set [Rel] -> Bool
hasDecreasingArg calls =
  if Set.null calls || any null calls
  then False
  else if all ((== Lt) . head) calls
       then True
       else hasDecreasingArg (Set.map tail calls)

checkProgram :: Program -> Either NonterminationError ()
checkProgram defs = do
  let graph = generateGraph defs
  let env = recursionBehavior (complete graph)
  mapM_ (checkDef env) defs


-- local env maps free a variable to a row:
-- the row describes how the variable relates to each (positional) parameter.
data LocalEnv = LocalEnv [Rel] (Map Lowercase [Rel])
              deriving (Show, Eq)

-- global env maps global variables to arities.
-- Note arity is defined as the number of patterns on the left-hand side:
-- not to be confused with the number of arrows in the type.
type GlobalEnv = Map Lowercase Int

liveVariables :: Pattern -> Map Lowercase Rel
liveVariables (Hole name) = Map.fromList[(name, Eq)]
liveVariables (Constructor _ pats) = Map.unions (map (Map.map (const Lt) . liveVariables) pats)


makeLocalEnv :: [Pattern] -> LocalEnv
makeLocalEnv pats = LocalEnv def m
  where def = map (const Uk) pats
        m = makeLocalEnvMap pats

makeLocalEnvMap :: [Pattern] -> (Map Lowercase [Rel])
makeLocalEnvMap pats =
  let liveSets :: [Map Lowercase Rel]
      liveSets = map liveVariables pats
      vars :: [Lowercase]
      vars = do { live <- liveSets ; Map.keys live }
  in Map.fromList $ do
    var <- vars
    let rels = flip map liveSets $ \live ->
          case Map.lookup var live of
           Nothing -> Uk
           Just rel -> rel
    return (var, rels)

makeGlobalEnv :: Program -> GlobalEnv
makeGlobalEnv defs = Map.fromList [ (name, getOnly (unique (map caseArity cases)))
                                  | (DefVal name _ cases) <- defs
                                  ]

caseArity :: Case -> Int
caseArity (Case pats _) = length pats

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

getOnly :: Show a => [a] -> a
getOnly [] = error "no values"
getOnly [x] = x
getOnly xs = error ("several values: " ++ show xs)
    


-- generate the call graph: traverse the program and at every call site
-- emit an edge that relates the arguments to the parameters.
generateGraph :: Program -> Multigraph Uppercase (Matrix Rel)
generateGraph defs = merges (map (genDef genv) defs)
  where genv = makeGlobalEnv defs

genDef :: GlobalEnv -> Def -> Multigraph Uppercase (Matrix Rel)
genDef _ (DefData{}) = emptyGraph
genDef genv (DefVal name _ cases) = merges (map (genCase genv name) cases)

genCase :: GlobalEnv -> Lowercase -> Case -> Multigraph Uppercase (Matrix Rel)
genCase genv name (Case pats expr) = merges (map (genCall name lenv) calls)
  where lenv = makeLocalEnv pats
        calls = findCalls genv expr -- TODO handle shadowing

-- find all calls in an expression.
-- you need to know the arity of globals to know how many args are in a call.
findCalls :: GlobalEnv -> Expr -> [(Lowercase, [Expr])]
findCalls genv e = findCallsWithArgs genv e []

findCallsWithArgs :: GlobalEnv -> Expr -> [Expr] -> [(Lowercase, [Expr])]
findCallsWithArgs _ (Cst _) _ = []
findCallsWithArgs genv (App f x) args = findCallsWithArgs genv f (x:args) ++ findCalls genv x
findCallsWithArgs genv (Var name) args =
  case Map.lookup name genv of
   Nothing -> []
   Just arity -> if arity > length args
                 then [] -- TODO actually this IS a call, it just has Uk for many params.
                 else [(name, args)]

-- given a call site, emit a 1-edge call graph.
genCall :: Lowercase -> LocalEnv -> (Lowercase, [Expr]) -> Multigraph Uppercase (Matrix Rel)
genCall defname lenv (callee, args) = singletonGraph (defname, callee, mat)
  where mat = if null args
              then Matrix.matrix 0 0 undefined
              else vcats (map (genArg lenv) args)

vcats :: [Matrix a] -> Matrix a
vcats = foldr1 (Matrix.<->)

row :: [a] -> Matrix a
row = Matrix.rowVector . Vector.fromList

-- row: how does this arg relate to each param?
genArg :: LocalEnv -> Expr -> Matrix Rel
genArg (LocalEnv def _) (App _ _) = row def
genArg (LocalEnv def _) (Cst _) = row def
genArg (LocalEnv def m) (Var name) = row (Map.findWithDefault def name m)
  

testTermination :: Spec
testTermination = do
  it "loop fails" $ do
    checkProgram [
      DefVal "loop" Nothing [
         Case [] (Var "loop")
         ]
      ] `shouldBe` Left (NonterminationError { name = "loop"
                                             , selfCalls = Set.fromList [
                                               -- just one call, no args.
                                               []
                                               ]})
  it "loop0 loop1 fails" $ do
    checkProgram [
      DefVal "loop0" Nothing [ Case [] (Var "loop1") ],
      DefVal "loop1" Nothing [ Case [] (Var "loop0") ]
      ] `shouldBe` Left (NonterminationError { name = "loop0"
                                             , selfCalls = Set.fromList [ [] ]})
  it "length terminates" $ do
    checkProgram [
      DefVal "length" Nothing [
         Case [Constructor "Nil" []] (Cst "Zero"),
         Case [Constructor "Cons" [Hole "hd", Hole "tl"]] (App (Cst "Succ")
                                                           (App (Var "length")
                                                            (Var "tl")))
         ]
      ] `shouldBe` Right ()
  it "length without base case... is actually OK! (handled by type/case checker)" $ do
    checkProgram [
      DefVal "length" Nothing [
         -- Case [Constructor "Nil" []] (Cst "Zero"),
         Case [Constructor "Cons" [Hole "hd", Hole "tl"]] (App (Cst "Succ")
                                                           (App (Var "length") (Var "tl")))
         ]
      ] `shouldBe` Right ()
  it "map terminates" $ do
    checkProgram [
      DefVal "map" Nothing [
         Case [Hole "f", Constructor "Nil" []] (Cst "Nil"),
         Case [Hole "f", Constructor "Cons" [Hole "hd", Hole "tl"]]
         (App (App (Cst "Cons") (App (Var "f") (Var "hd")))
          (App (App (Var "map") (Var "f")) (Var "tl")))
         ]
      ] `shouldBe` Right ()
