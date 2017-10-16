{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Termination (
  ) where

import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe

import Model


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

example :: Matrix Rel
example = Matrix.fromLists [
  [ Uk, Eq ],
  [ Lt, Uk ]
  ]


converge :: Eq a => (a -> a) -> a -> a
converge step start = if next == start then start else converge step next
  where next = step start


newtype Multigraph v e = Multigraph (Set (v, v, e))
                       deriving (Show, Eq)

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


complete :: Multigraph Uppercase (Matrix Rel) -> Multigraph Uppercase (Matrix Rel)
complete g = converge (extendPaths g) g


