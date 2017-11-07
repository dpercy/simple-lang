{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CaseCoverage (
  checkProgram,
  explain,
  testCaseCoverage,
  ) where


import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative
import Test.Hspec

import Model


{-

Use patterns (or sets of patterns) to represent sets of values.

You can subtract one pattern from another,
and the resulting set may require more than one pattern to represent.

To do case coverage:
- start with a catchall representing all possible input values
- subtract the values handled by each case
- if there are unhandled cases at the end, that's bad
    - also, if some case doesn't subtract anything, that indicates the case is unreachable.


-}


exampleProg :: Program
exampleProg = [ DefData "Nat" [ Variant "Zero" []
                              , Variant "Succ" [ T "Nat" ]
                              ]
              , DefData "Bool" [ Variant "True" []
                               , Variant "False" []
                               ]
              ]

exampleEnv :: Env
exampleEnv = getTypeEnv exampleProg

c :: Uppercase -> [Pattern] -> Pattern
c = Constructor

h :: Pattern
h = Hole "_"

t :: Pattern
t = c "True" []

f :: Pattern
f = c "False" []

b :: Type
b = T "Bool"

n :: Type
n = T "Nat"

z :: Pattern
z = c "Zero" []

s :: Pattern -> Pattern
s x = c "Succ" [x]

testCaseCoverage :: Spec
testCaseCoverage = do
  let env = exampleEnv
  context "simple pat subtraction" $ do
    -- 9 cases total for 2 bool patterns: (hole | false | true)^2
    it "anything minus hole is nothing" $ do
      subtractPat env b (c "Hole" []) h `shouldBe` Fuzzy []
      subtractPat env b f h `shouldBe` Fuzzy []
      subtractPat env b t h `shouldBe` Fuzzy []
    it "any case minus itself is nothing" $ do
      subtractPat env b t t `shouldBe` Fuzzy []
      subtractPat env b f f `shouldBe` Fuzzy []
    it "true/false minus the opposite is itself" $ do
      subtractPat env b t f `shouldBe` Fuzzy [t]
      subtractPat env b f t `shouldBe` Fuzzy [f]
    it "hole minus true/false is the opposite" $ do
      subtractPat env b h f `shouldBe` Fuzzy [t]
      subtractPat env b h t `shouldBe` Fuzzy [f]
  context "nested pat subtraction" $ do
    it "any case minus itself is nothing" $ do
      subtractPat env n z z `shouldBe` Fuzzy []
      subtractPat env n (s h) (s h) `shouldBe` Fuzzy []
    it "zero/succ(_) minus the opposite is itself" $ do
      subtractPat env n z (s h) `shouldBe` Fuzzy [z]
      subtractPat env n (s h) z `shouldBe` Fuzzy [s h]
    it "hole minus zero/succ(_) is the opposite" $ do
      subtractPat env n h (s h) `shouldBe` Fuzzy [z]
      subtractPat env n h z `shouldBe` Fuzzy [s h]
    it "hole minus a nested case gives you a nested case" $ do
      -- naturals minus 2 gives 0, 1, 3-or-more.
      subtractPat env n h (s (s z)) `shouldBe` Fuzzy [
        z,
        s z,
        s (s (s h))
        ]
  context "rows of patterns" $ do
    it "all holes minues all holes" $ do
      subtractPats env [] [] [] `shouldBe` Fuzzy []
      subtractPats env [n] [h] [h] `shouldBe` Fuzzy []
      subtractPats env [n, n] [h, h] [h, h] `shouldBe` Fuzzy []
      subtractPats env [n, n, n] [h, h, h] [h, h, h] `shouldBe` Fuzzy []
    it "singleton" $ do
      subtractPats env [b] [t] [f] `shouldBe` Fuzzy [ [t] ]
      subtractPats env [b] [h] [f] `shouldBe` Fuzzy [ [t] ]
    it "cross product of two bools" $ do
      subtractPats env [b, b] [h, h] [t, t] `shouldBe` Fuzzy [ [f, h], [h, f] ]
      subtractPats env [b, b] [h, h] [f, f] `shouldBe` Fuzzy [ [t, h], [h, t] ]
      (do p <- subtractPats env [b, b] [h, h] [t, t]
          subtractPats env [b, b] p [f, f])
        `shouldBe` Fuzzy [ [f, t], [t, f] ]
    it "all holes minus one case" $ do
      subtractPats env [n, n, n] [h, h, h] [h, s z, h] `shouldBe` Fuzzy [
        [h, z, h],
        [h, s (s h), h]
        ]


type Env = Map Type [Variant]

-- Fuzzy a represents a set of a.
-- It's useful to avoid confusing a sequence/row/product of things with a set/sum of things.
newtype Fuzzy a = Fuzzy [a] deriving (Functor, Monad, Applicative, Alternative, Eq, Show)


lookupArgTypes :: Env -> Type -> Uppercase -> [Type]
lookupArgTypes env ty cst = case Map.lookup ty env of
  Nothing -> error "type not in env"
  Just variants -> head [ args
                        | Variant name args <- variants
                        , name == cst
                        ]

subtractPat :: Env -> Type -> Pattern -> Pattern -> Fuzzy Pattern
subtractPat _ _ _ (Hole _) = Fuzzy []
subtractPat env ty (Constructor head0 args0) (Constructor head1 args1) =
  if head0 /= head1
  then Fuzzy [ Constructor head0 args0 ]
  else let argTypes = lookupArgTypes env ty head0
       in Constructor head0 <$> subtractPats env argTypes args0 args1
subtractPat env ty (Hole _) pat1 = do
  pat0 <- splitPat env ty
  subtractPat env ty pat0 pat1


subtractPats :: Env -> [Type] -> [Pattern] -> [Pattern] -> Fuzzy [Pattern]
-- An empty row minus an empty row leaves no cases unhandled.
subtractPats _ [] [] [] = Fuzzy []
subtractPats env (ty:tys) (x:xs) (y:ys) = leftPunch <|> rightPunch
  where leftPunch = do
          left <- subtractPat env ty x y
          return (left:xs)
        rightPunch = do
          right <- subtractPats env tys xs ys
          return (x:right)
subtractPats _ _ _ _ = error "uneven rows in subtractPats"


subtractPats' :: Env -> [Type] -> Fuzzy [Pattern] -> [Pattern] -> Fuzzy [Pattern]
subtractPats' env ty fpats0 pats1 = do pats0 <- fpats0
                                       subtractPats env ty pats0 pats1


splitPat :: Env -> Type -> Fuzzy Pattern
splitPat env ty = case Map.lookup ty env of
  Nothing -> error "type not in env"
  Just variants -> Fuzzy [ Constructor name [ Hole "_" | _ <- args ]
                         | Variant name args <- variants
                         ]


getTypeEnv :: Program -> Env
getTypeEnv prog = Map.fromList $ do
  def <- prog
  case def of
   DefData tyName variants -> [ (T tyName, variants) ]
   DefVal{} -> []
   Expr _ -> []



data CaseCoverageError = CaseCoverageError { vname :: Lowercase
                                           , unhandledPatterns :: Fuzzy [Pattern]
                                           }
                       | MissingTypeAnnotation { vname :: Lowercase }
                       deriving (Show, Eq)

explain :: CaseCoverageError -> String
explain (CaseCoverageError { vname, unhandledPatterns = (Fuzzy unhandledPatterns) }) =
  "In the definition of " ++ vname ++ ", these cases aren't handled:"
  ++ unlines (map show unhandledPatterns)
explain (MissingTypeAnnotation vname) = "Can't do case coverage on value with no type annotation: " ++ vname

checkProgram :: Program -> Either CaseCoverageError ()
checkProgram prog = do
  let env = getTypeEnv prog
  mapM_ (checkStmt env) prog

checkStmt :: Env -> Stmt -> Either CaseCoverageError ()
checkStmt _ (Expr _) = return ()
checkStmt _ (DefVal _ _ [Case [] _]) = return () -- 1 case, 0 args covers all cases.
checkStmt _ (DefData{}) = return ()
checkStmt _ (DefVal vname Nothing _) = Left (MissingTypeAnnotation vname)
checkStmt env (DefVal vname (Just ty) cases) =
  let arity = length (casePatterns (head cases)) in
  let argTypes = take arity (typeArguments ty) in
  let initialInputs = Fuzzy [ take arity (repeat (Hole "_")) ] in
  let unhandledPatterns = foldl (subtractPats' env argTypes) initialInputs (map casePatterns cases) in
  case unhandledPatterns of
   Fuzzy [] -> return ()
   _ -> Left (CaseCoverageError { vname, unhandledPatterns })


casePatterns :: Case -> [Pattern]
casePatterns (Case pats _) = pats
