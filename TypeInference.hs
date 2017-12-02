{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeInference (
  checkProgram,
  testTypeInference,
) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Void (absurd)

import Test.Hspec

import Model


testTypeInference :: Spec
testTypeInference = error "TODO"


natDef = DefData "Nat" [Variant "Z" [], Variant "S" [T "Nat"]]
twoDef = DefVal "two" (Just (T "Nat")) [
  Case [] (App (Cst "S")  (App (Cst "S")  (App (Cst "S") (Cst "Z"))))]
plusDef = DefVal "plus" (Just (F (T "Nat") (F (T "Nat") (T "Nat")))) [
  Case [Constructor "Z" [], Hole "y"] (Var "y"),
  Case [Constructor "S" [Hole "x"], Hole "y"] (App (Cst "S")
                                               (App (App (Var "plus")
                                                     (Var "x"))
                                                (Var "y")))
  ]


{-

Type inference has two phases:
1. generate constraints
2. solve constraints

So what are constraints?

There are two kinds of constraints:
-  t1 :===: t2  means t1 and t2 must be equal
-  ty :=<=: ts  means ty must be an instance of ts

TODO read this http://community.haskell.org/~wren/unification-fd/test/tutorial/tutorial2.html

-}
data Constraint = Type :===: Type
                | Type :=<=: TypeSchema
                deriving (Show, Eq)
{-

A constraint asserts that two partially-known types must be equal.
What's a partially-known type?

A partially-known type can contain:
- TyVar, a unique identifier that stands for an unknown type.
  These are generated with a counter in phase 1 and solved
  in phase 2.
- TyRef, a reference to the type of some global or local.

-}
type Type = TypeOf TypeHole
data TypeHole = TyVar Int
              | TyRef NamePrefix String
              deriving (Show, Eq)




-- Rewrites a Program:
--   - adds type annotations to defs that lack them
--   - replaces type-erroring defs with Error statements
checkProgram :: Program -> Program
checkProgram stmts = error "TODO"


-- Monad for generating constraints.
newtype GC v = GC ( (StateT Counter
                     (ReaderT NamePrefix
                      (Writer [Constraint])))
                    v )
             deriving ( Applicative
                      , Functor
                      , Monad
                      , MonadState Counter
                      , MonadReader NamePrefix
                      , MonadWriter [Constraint]
                      )

runGC :: GC v -> (v, [Constraint])
runGC (GC comp) = runWriter $
                  (flip runReaderT (NamePrefix [])) $
                  (flip evalStateT (Counter 0)) $
                  comp



newtype Counter = Counter Int
newTyVar :: GC Type
newTyVar = do
  Counter i <- get
  put (Counter (i + 1))
  return (H (TyVar i))

newtype NamePrefix = NamePrefix [String] deriving (Show, Eq)
withPrefix :: String -> GC a -> GC a
withPrefix pathComponent = local $ \(NamePrefix p) ->
  NamePrefix (p ++ [pathComponent])



-- Generate constraints on a Program.
gcProgram :: Program -> GC ()
gcProgram = mapM_ gcStmt

gcStmt :: Stmt -> GC ()
gcStmt (Error _) = return ()
gcStmt (Expr e) = void (gcExpr e)
gcStmt (DefVal name (Just ty) cases) = do
  name' <- lookupName name
  let ty' = H (InstanceOf ty)
  tell [name' :===: ty']
  gcStmt (DefVal name Nothing cases)
gcStmt (DefVal name Nothing cases) = do
  mapM_ (gcCase name) cases

-- Generate constraints for the expression,
-- and return the type of the expression.
gcExpr :: Expr -> GC Type
gcExpr (Var name) = lookupName name
gcExpr (Cst cname) = lookupName cname
gcExpr (App f a) = do
  tyFunc <- gcExpr f
  tyArg <- gcExpr a
  tyResult <- newTyVar
  tell [tyFunc :===: (F tyArg tyResult)]
  return tyResult

-- lookupName takes the name of a variable or a constructor,
-- and returns its type.
lookupName :: String -> GC Type
lookupName name = do
  prefix <- ask
  return (H (TyRef prefix name))

-- Generates constraints for a single equation of a DefVal.
gcCase :: String -> Case -> GC ()
gcCase name (Case [] expr) = do
  tyName <- lookupName name
  tyExpr <- gcExpr expr
  tell [tyName :===: tyExpr]
gcCase name (Case (pat0:pats) expr) =
  error "TODO patterns"
