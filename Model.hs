{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model where

import Data.Data (Data)
import Data.Void (Void)
import Test.Hspec


-- programs consist of data definitions, and value definitions
type Program = [Stmt]

data Stmt = DefData Uppercase [Variant]
            -- defval's type annotation is optional,
            -- but a simple-minded typechecker might reject you if you omit the type.
          | DefVal Lowercase (Maybe TypeSchema) [Case]
          | Expr Expr
            -- Error is not a "real" statement; it's the result of
            -- some statements failing, for example during typechecking.
          | Error String
          deriving (Show, Eq)



-- data definitions have a name and a set of variants.
-- each variant defines a single constructor: its name, and its argument types.
data Variant = Variant Uppercase [MonoType] -- note: no generic containers for now.
             deriving (Show, Eq)

-- a type is either:
--   - a named type (defined by a data definition)
--   - a function type (which contains an argument type and a result type)
data TypeOf h = T Uppercase
              | F (TypeOf h) (TypeOf h)
              | H h
              deriving (Show, Eq, Ord)

type MonoType = TypeOf Void

-- represents an implicit forall type like (forall x y. x -> (x -> y) -> y)
type TypeSchema = TypeOf Lowercase

--
data Case = Case [Pattern] Expr
          deriving (Show, Eq)

data Pattern = Hole Lowercase
             | Constructor Uppercase [Pattern]
             deriving (Show, Eq)

data Expr = Var Lowercase
          | Cst Uppercase
          | App Expr Expr
          deriving (Show, Eq, Data)

type Lowercase = String
type Uppercase = String


typeArguments :: TypeOf h -> [TypeOf h]
typeArguments (F a0 ty) = a0:(typeArguments ty)
typeArguments _ = []

typeFinalResult :: TypeOf h -> TypeOf h
typeFinalResult (F _ ty) = typeFinalResult ty
typeFinalResult ty = ty

makeFunctionType :: [TypeOf h] -> TypeOf h -> TypeOf h
makeFunctionType args result = foldr F result args


testModel :: Spec
testModel = do
  it "function arguments and results" $ do
    let ty = F (T "a") (F (T "b") (T "c")) :: MonoType
    let args = typeArguments ty
    let result = typeFinalResult ty
    args `shouldBe` [T "a", T "b"]
    result `shouldBe` T "c"
    makeFunctionType args result `shouldBe` ty
