{-# OPTIONS_GHC -Wall #-}

module Model where


import Data.Void


-- programs consist of data definitions, and value definitions
type Program t = [Def t]

data Def t = DefData Uppercase [Variant]
           | DefVal Lowercase (Type t) [Case]
           deriving (Show, Eq)


-- data definitions have a name and a set of variants.
-- each variant defines a single constructor: its name, and its argument types.
data Variant = Variant Uppercase [MonomorphicType] -- note: no generic containers for now.
             deriving (Show, Eq)

-- a type is either:
--   - a named type (defined by a data definition)
--   - a function type (which contains an argument type and a result type)
--   - a "blank", which can stand in for InferMe or for TyVar.
data Type a = T Uppercase
            | F (Type a) (Type a)
            | B a
            deriving (Show, Eq)

-- A written program can leave out types, so (B InferMe) represents holes that should be
-- inferred by the type checker.
data InferMe = InferMe deriving (Show, Eq)

-- A monomorphic type has no holes and no type variables.
-- So the B case doesn't exist.
type MonomorphicType = Type Void

monomorphic :: Type Void -> Type a
monomorphic (F a b) = F (monomorphic a) (monomorphic b)
monomorphic (T name) = T name
monomorphic (B v) = absurd v

--
data Case = Case [Pattern] Expr
          deriving (Show, Eq)

data Pattern = Hole Lowercase
             | Constructor Uppercase [Pattern]
             deriving (Show, Eq)

data Expr = Var Lowercase
          | Cst Uppercase
          | App Expr Expr
          deriving (Show, Eq)

type Lowercase = String
type Uppercase = String

