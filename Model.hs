{-# OPTIONS_GHC -Wall #-}

module Model where


-- programs consist of data definitions, and value definitions
type Program = [Def]

data Def = DefData Uppercase [Variant]
           -- defval's type annotation is optional,
           -- but a simple-minded typechecker might reject you if you omit the type.
         | DefVal Lowercase (Maybe Type) [Case]
         deriving (Show, Eq)



-- data definitions have a name and a set of variants.
-- each variant defines a single constructor: its name, and its argument types.
data Variant = Variant Uppercase [Type] -- note: no generic containers for now.
             deriving (Show, Eq)

-- a type is either:
--   - a named type (defined by a data definition)
--   - a function type (which contains an argument type and a result type)
data Type = T Uppercase
          | F Type Type
          deriving (Show, Eq)


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

