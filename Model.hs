module Model where


import Data.Void


type Program t = [Def t]

data Def t = DefVal Lowercase (Type t) [Case]
           | DefData Uppercase [Variant]
           deriving (Show, Eq)

data Case = Case [Pattern] Expr
          deriving (Show, Eq)

data Variant = Variant Uppercase [Type Void]
             deriving (Show, Eq)

data Type a = B a -- blank
            | T Uppercase -- named type
            | F (Type a) (Type a) -- function type
            deriving (Show, Eq)

data InferMe = InferMe
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

