{-# OPTIONS_GHC -Wall #-}

module Model where


import Test.Hspec


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
          deriving (Show, Eq, Ord)


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


typeArguments :: Type -> [Type]
typeArguments (F a0 ty) = a0:(typeArguments ty)
typeArguments _ = []

typeFinalResult :: Type -> Type
typeFinalResult (F _ ty) = typeFinalResult ty
typeFinalResult ty = ty

makeFunctionType :: [Type] -> Type -> Type
makeFunctionType args result = foldr F result args


testModel :: Spec
testModel = do
  it "function arguments and results" $ do
    let ty = F (T "a") (F (T "b") (T "c"))
    let args = typeArguments ty
    let result = typeFinalResult ty
    args `shouldBe` [T "a", T "b"]
    result `shouldBe` T "c"
    makeFunctionType args result `shouldBe` ty
