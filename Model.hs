

data Value = I Integer
           | S String
           | Struct { name :: String, args :: [Value] }
           | Func { name :: String, curriedArgs :: [Value] }
           deriving (Show, Eq)

data Statement = DefStruct { name :: String, arity :: Integer }
               | DefVal { name :: String, expr :: Expr }
               | DefFun { name :: String, params :: [String], body :: Expr }
               | E Expr
               deriving (Show, Eq)

data Expr = Int Integer
          | Str String
          | Var String
          | App Expr Expr
          | Match Expr [Case]
          deriving (Show, Eq)

data Case = Case Pat Expr

data Pat = H String
         | C String [Pat]
         deriving (Show, Eq)
