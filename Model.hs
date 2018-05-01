
import Data.Map (Map)
import qualified Data.Map as Map

-- interface

data Value = I Integer
           | S String
           | Struct String [Value]  -- fqname, args
           | Func String [Value]    -- fqname, curriedargs
           deriving (Show, Eq)

newtype Toplevel = Toplevel (Map String (Partial Value))

-- TODO model errors
eval :: Toplevel -> Env -> Expr -> Partial Value
eval top env e = case e of
  Int i -> pure (I i)
  Str s -> pure (S s)
  Var x -> pure (lookupVar top x)
  App f x -> apply top <$> eval top env f <*> eval top env x
  Match scrut cases -> match <$> eval top env scrut <*> pure cases

apply :: Toplevel -> Value -> Value -> Partial Value
apply top (Func name curriedargs) arg = error "TODO lookup name in toplevel to get function info"


lookupVar :: Toplevel -> String -> Value
lookupVar = undefined

lookupFunc :: Toplevel -> String -> ([String], Expr)
lookupFunc = undefined

match :: Toplevel -> Value -> [Case] -> SOMETHING
-- match -> local vars -> subst, or env
--  - subst requires embedding or quoting Value in Expr
--  - env is more honest?
{-

- env is more efficient
- env more closely matches the intuition!

-}


-- implementation

data Partial v = Done v | Continue (Partial v)
instance Functor Partial where
  fmap f v = (pure f) <*> v

instance Applicative Partial where
  pure = Done
  f <*> v = case (f, v) of
             (Done f', Done v') -> Done (f' v')
             (Continue f'', Continue v'') -> Continue (f'' <*> v'')
             (Continue f'', _) -> Continue (f'' <*> v)
             (_, Continue v'') -> Continue (f <*> 


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

data Case = Case Pat Expr deriving (Show, Eq)

data Pat = H String
         | C String [Pat]
         deriving (Show, Eq)
