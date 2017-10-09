{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeInference (
  testTypeInference,
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Test.Hspec

import Model
import MiniKanren hiding (Goal)
import qualified MiniKanren

type Goal a = MiniKanren.Goal (Type TyVar) a


newtype TyVar = TyVar Int
              deriving (Show, Eq)

instance Unify (Type TyVar) where
  zipChildren (T name1) (T name2) = if name1 == name2 then Just [] else Nothing
  zipChildren (F a1 b1) (F a2 b2) = Just [(a1, a2), (b1, b2)]
  zipChildren _ _ = Nothing

  mapChildren f (F a b) = F (f a) (f b)
  mapChildren _ (B hole) = B hole
  mapChildren _ (T name) = T name

  injectVar = B . TyVar
  
  projectVar (B (TyVar i)) = Just i
  projectVar _ = Nothing
  


testTypeInference :: Spec
testTypeInference = do
  it "empty prog gives empty env" $ do
    reconstructProgram


makeFunc :: [Type a] -> Type a -> Type a
makeFunc [] t = t
makeFunc (x0:xs) t = F x0 (makeFunc xs t)


-- we want to take a partially-annotated program and return a fully-annotated one.
-- TODO modify runGoal to say "failed _because_ ..."
infer :: Program InferMe -> Either TypeError Env
infer prog =
  let goalEnv :: Goal Env
      goalEnv = reconstructProgram prog
  in
  case runGoal goalEnv of
   [] -> Left (TypeError "failed to unify I guess?")
   [env] -> Right env
   envs -> Left (TypeError ("more than one? " ++ show envs))

newtype TypeError = TypeError String deriving (Show, Eq)

-- env maps what?
--   - constructors to types
--   - local variables to types
--   - global variables to types
type Env = Map String (Type TyVar)



reconstructProgram :: Program InferMe -> Goal Env
reconstructProgram prog = do
  env <- initEnv prog
  -- TODO put some constraints on env
  return env
  


initEnv :: Program InferMe -> Goal Env
initEnv defs = Map.unions <$> mapM ieDef defs 
  where ieDef :: Def InferMe -> Goal Env
        ieDef = undefined



-- initEnv defs = Map.fromList $ do
--   def <- defs
--   case def of
--    DefData name variants -> return (map (initEnvVariant name) variants)
--      where initEnvVariant tyname (Variant vname argtypes) =
--              (vname, makeFunc argtypes (T tyname))
--    DefVal name ty _cases -> do
--      var <- fresh
     
     



caseHasType :: Env -> Case -> Type TyVar -> Goal ()
caseHasType = undefined


-- patternHasType also produces a new environment
patternHasType :: Env -> Pattern -> Type TyVar -> Goal Env
patternHasType = undefined


exprHasType :: Env -> Expr -> Type TyVar -> Goal ()
exprHasType = undefined
