{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck (
  checkProgram,
  explain,
  testTypeCheck,
  ) where

import Control.Monad (void)
import Data.Map (Map)
import Data.Void
import qualified Data.Map as Map
import Test.Hspec

import Util
import Model


testTypeCheck :: Spec
testTypeCheck = do
  let natDef = DefData "Nat" [Variant "Z" [], Variant "S" [T "Nat"]]
  let twoDef = DefVal "two" (Just (T "Nat")) [
        Case [] (App (Cst "S")  (App (Cst "S")  (App (Cst "S") (Cst "Z"))))]
  let plusDef = DefVal "plus" (Just (F (T "Nat") (F (T "Nat") (T "Nat")))) [
        Case [Constructor "Z" [], Hole "y"] (Var "y"),
        Case [Constructor "S" [Hole "x"], Hole "y"] (App (Cst "S")
                                                     (App (App (Var "plus")
                                                           (Var "x"))
                                                      (Var "y")))
        ]
  it "nats example" $ do
    checkProgram [natDef, twoDef, plusDef] `shouldBe` [natDef, twoDef, plusDef]
  it "missing arg type" $ do
    let plusDefWrong = DefVal "plus" (Just (F (T "Nat") (T "Nat"))) [
          --                         ^-- should be Nat -> Nat -> Nat, 2 args.
          Case [Constructor "Z" [], Hole "y"] (Var "y"),
          Case [Constructor "S" [Hole "x"], Hole "y"] (App (Cst "S")
                                                       (App (App (Var "plus")
                                                             (Var "x"))
                                                        (Var "y")))
          ]
    let err = MissingArgumentType { valueName = "plus"
                                  , numPatterns = 2
                                  , declaredType = (F (T "Nat") (T "Nat"))
                                  }
    checkProgram [natDef, twoDef, plusDefWrong]
      `shouldBe` [natDef, twoDef, Error (explain err)]
  it "missing pattern type" $ do
    let plusDefWrong = DefVal "plus" (Just (F (T "Nat") (F (T "Nat") (T "Nat")))) [
          Case [Hole "y"] (Var "y"),
          --   ^-- should have 2 patterns
          Case [Constructor "S" [Hole "x"], Hole "y"] (App (Cst "S")
                                                       (App (App (Var "plus")
                                                             (Var "x"))
                                                        (Var "y")))
          ]
    let err = ExpressionTypeMismatch { expression = (Var "y")
                                     , expectedType = (F (T "Nat") (T "Nat"))
                                     , actualType = (T "Nat")
                                     }
    checkProgram [natDef, twoDef, plusDefWrong]
      `shouldBe` [natDef, twoDef, Error (explain err)]
  it "shadowing" $ do
    let prog = [ DefData "Unit" [Variant "Unit"[]]
               , DefVal "id" (Just (F (T "Unit") (T "Unit"))) [
                    -- id id = id   -- rightmost id refers to the param, not the global.
                    Case [Hole "id"] (Var "id")
                    ]
               ]
    checkProgram prog `shouldBe` prog



type TC v = Either TypeError v

-- for now the type checker still only computes monotypes.
type Type = MonoType

data TypeError = MissingAnnotation { name :: Lowercase }
               | MissingArgumentType { valueName :: String
                                     , numPatterns :: Int
                                     , declaredType :: Type
                                     }
               | Unbound { name :: String }
               | ConstructorArity { cname :: Uppercase
                                  , expectedNumArgs :: Int
                                  , actualNumArgs :: Int
                                  }
               | PatternTypeMismatch { pattern :: Pattern
                                     , expectedType :: Type
                                     , actualType :: Type
                                     }
               | CallNonFunction { callee :: Expr
                                 , calleeType :: Type
                                 , argument :: Expr
                                 }
               | ExpressionTypeMismatch { expression :: Expr
                                        , expectedType :: Type
                                        , actualType :: Type
                                        }
               | ContravariantTypeRecursion { typeNames :: [Uppercase] }
               deriving (Show, Eq)

instance Explain TypeError where
  explain (MissingAnnotation { name }) = "You need to write a type annotation for " ++ name
  explain (MissingArgumentType { valueName, numPatterns, declaredType }) =
    "This equation for " ++ valueName ++ " has " ++ show numPatterns ++ " arguments,"
    ++ " but its type (" ++ show declaredType ++ ")"
    ++ " has " ++ show (length (typeArguments declaredType))
  explain (Unbound { name }) =
    name ++ " is not defined--or doesn't have an explicit type annotation--sorry! :("
  explain (ConstructorArity { cname, expectedNumArgs, actualNumArgs }) =
    "Constructor `" ++ cname ++ "` should have "
    ++ nargs expectedNumArgs ++ " but has been given " ++ nargs actualNumArgs
    where nargs 1 = "1 argument"
          nargs n = show n ++ " arguments"
  explain (PatternTypeMismatch { pattern, expectedType, actualType }) =
    "The pattern `" ++ show pattern ++ "` is supposed to have type " ++ show expectedType
    ++ " but actually has type " ++ show actualType
  explain (CallNonFunction { callee, calleeType, argument }) =
    "You tried to apply `" ++ show callee ++ "` to `" ++ show argument
    ++ " but the callee is not a function; it has type " ++ show calleeType
  explain (ExpressionTypeMismatch { expression, expectedType, actualType }) =
    "The expression `" ++ show expression ++ "` is supposed to have type " ++ show expectedType
    ++ " but actually has type " ++ show actualType
  explain (ContravariantTypeRecursion { typeNames }) =
    "These types form a contravariant cycle: " ++ show typeNames


type Env = Map String TypeSchema

typeLookup :: String -> Env -> TC Type
typeLookup name env = case Map.lookup name env of
  Just ty -> instantiate ty
  Nothing -> Left (Unbound name)

-- abstract takes a typechecker type and returns a TypeSchema.
-- It "abstracts over" a concrete type by replacing unification variables with
-- forall-qualified variables.
abstract :: Type -> TypeSchema
abstract (T t) = T t
abstract (F inn out) = F (abstract inn) (abstract out)
abstract (H h) = absurd h

-- monoType just creates a non-polymorphic type schema.
monoType :: MonoType -> TypeSchema
monoType (T t) = T t
monoType (F inn out) = F (monoType inn) (monoType out)
monoType (H h) = absurd h

-- instantiate "copies" a type schema, generating fresh unification variables
-- for each of the forall-qualified variables.
instantiate :: TypeSchema -> TC Type
instantiate (T t) = return (T t)
instantiate (F inn out) = do
  inn' <- instantiate inn
  out' <- instantiate out
  return (F inn' out')
  
instantiate (H _) = error "TODO implement polymorphism"


checkProgram :: Program -> Program
-- Running checkProgramOnce can remove a definition if it has an error,
-- but it doesn't remove other defs that depend on that one.
-- So you need to keep typechecking until it converges.
-- The resulting program has no type errors.
-- (This is always possible: in the worst case, if every def is bad,
--  you get the empty program.)
checkProgram = converge checkProgramOnce

checkProgramOnce :: Program -> Program
checkProgramOnce prog =
  let env = unions (map scanStmt prog) in
  map (checkStmtRecover env) prog

checkStmtRecover :: Env -> Stmt -> Stmt
checkStmtRecover env stmt = case checkStmt env stmt of
  Left err -> Error (explain err)
  Right () -> stmt

-- returns a partial environment
scanStmt :: Stmt -> Env
scanStmt (DefData tyname variants) = unions (map (scanVariant tyname) variants)
scanStmt (DefVal _ Nothing _) = Map.empty
scanStmt (DefVal vname (Just ty) _) = Map.fromList [(vname, ty)]
scanStmt (Expr _) = Map.empty
scanStmt (Error _) = Map.empty

scanVariant :: Uppercase -> Variant -> Env
scanVariant tyname (Variant cname argtypes) = Map.fromList [(cname, monoType ctype)]
  where ctype = makeFunctionType argtypes (T tyname)

checkStmt :: Env -> Stmt -> TC ()
-- A defdata introduces no new constraints to check;
-- we already handled it by creating the env.
checkStmt _   (DefData _ _) = return ()
-- An expr we just check bottom-up.
-- Ditto for un-annotated defs with no params and one case.
checkStmt env (Expr expr) = void (checkExpr env expr)
checkStmt env (DefVal _    Nothing [Case [] expr]) = void (checkExpr env expr)
-- Un-annotated defs in general are not supported yet.
-- We only do type checking, not type inference.
checkStmt _   (DefVal name Nothing _) = Left (MissingAnnotation name)
-- When there is an annotation, check each case against it.
checkStmt env (DefVal name (Just ty) cases) = do
  ty' <- instantiate ty
  mapM_ (checkCase name env ty') cases
checkStmt _   (Error _) = return ()

checkCase :: String -> Env -> Type -> Case -> TC ()
checkCase name env ty (Case pats expr) = do
  (typedPats, tyExpr) <- (zipPats ty pats
                          `orFail` MissingArgumentType name (length pats) ty)
  binds <- checkPatterns env typedPats
  let env' = env `shadowedBy` binds
  checkExprWithType env' expr tyExpr

shadowedBy :: Ord k => Map k v -> Map k v -> Map k v
shadowedBy = flip Map.union


-- TODO unions should return a type error instead when things collide
unions :: Ord k => [Map k v] -> Map k v
unions = Map.unionsWith (error "Map key collision")

-- tries to zip the patterns with the function arguments.
-- will fail if there are more patterns than function arguments.
zipPats :: Type -> [Pattern] -> Maybe ([(Type, Pattern)], Type)
zipPats ty [] = Just ([], ty)
zipPats (F inn out) (p0:ps) = case zipPats out ps of
  Nothing -> Nothing
  Just (pairs, final) -> Just ((inn, p0):pairs, final)
zipPats _ (_:_) = Nothing


checkPatterns :: Env -> [(Type, Pattern)] -> TC Env
checkPatterns env typedPats = unions <$> mapM (checkPattern env) typedPats

-- patterns are checked top-down.
-- returns a partial environment - only the new bindings
checkPattern :: Env -> (Type, Pattern) -> TC Env
-- When checking a pattern, typechecker-types flow in from the outmost form,
-- and stop when they reach a hole. (This is like the dual of expressions!)
-- 
checkPattern _ (ty, Hole name) = return (Map.fromList [(name, abstract ty)])
checkPattern env (ty, pat@(Constructor cname argpats)) = do
  ctype <- typeLookup cname env
  -- In contrast to zipPats,
  -- here we check if the number of args exactly matches.
  let ctyArgs = typeArguments ctype
  let ctyResult = typeFinalResult ctype
  if ctyResult /= ty
    then Left (PatternTypeMismatch { pattern = pat
                                   , expectedType = ty
                                   , actualType = ctyResult
                                   })
    else do
      typedPats <- (tryZip ctyArgs argpats
                    `orFail` ConstructorArity { cname = cname
                                              , expectedNumArgs = length ctyArgs
                                              , actualNumArgs = length argpats
                                              })
      checkPatterns env typedPats


checkExprWithType :: Env -> Expr -> Type -> TC ()
checkExprWithType env expr expectedType = do
  actualType <- checkExpr env expr
  if expectedType == actualType
    then return ()
    else Left (ExpressionTypeMismatch { expression = expr
                                      , expectedType = expectedType
                                      , actualType = actualType
                                      })


-- expressions are checked bottom-up.
checkExpr :: Env -> Expr -> TC Type
checkExpr env (Var name) = typeLookup name env
checkExpr env (Cst name) = typeLookup name env
checkExpr env (App callee arg) = do
  calleeType <- checkExpr env callee
  (expectedArgType, result) <- case calleeType of
                                (F inn out) -> return (inn, out)
                                _ -> Left (CallNonFunction { callee = callee
                                                           , calleeType = calleeType
                                                           , argument = arg
                                                           })
  checkExprWithType env arg expectedArgType
  return result


tryZip :: [a] -> [b] -> Maybe [(a, b)]
tryZip [] [] = return []
tryZip (x:xs) (y:ys) = do
  xys <- tryZip xs ys
  return ((x, y):xys)
tryZip _ _ = Nothing


orFail :: Maybe good -> bad -> Either bad good
orFail Nothing bad = Left bad
orFail (Just good) _ = Right good
