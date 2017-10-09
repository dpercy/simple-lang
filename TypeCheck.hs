{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck (
  checkProgram,
  explain,
  testTypeCheck,
  ) where


import Data.Map (Map)
import qualified Data.Map as Map
import Test.Hspec

import Model


testTypeCheck :: Spec
testTypeCheck = do
  it "nats example" $ do
    checkProgram [ DefData "Nat" [Variant "Z" [], Variant "S" [T "Nat"]]
                 , DefVal "two" (Just (T "Nat")) [
                      Case [] (App (Cst "S")  (App (Cst "S")  (App (Cst "S") (Cst "Z"))))
                      ]
                 , DefVal "plus" (Just (F (T "Nat") (F (T "Nat") (T "Nat")))) [
                      Case [Constructor "Z" [], Hole "y"] (Var "y"),
                      Case [Constructor "S" [Hole "x"], Hole "y"] (App (Cst "S")
                                                                   (App (App (Var "plus")
                                                                         (Var "x"))
                                                                    (Var "y")))
                      ]
                 ] `shouldBe` Right ()
  it "missing arg type" $ do
    checkProgram [ DefData "Nat" [Variant "Z" [], Variant "S" [T "Nat"]]
                 , DefVal "two" (Just (T "Nat")) [
                      Case [] (App (Cst "S")  (App (Cst "S")  (App (Cst "S") (Cst "Z"))))
                      ]
                 , DefVal "plus" (Just (F (T "Nat") (T "Nat"))) [
                      Case [Constructor "Z" [], Hole "y"] (Var "y"),
                      Case [Constructor "S" [Hole "x"], Hole "y"] (App (Cst "S")
                                                                   (App (App (Var "plus")
                                                                         (Var "x"))
                                                                    (Var "y")))
                      ]
                 ] `shouldBe` Left (MissingArgumentType { valueName = "plus"
                                                        , numPatterns = 2
                                                        , declaredType = (F (T "Nat") (T "Nat"))
                                                        })
  it "missing pattern type" $ do
    checkProgram [ DefData "Nat" [Variant "Z" [], Variant "S" [T "Nat"]]
                 , DefVal "two" (Just (T "Nat")) [
                      Case [] (App (Cst "S")  (App (Cst "S")  (App (Cst "S") (Cst "Z"))))
                      ]
                 , DefVal "plus" (Just (F (T "Nat") (F (T "Nat") (T "Nat")))) [
                      Case [Hole "y"] (Var "y"),
                      Case [Constructor "S" [Hole "x"], Hole "y"] (App (Cst "S")
                                                                   (App (App (Var "plus")
                                                                         (Var "x"))
                                                                    (Var "y")))
                      ]
                 ] `shouldBe` Left (ExpressionTypeMismatch { expression = (Var "y")
                                                           , expectedType = (F (T "Nat") (T "Nat"))
                                                           , actualType = (T "Nat")
                                                           })


type TC v = Either TypeError v

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
               deriving (Show, Eq)

explain :: TypeError -> String
explain (MissingAnnotation { name }) = "You need to write a type annotation for " ++ name
explain (MissingArgumentType { valueName, numPatterns, declaredType }) =
  "This equation for " ++ valueName ++ " has " ++ show numPatterns ++ " arguments,"
  ++ " but its type (" ++ show declaredType ++ ")"
  ++ " has " ++ show (length (typeArguments declaredType))
explain (Unbound { name }) = name ++ " is not defined"
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


type Env = Map String Type

typeLookup :: String -> Env -> TC Type
typeLookup name env = Map.lookup name env `orFail` Unbound name


checkProgram :: Program -> TC ()
checkProgram prog =
  -- TODO distinguish between shadowing-union and noncolliding-union
  --   - noncolliding union should make a type error for nonlinear patterns or duped globals
  let env = Map.unions (map scanDef prog) in
  mapM_ (checkDef env) prog

-- returns a partial environment
scanDef :: Def -> Env
scanDef (DefData tyname variants) = Map.unions (map (scanVariant tyname) variants)
scanDef (DefVal _ Nothing _) = Map.empty
scanDef (DefVal vname (Just ty) _) = Map.fromList [(vname, ty)]

scanVariant :: Uppercase -> Variant -> Env
scanVariant tyname (Variant cname argtypes) = Map.fromList [(cname, ctype)]
  where ctype = makeFunctionType argtypes (T tyname)

checkDef :: Env -> Def -> TC ()
checkDef _ (DefData _ _) = return ()
checkDef _ (DefVal name Nothing _) = Left (MissingAnnotation name)
checkDef env (DefVal name (Just ty) cases) = mapM_ (checkCase name env ty) cases

checkCase :: String -> Env -> Type -> Case -> TC ()
checkCase name env ty (Case pats expr) = do
  (typedPats, tyExpr) <- (zipPats ty pats
                          `orFail` MissingArgumentType name (length pats) ty)
  binds <- checkPatterns env typedPats
  -- TODO test that these unions go the right way
  let env' = Map.union env binds
  checkExprWithType env' expr tyExpr

-- tries to zip the patterns with the function arguments.
-- will fail if there are more patterns than function arguments.
zipPats :: Type -> [Pattern] -> Maybe ([(Type, Pattern)], Type)
zipPats ty [] = Just ([], ty)
zipPats (F inn out) (p0:ps) = case zipPats out ps of
  Nothing -> Nothing
  Just (pairs, final) -> Just ((inn, p0):pairs, final)
zipPats _ (_:_) = Nothing


checkPatterns :: Env -> [(Type, Pattern)] -> TC Env
checkPatterns env typedPats = Map.unions <$> mapM (checkPattern env) typedPats

-- patterns are checked top-down.
-- returns a partial environment - only the new bindings
checkPattern :: Env -> (Type, Pattern) -> TC Env
checkPattern _ (ty, Hole name) = return (Map.fromList [(name, ty)])
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
