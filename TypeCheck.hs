{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeCheck (
  checkProgram,
  explain,
  testTypeCheck,
  ) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad (void)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Maybe as Maybe

import Data.Void

import Test.Hspec

import Util hiding (recover)
import Model


testTypeCheck :: Spec
testTypeCheck = do
  it "test error recovery" $ do
    -- I want a failed computation to not mess with the store.
    let mutateThenFail :: TC Stmt
        mutateThenFail = do void newTyVar
                            void (throwError (Unbound "ouch"))
                            return (Expr (Var "success"))
    runTC mutateThenFail `shouldBe` Left (Unbound "ouch")
    recover mutateThenFail `shouldBe` Error (explain (Unbound "ouch"))
    let mutateFailAndCatch = do (void mutateThenFail) `catchError` \_err -> return ()
                                Map.size <$> get
    runTC mutateFailAndCatch `shouldBe` Right 0
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
    checkProgram [natDef, twoDef, plusDefWrong]
      `shouldBe` [natDef, twoDef, Error "TODO make better error explanations"]
  it "missing pattern type" $ do
    let plusDefWrong = DefVal "plus" (Just (F (T "Nat") (F (T "Nat") (T "Nat")))) [
          Case [Hole "y"] (Var "y"),
          --   ^-- should have 2 patterns
          Case [Constructor "S" [Hole "x"], Hole "y"] (App (Cst "S")
                                                       (App (App (Var "plus")
                                                             (Var "x"))
                                                        (Var "y")))
          ]
    checkProgram [natDef, twoDef, plusDefWrong]
      `shouldBe` [natDef, twoDef, Error "TODO make better error explanations"]
  it "shadowing" $ do
    let prog = [ DefData "Unit" [Variant "Unit"[]]
               , DefVal "id" (Just (F (T "Unit") (T "Unit"))) [
                    -- id id = id   -- rightmost id refers to the param, not the global.
                    Case [Hole "id"] (Var "id")
                    ]
               ]
    checkProgram prog `shouldBe` prog


newtype TyVar = TyVar Int deriving (Show, Eq, Ord)
type Type = TypeOf TyVar

-- Every unification variable that "exists" is in the store.
-- - (Nothing) means the variable is still free to be instantiated
-- - (Just t) means a previous constraint forces the variable to be equal to t.
type Store = Map TyVar (Maybe Type)

emptyStore :: Store
emptyStore = Map.empty

newTyVar :: TC TyVar
newTyVar = do
  store <- (get :: TC Store)
  let tyvar = TyVar (Map.size store) -- use map size as counter
  let store' :: Store
      store' = Map.insert tyvar Nothing store
  put store'
  return tyvar


newtype TC v = TC (StateT Store (Except TypeError) v)
             deriving ( Applicative
                      , Functor
                      , Monad
                      , MonadState Store
                      , MonadError TypeError
                      )

runTC :: TC v -> Either TypeError v
runTC (TC comp) = runExcept (evalStateT comp emptyStore)


-- unify takes two types, which can contain unassigned tyvars,
-- and mutates unassigned tyvars as necessary to make the types equal.
unify :: Type -> Type -> TC ()
unify t1 t2 = do t1' <- walk t1
                 t2' <- walk t2
                 unify' t1' t2'

unifys :: [Type] -> [Type] -> TC ()
unifys [] [] = return ()
unifys (x:xs) (y:ys) = do unify x y; unifys xs ys
unifys _ _ = error "unifys unequal lists"

-- removes tyvar wrappers from the top level
walk :: Type -> TC Type
walk inputTy@(H tyvar) = do maybeTy <- derefTyVar tyvar
                            case maybeTy of
                             Nothing -> return inputTy
                             Just boundTy -> walk boundTy
-- any root constructor besides H is not a tyvar (so it's done).
walk inputTy = return inputTy

-- unify' assumes its two inputs have already been walked
unify' :: Type -> Type -> TC ()
unify' (T x)     (T y) | x == y = return ()
unify' (F a b)   (F x y)        = do unify a x; unify b y
unify' (H tyvar) ty             = assign tyvar ty
unify' ty        (H tyvar)      = assign tyvar ty
unify' t1        t2             = throwError (FailedToUnify t1 t2)

-- assumes the input tyvar is unassigned.
-- mutates the tyvar (by updating the Store),
-- but first checks that the ty does not contain the given tyvar.
-- This ensues that the store is acyclic, which is important
-- so that walk terminates
assign :: TyVar -> Type -> TC ()
assign tyvar ty = do
  if ty `containsTyVar` tyvar
    then throwError (CyclicType tyvar ty)
    else return ()
  store <- (get :: TC Store)
  let store' = Map.insert tyvar (Just ty) store
  put store'
  return ()

containsTyVar :: Type -> TyVar -> Bool
ty `containsTyVar` tyvar = tyvar `elem` holes ty

holes :: TypeOf a -> [a]
holes (T _) = []
holes (F a b) = holes a ++ holes b
holes (H h) = [h]

data TypeError = FailedToUnify { left :: Type, right :: Type }
               | CyclicType { tyvar :: TyVar, ty :: Type }
               | NotASubtype { declared :: TypeSchema, inferred :: TypeSchema }
                 
               | Unbound { name :: String }
               | ConstructorArity { cname :: Uppercase
                                  , expectedNumArgs :: Int
                                  , actualNumArgs :: Int
                                  }
               deriving (Show, Eq)

instance Explain TypeError where
  explain (FailedToUnify { left, right }) =
    "Failed to unify: " ++ show left ++ " with " ++ show right
  explain (CyclicType { tyvar, ty }) =
    "Can't construct the infinite type " ++ show (H tyvar) ++ "=" ++ show ty
  -- TODO "subtype" is not the right word here. More like "instantiation".
  -- We don't actually have 'subtyping': instead, a type schema represents a whole family of types.
  -- If a value v has type (schema) T, then for *any* instantiation T' of T we can say v :: T'.
  -- It's almost like v has an infinite intersection type!
  -- id :: (a -> a) implies id :: (Int -> Int) *and* id :: (String -> String) *and* id :: (Foo -> Foo)...
  -- Whereas with subtyping, if I say x :: Object,
  -- it's absolutely false that x :: String *and* x :: Integer
  -- (instead, at most one of these can be true).
  explain (NotASubtype { declared, inferred }) =
    "Inferred type " ++ show declared ++ " is not a subtype of " ++ show inferred
  explain (Unbound { name }) =
    name ++ " is not defined--or doesn't have an explicit type annotation--sorry! :("
  explain (ConstructorArity { cname, expectedNumArgs, actualNumArgs }) =
    "Constructor `" ++ cname ++ "` should have "
    ++ nargs expectedNumArgs ++ " but has been given " ++ nargs actualNumArgs
    where nargs 1 = "1 argument"
          nargs n = show n ++ " arguments"

-- Env maps names-of-values to types.
-- Globals always have a type schema.
-- Locals always have a partially-inferred type (just "Type").
newtype Env = Env { getEnv :: Map String (Either TypeSchema Type) }

emptyEnv :: Env
emptyEnv = Env (Map.empty)

-- takes the name of a value (such as a constructor or local),
-- and returns a typechecker-type for it.
-- Automatically instantiates the type if it's a TypeSchema.
typeLookup :: String -> Env -> TC Type
typeLookup name (Env env) = case Map.lookup name env of
  Nothing -> throwError (Unbound name)
  Just (Left ts) -> instantiate ts
  Just (Right t) -> return t

derefTyVar :: TyVar -> TC (Maybe Type)
derefTyVar tyvar = do
  maybeMaybeTy <- Map.lookup tyvar <$> (get :: TC Store)
  case maybeMaybeTy of
   Nothing -> error "TyVar not in store???"
   Just maybeTy -> return maybeTy 

-- abstract takes a typechecker type and returns a TypeSchema.
-- It "abstracts over" a concrete type by replacing unification variables with
-- forall-qualified variables.
abstract :: Type -> TC TypeSchema
abstract (T t) = return (T t)
abstract (F inn out) = F <$> abstract inn <*> abstract out
-- TyVar case is actually tricky!
--  - free tyvars can become foralls
--  - bound tyvars must become their bound value
abstract (H tyvar@(TyVar i)) = do
  maybeTy <- derefTyVar tyvar
  case maybeTy of
   Nothing -> return (H ("x" ++ show i))
   -- This only terminates if the substitution is acyclic!
   Just ty -> abstract ty

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
instantiate (H _) = H <$> newTyVar


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
  map (checkStmt env) prog

-- returns a partial environment
scanStmt :: Stmt -> Env
scanStmt (DefData tyname variants) = unions (map (scanVariant tyname) variants)
scanStmt (DefVal _ Nothing _) = emptyEnv
scanStmt (DefVal vname (Just ty) _) = Env (Map.fromList [(vname, Left ty)])
scanStmt (Expr _) = emptyEnv
scanStmt (Error _) = emptyEnv

scanVariant :: Uppercase -> Variant -> Env
scanVariant tyname (Variant cname argtypes) = Env$Map.fromList [(cname, Left (monoType ctype))]
  where ctype = makeFunctionType argtypes (T tyname)

-- checkStmt:
--  - always succeeds,
--  - always returns a new Stmt,
--  - has no observable Store effects (no reads, no writes),
--      because its inputs and outputs contain no references to Store.
-- Therefore, it's safe to use 'recover' to run checkStmt
-- in an empty store.
{-

TODO this is wrong.
For example, try runnin the demo server and erasing the type decl for "plus".
Any statement that uses "plus" gets a type error.

This checkStmt isolates the TC state to a single statement.
That's wrong!
Instead, constraints from many call sites help determine the type
of an identifier, which we want to insert at its def site.

The definition of scanStmt assumes we know the type decls at the start,
but that's exactly wrong.
Instead, the env needs to hold the *partially-known* type of each identifier
as we discover and solve new constraints.

But at the same time, we need to keep the constraints separate somehow!
Because you want "map" and "id" to be polymorphic.
((id id) 1) must work.


id :: ?
id x = x  -- id has type  checkCase (\x -> x)

((id_1 id_2) 1)



Maybe this would all be easier by explicitly generating constraints!
You can use a monad for fresh variables and writing constraints.
Explicit constraints might also make it easier to make good error messages,
by putting metadata in the constraints.




-}
checkStmt :: Env -> Stmt -> Stmt
-- A defdata introduces no new constraints to check;
-- we already handled it by creating the env.
checkStmt _   s@(DefData _ _) = s
-- An expr we just check bottom-up.
-- Ditto for un-annotated defs with no params and one case.
checkStmt env (Expr expr) = recover $ do void (checkExpr env expr)
                                         return (Expr expr)
checkStmt env s@(DefVal name maybeDeclTy cases) = recover $ do
  caseTypes <- mapM (checkCase env) cases
  casesType <- H <$> newTyVar
  mapM_ (unify casesType) caseTypes
  inferredType <- abstract casesType
  case maybeDeclTy of
   -- No declared type -> fill in using the inferred type
   Nothing -> return (DefVal name (Just inferredType) cases)
   -- Declared type -> the declared type must be an instance of the inferred type
   -- (If you declare (id) as (Int -> Int) that's ok,
   -- because the declared type (Int -> Int)
   -- is an instance of the inferred type (a -> a).
   -- But if you declare (+ 1) as (a -> a) that's bad,
   -- because the declared type (a -> a) is not an instance of (Int -> Int).
   Just declaredType -> do declaredType `checkSubtypeOf` inferredType
                           return s
checkStmt _   s@(Error _) = s

-- Run the given computation in an empty store,
-- and replaces failure with an Error statement.
recover :: TC Stmt -> Stmt
recover comp = case runTC comp of
  Right stmt -> stmt
  Left err -> Error (explain err)

checkSubtypeOf :: TypeSchema -> TypeSchema -> TC ()
checkSubtypeOf ts1 ts2 = do
  -- ts1 is a subtype of ts2 if they unify such that
  -- all type variables in ts1 are left free.
  -- If any type variables in ts1 need to be specialized to make them unify,
  -- then ts1 can't be a subtype of ts2.
  -- Also, if they don't unify at all then neither is a subtype of the other.
  t1 <- instantiate ts1
  t2 <- instantiate ts2
  unify t1 t2 `catchError` \_err -> throwError (NotASubtype ts1 ts2)
  let tyvars = holes t1
  binds <- mapM derefTyVar tyvars
  if any Maybe.isJust binds
    then throwError (NotASubtype ts1 ts2)
    else return ()
  
  
-- (case (C1 x...) (C2 y...) -> e) is NOT like (C1 x...) and then (case (C2 y...) -> e).
-- The difference is in the shadowing rules:
-- 1. patterns must be linear (don't shadow other patterns)
-- 2. patterns must shadow globals
-- But I will cheat and allow shadowing. So a rule like f x x = x means f _ x = x.
checkCase :: Env -> Case -> TC Type
checkCase env (Case [] expr) = checkExpr env expr
checkCase env (Case (pat0:pats) expr) = do
  (headPatTy, headPatBinds) <- checkPattern env pat0
  let env' = env `shadowedBy` headPatBinds
  restCaseTy <- checkCase env' (Case pats expr)
  return (F headPatTy restCaseTy)

shadowedBy :: Env -> Env -> Env
shadowedBy (Env outer) (Env inner) = Env (Map.union inner outer)


-- TODO unions should return a type error instead when things collide
unionMaps :: Ord k => [Map k v] -> Map k v
unionMaps = Map.unionsWith (error "Map key collision")

unions :: [Env] -> Env
unions envs = Env (unionMaps (map getEnv envs))


checkPatterns :: Env -> [Pattern] -> TC ([Type], Env)
checkPatterns _ [] = return ([], emptyEnv)
checkPatterns env (pat0:pats) = do
  (ty0, binds0) <- checkPattern env pat0
  (tys, bindss) <- checkPatterns env pats
  return (ty0:tys, unions [binds0, bindss])

checkPattern :: Env -> Pattern -> TC (Type, Env)
checkPattern _  (Hole name) = do
  ty <- H <$> newTyVar
  let binds = Env (Map.singleton name (Right ty))
  return (ty, binds)
checkPattern env (Constructor cname argpats) = do
  ctype <- typeLookup cname env
  -- In contrast to zipPats,
  -- here we check if the number of args exactly matches.
  let ctyArgs = typeArguments ctype
  let ctyResult = typeFinalResult ctype
  if length ctyArgs == length argpats
    then return ()
    else throwError (ConstructorArity { cname = cname
                                      , expectedNumArgs = length ctyArgs
                                      , actualNumArgs = length argpats
                                      })
  (argTys, binds) <- checkPatterns env argpats
  unifys ctyArgs argTys
  return (ctyResult, binds)


-- expressions are checked bottom-up.
checkExpr :: Env -> Expr -> TC Type
checkExpr env (Var name) = typeLookup name env
checkExpr env (Cst name) = typeLookup name env
checkExpr env (App callee arg) = do
  calleeType <- checkExpr env callee
  argType    <- checkExpr env arg
  resultType <- H <$> newTyVar
  unify calleeType (F argType resultType)
  return resultType
