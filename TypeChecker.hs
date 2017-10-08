module TypeChecker (testGenConstraints, testSolveTypeConstraints) where

import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void
import Test.Hspec

import Model

{-

Totality checker in 3 parts:
1. typechecker
    - prevents crashing from bad function/argument combinations
    - prevents omega by restricting type recursion to covariant only
2. case coverage
    - prevents crashing from unhandled cases
3. well-founded recursion checker
    - proves that arguments to recursive calls always shrink

-}



{-

Typechecker


"PLAI" recommends dividing this into two parts:
- constraint generation
- constraint solving

Plus these could be separate passes as well:
- scope checking
- covariant-only type recursion


Example program:

f :: Foo -> Bar -> Quux
f F b     = b       -- case0
f b (B v) = b v     -- case1

constraints:
- typeof_global(f) = Foo -> Bar -> Quux
- typeof_global(f) = typeof_pat(f.0.0) -> typeof_pat(f.0.1) -> typeof_expr(f.0.e)
    - typeof_pat(f.0.0)  = typeof_constructor(F)  -- scan param0
    -                                             -- scan param1: no constraints, but bind (b => typeof_pat(f.0.1))
    - typeof_expr(f.0.e) = typeof_pat(f.0.1)      -- scan expr, with bindings generated by the two params
- typeof_global(f) = typeof_pat(f.1.0) -> typeof_pat(f.1.1) -> typeof_expr(f.1.e)
    -                                             -- scan param0: bind (b => typeof_pat(f.1.0))
    - typeof_pat(f.1.1.a) -> typeof_pat(f.1.1) = typeof_pat(f.1.1.f)    -- and bind (v => typeof_pat(f.1.1.a))
    - typeof_pat(f.1.1.f) -> typeof_constructor(B)
    - typeof_expr(f.1.e.a) -> typeof_expr(f.1.e) = typeof_expr(f.1.e.f)
    - typeof_expr(f.1.e.f) = typeof_pat(f.1.0)
    - typeof_expr(f.1.e.a) = typeof_pat(f.1.1.a)
    



-}

testGenConstraints :: Spec
testGenConstraints = do
  it "data definition" $ do
    (execWriter (cProgram [DefData "NatList" [ Variant "Empty" []
                                             , Variant "Cons" [T "Nat", T "NatList"]]])
     `shouldBe`
     [ TypeEquals (B$TyDataConstructor "Empty") (T "NatList")
     , TypeEquals (B$TyDataConstructor "Cons") (F (T "Nat")
                                              (F (T "NatList")
                                               (T "NatList"))) ])
  it "simple value definition" $ do
    (execWriter (cProgram [DefVal "x" (B InferMe) [Case [] (Cst "Potato")]])
     `shouldBe`
     [ TypeEquals (B$TyGlobal "x") (B TyInferMe)
     , TypeEquals (B$TyGlobal "x") (B$TyExpression "x.case0.expr")
     , TypeEquals (B$TyExpression "x.case0.expr") (B$TyDataConstructor "Potato")
     ])

  it "1-arg function" $ do
    (execWriter (cProgram [DefVal "f" (B InferMe) [ Case [Hole "x"] (Var "x")
                                                  , Case [Hole "x"] (Var "y")]])
     `shouldBe`
     [ TypeEquals (B$TyGlobal "f") (B TyInferMe)
     , TypeEquals (B$TyGlobal "f") (F (B$TyPattern "f.case0.pat0") (B$TyExpression "f.case0.expr"))
     , TypeEquals (B$TyExpression "f.case0.expr") (B$TyPattern "f.case0.pat0")
     , TypeEquals (B$TyGlobal "f") (F (B$TyPattern "f.case1.pat0") (B$TyExpression "f.case1.expr"))
     , TypeEquals (B$TyExpression "f.case1.expr") (B$TyGlobal "y")
     ])
  it "destructuring in a function" $ do
    (execWriter (cProgram [DefVal "f" (B InferMe) [ Case [Constructor "X" []]
                                                    (Var "x")
                                                  , Case [Constructor "Cons" [Hole "a", Hole "b"]]
                                                    (Var "b")]])
     `shouldBe`
     [ TypeEquals (B$TyGlobal "f") (B TyInferMe)
       -- case0
     , TypeEquals (B$TyGlobal "f") (F (B$TyPattern "f.case0.pat0")
                                  (B$TyExpression "f.case0.expr"))
       -- case0 pat0
     , TypeEquals (B$TyDataConstructor "X") (B$TyPattern "f.case0.pat0")
       -- case0 expr
     , TypeEquals (B$TyExpression "f.case0.expr") (B$TyGlobal "x")
       -- case1
     , TypeEquals (B$TyGlobal "f") (F (B$TyPattern "f.case1.pat0")
                                  (B$TyExpression "f.case1.expr"))
       -- case1 pat0
     , TypeEquals (B$TyDataConstructor "Cons")
       (F (B$TyPattern "f.case1.pat0.arg0")
        (F (B$TyPattern "f.case1.pat0.arg1")
         (B$TyPattern "f.case1.pat0")))
       -- case1 expr
     , TypeEquals (B$TyExpression "f.case1.expr") (B$TyPattern "f.case1.pat0.arg1")
     ])
  it "interesting expressions" $ do
    (execWriter (cProgram [DefVal "x" (B InferMe) [ Case [] (App (App (Cst "Cons") (Var "hd")) (Var "tl"))]])
     `shouldBe`
     [ TypeEquals (B$TyGlobal "x") (B TyInferMe)
     , TypeEquals (B$TyGlobal "x") (B$TyExpression "x.case0.expr")
     , TypeEquals (B$TyExpression "x.case0.expr.f")
       (F (B$TyExpression "x.case0.expr.a")
        (B$TyExpression "x.case0.expr"))
     , TypeEquals (B$TyExpression "x.case0.expr.f.f")
       (F (B$TyExpression "x.case0.expr.f.a")
        (B$TyExpression "x.case0.expr.f"))
     , TypeEquals (B$TyExpression "x.case0.expr.f.f") (B$TyDataConstructor "Cons")
     , TypeEquals (B$TyExpression "x.case0.expr.f.a") (B$TyGlobal "hd")
     , TypeEquals (B$TyExpression "x.case0.expr.a") (B$TyGlobal "tl")
     ])


data TypeConstraint = TypeEquals (Type TyVar) (Type TyVar)
                    deriving (Show, Eq)

-- TODO merge all these cases?
data TyVar = TyGlobal Lowercase
           | TyDataConstructor Uppercase 
           | TyPattern Path
           | TyExpression Path
           | TyInferMe
           deriving (Show, Eq, Ord)

type Path = String

-- like Python enumerate: label each item with its index
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

cProgram :: (Program InferMe) -> Writer [TypeConstraint] ()
cProgram defs = forM_ defs $ \def ->
  case def of
   DefVal name ann cases -> cDefVal name ann cases
   DefData name variants -> cDefData name variants

---- Type a  =>  types plus leaves a
cDefData :: Uppercase -> [Variant] -> Writer [TypeConstraint] ()
cDefData name variants =  forM_ variants $ \(Variant vname fields) ->
  -- data D = Foo A B C  =>  Foo :: A -> (B -> (C -> D))
  let ty :: Type TyVar
      ty = foldr F (T name) (map tyKnown2 fields)
  in
  tell [TypeEquals (B$TyDataConstructor vname) ty]

tyKnown :: Type InferMe -> Type TyVar
tyKnown (B InferMe) = B TyInferMe
tyKnown (T name) = T name
tyKnown (F t r) = F (tyKnown t) (tyKnown r)

tyKnown2 :: Type Void -> Type TyVar
tyKnown2 (B nope) = absurd nope
tyKnown2 (T name) = T name
tyKnown2 (F t r) = F (tyKnown2 t) (tyKnown2 r)

cDefVal :: Lowercase -> (Type InferMe) -> [Case] -> Writer [TypeConstraint] ()
cDefVal name ann cases = do
  cDefValAnn name ann
  forM_ (enumerate cases) $ \(idx, case_) ->
    let prefix = name ++ ".case" ++ show idx
    in cDefValCase name prefix case_

cDefValAnn :: Lowercase -> (Type InferMe) -> Writer [TypeConstraint] ()
cDefValAnn name ann = tell [TypeEquals (B$TyGlobal name) (tyKnown ann)]

cDefValCase :: Lowercase -> Path -> Case -> Writer [TypeConstraint] ()
cDefValCase name prefix (Case pats expr) = do
  -- generate internal constraints on the patterns
  -- (and also an environment from local names to Ty variables)
  let patNames = flip map (enumerate pats) $ \(idx, _) -> prefix ++ ".pat" ++ show idx
  let exprName = prefix ++ ".expr"
  -- global = pat -> ... -> expr
  let funTy = foldr F (B$TyExpression exprName) (map (B . TyPattern) patNames)
  tell [TypeEquals (B$TyGlobal name) funTy]
  -- recur pats
  let constraints :: [TypeConstraint]
      env :: Map Lowercase (Type TyVar)
      (constraints, env) = execWriter (cPats patNames pats)
  tell constraints
  -- recur expr
  cExpr env exprName expr

cPats :: [Path] -> [Pattern] -> Writer ([TypeConstraint], Map Lowercase (Type TyVar)) ()
cPats prefixes pats = do
  forM_ (zip prefixes pats) $ \(prefix', pat) -> cPat prefix' pat

cPat :: Path -> Pattern -> Writer ([TypeConstraint], Map Lowercase (Type TyVar)) ()
cPat prefix (Hole x) = let mapping = (x, (B$TyPattern prefix))
                       in tellEnv (Map.fromList [mapping])
cPat prefix (Constructor c args) = do
  -- the constructor is a function type that matches the argument types
  let argNames = flip map (enumerate args) $ \(idx, _) -> prefix ++ ".arg" ++ show idx
  let funty = foldr F (B$TyPattern prefix) (map (B . TyPattern) argNames)
  tellConstraints [ TypeEquals (B$TyDataConstructor c) funty ]
  -- recur into each arg pattern
  forM_ (zip argNames args) $ \(prefix', arg) ->
    cPat prefix' arg


tellEnv :: Map Lowercase (Type TyVar) -> Writer ([TypeConstraint], Map Lowercase (Type TyVar)) ()
tellEnv x = tell (mempty, x)

tellConstraints :: [TypeConstraint] -> Writer ([TypeConstraint], Map Lowercase (Type TyVar)) ()
tellConstraints xs = tell (xs, mempty)
  
  
cExpr :: (Map Lowercase (Type TyVar)) -> Path -> Expr -> Writer [TypeConstraint] ()
cExpr env prefix (Var x) = case Map.lookup x env of
  Nothing -> tell [TypeEquals (B$TyExpression prefix) (B$TyGlobal x)]
  Just t -> tell [TypeEquals (B$TyExpression prefix) t]
cExpr _env prefix (Cst c) = tell [TypeEquals (B$TyExpression prefix) (B$TyDataConstructor c)]
cExpr env prefix (App f a) = do
  let prefixF = prefix ++ ".f"
  let prefixA = prefix ++ ".a"
  tell [ TypeEquals
         -- f :: a -> result
         (B$TyExpression prefixF)
         (F (B$TyExpression prefixA) (B$TyExpression prefix))]
  cExpr env prefixF f
  cExpr env prefixA a


{-

Constraint solver

TODO:
- DONE go back and rephrase Ty in terms of explicit "type variables".
.   - do this by opening a parameter in Type: (Type Void) is concrete types; (Type TyVar) is solver types.

- define a type-substitution as Map TyVar Type

-}


newtype TypeError = TypeError String
                  deriving (Eq, Show)

type TySubst = Map TyVar (Type TyVar)

solveTypeConstraints :: [TypeConstraint] -> Either TypeError TySubst
solveTypeConstraints = undefined




testSolveTypeConstraints :: Spec
testSolveTypeConstraints = do
  it "no constraints => win" $ do
    solveTypeConstraints [] `shouldBe` Right Map.empty
  it "var == constant => win" $ do
    solveTypeConstraints [TypeEquals (B (TyGlobal "x")) (T "Potato")]
    `shouldBe` Right (Map.fromList [ (TyGlobal "x", T "Potato") ])
  it "mismatched types => lose" $ do
    solveTypeConstraints [TypeEquals (T "A") (T "B")] `shouldBe` Left (TypeError "cannot unify A with B")
