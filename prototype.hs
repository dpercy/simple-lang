{-# OPTIONS_GHC -Wall #-}

module Prototype (testAll) where

import Text.Parsec hiding (State, token)
import Text.Parsec.Indent
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Test.Hspec

testAll :: IO ()
testAll = hspec $ do
  context "testParse" testParse
  context "testGenConstraints" testGenConstraints
  

-- data model
type Program = [Def]

-- TODO type parameters
data Def = DefVal Lowercase Type [Case]
         | DefData Uppercase [Variant]
         deriving (Show, Eq)

data Case = Case [Pattern] Expr
          deriving (Show, Eq)

data Variant = Variant Uppercase [Type]
             deriving (Show, Eq)

data Type = B -- blank
          | T Uppercase -- named type
          | F Type Type -- function type
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



-- parsing
type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser name input =
    runIndent name (runParserT (spaces >> aParser) () name input)

{-
Make sure to follow the "lexeme" convention:
- every token (terminal in the grammar) starts with 'tok'
- every token checks indentation
- every token consumes spaces after it

This ensures spaces are handled efficiently,
and indentation is checked in all cases.

`token` is a helper function that turns any parsec parser
into a "tok*" parser that follows this convention.
-}
token :: IParser a -> IParser a
token p = do
  sameOrIndented
  v <- p
  spaces
  return v

tokLower :: IParser Lowercase
tokLower = token ((:) <$> lower <*> many alphaNum)

tokUpper :: IParser Uppercase
tokUpper = token ((:) <$> upper <*> many alphaNum)

reservedWords :: [String]
reservedWords = [ "data", "type", "case", "match" ]

pVar :: IParser Lowercase
pVar = try $ do v <- tokLower
                guard (not (v `elem` reservedWords))
                return v

pWord :: String -> IParser ()
pWord word = try $ do v <- tokLower
                      void $ guard (v == word)

kwData :: IParser ()
kwData = pWord "data"


pParens :: IParser a -> IParser a
pParens parser = (do void $ token (char '(')
                     e <- parser
                     void $ token (char ')')
                     return e)


pPattern :: IParser Pattern
pPattern = (Hole <$> pVar)
           <|> pParens (Constructor <$> tokUpper <*> many pPattern)
           <|> Constructor <$> tokUpper <*> pure []


pExpr :: IParser Expr
pExpr = chainl1 pFactor (return App)
  where pFactor :: IParser Expr
        pFactor = pParens pExpr
                  <|> (Var <$> pVar)
                  <|> (Cst <$> tokUpper)


pDefVal :: IParser Def
pDefVal = do
  name <- lookAhead pVar
  typ <- try (pTypeDecl name) <|> return B
  cases <- many1 (pCase name)
  return $ DefVal name typ cases

pTypeDecl :: String -> IParser Type
pTypeDecl name = withPos $ do
  pWord name
  void $ token (string "::")
  pType

pType :: IParser Type
pType = chainl1 pFactor (token (string "->") >> return F)
  where pFactor :: IParser Type
        pFactor = pParens pType
                  <|> (T <$> tokUpper)
        

pCase :: String -> IParser Case
pCase name = withPos $ do
  pWord name
  pats <- many pPattern
  void $ token (char '=')
  expr <- pExpr
  return $ Case pats expr


pDefData :: IParser Def
pDefData = withPos $ do
  kwData
  typeName <- tokUpper
  void $ token (char '=')
  variants <- pVariant `sepBy` token (char '|')
  return $ DefData typeName variants
    where pVariant :: IParser Variant
          pVariant = Variant <$> tokUpper <*> many pType


pDef :: IParser Def
pDef = pDefData <|> pDefVal


pProgram :: IParser Program
pProgram = many pDef


testParse :: Spec
testParse = do
  let prettyLines s = (intercalate "\t" (map show $ lines s))
  describe "pExpr" $ do
    let means s e =
          it s $ do
            iParse pExpr "example" s `shouldBe` (Right e)
    "x" `means` (Var "x")
    "f x" `means` (App (Var "f") (Var "x"))
    "f x y" `means` (App (App (Var "f") (Var "x")) (Var "y"))
    "f x y" `means` (App (App (Var "f") (Var "x")) (Var "y"))
    "f (x y)" `means` (App (Var "f") (App (Var "x") (Var "y")))
  describe "pProgram" $ do
    let means s prog =
          it (prettyLines s) $ do
            iParse pProgram "example" s `shouldBe` (Right prog)
    "x = y" `means` [DefVal "x" B [Case [] (Var "y")]]
    "x = y\nx = z" `means` [DefVal "x" B [Case [] (Var "y"), Case [] (Var "z")]]
    "x = y\na = z" `means` [DefVal "x" B [Case [] (Var "y")], DefVal "a" B [Case [] (Var "z")]]
    -- Note this program is invalid because x is defined twice.
    "x = y\na = z\nx = q" `means` [ DefVal "x" B [ Case [] (Var "y")]
                                  , DefVal "a" B [ Case [] (Var "z")]
                                  , DefVal "x" B [ Case [] (Var "q")] ]

    "f x = x" `means` [ DefVal "f" B [ Case [Hole "x"] (Var "x") ]]
    "f (S x) = x" `means` [ DefVal "f" B [ Case [Constructor "S" [Hole "x"]] (Var "x") ]]
    "f (Cons hd tl) = x" `means` [ DefVal "f" B [ Case [Constructor "Cons" [ Hole "hd"
                                                                         , Hole "tl"]]
                                                (Var "x") ]]
    "data Nat = O | S Nat" `means` [ DefData "Nat" [ Variant "O" []
                                                   , Variant "S" [T "Nat"]]]
    "data NatList = Nil | Cons Nat NatList" `means` [ DefData "NatList"
                                                      [ Variant "Nil" []
                                                      , Variant "Cons" [T "Nat", T "NatList"]]]
    "f :: Int\nf = x" `means` [ DefVal "f" (T "Int") [ Case [] (Var "x") ] ]
    "f :: A -> B\nf = x" `means` [ DefVal "f" (F (T "A") (T "B")) [ Case [] (Var "x") ] ]




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
     [ TypeEquals (TyDataConstructor "Empty") (TyKnown (T "NatList"))
     , TypeEquals (TyDataConstructor "Cons") (TyKnown (F (T "Nat")
                                                       (F (T "NatList")
                                                        (T "NatList")))) ])
  it "simple value definition" $ do
    (execWriter (cProgram [DefVal "x" B [Case [] (Cst "Potato")]])
     `shouldBe`
     [ TypeEquals (TyGlobal "x") (TyKnown B)
     , TypeEquals (TyGlobal "x") (TyExpression "x.case0.expr")
     , TypeEquals (TyExpression "x.case0.expr") (TyDataConstructor "Potato")
     ])

  it "1-arg function" $ do
    (execWriter (cProgram [DefVal "f" B [ Case [Hole "x"] (Var "x")
                                        , Case [Hole "x"] (Var "y")]])
     `shouldBe`
     [ TypeEquals (TyGlobal "f") (TyKnown B)
     , TypeEquals (TyGlobal "f") (TyFunc (TyPattern "f.case0.pat0") (TyExpression "f.case0.expr"))
     , TypeEquals (TyExpression "f.case0.expr") (TyPattern "f.case0.pat0")
     , TypeEquals (TyGlobal "f") (TyFunc (TyPattern "f.case1.pat0") (TyExpression "f.case1.expr"))
     , TypeEquals (TyExpression "f.case1.expr") (TyGlobal "y")
     ])
  it "destructuring in a function" $ do
    (execWriter (cProgram [DefVal "f" B [ Case [Constructor "X" []]
                                          (Var "x")
                                        , Case [Constructor "Cons" [Hole "a", Hole "b"]]
                                          (Var "b")]])
     `shouldBe`
     [ TypeEquals (TyGlobal "f") (TyKnown B)
       -- case0
     , TypeEquals (TyGlobal "f") (TyFunc (TyPattern "f.case0.pat0")
                                  (TyExpression "f.case0.expr"))
       -- case0 pat0
     , TypeEquals (TyDataConstructor "X") (TyPattern "f.case0.pat0")
       -- case0 expr
     , TypeEquals (TyExpression "f.case0.expr") (TyGlobal "x")
       -- case1
     , TypeEquals (TyGlobal "f") (TyFunc (TyPattern "f.case1.pat0")
                                  (TyExpression "f.case1.expr"))
       -- case1 pat0
     , TypeEquals (TyDataConstructor "Cons")
       (TyFunc (TyPattern "f.case1.pat0.arg0")
                   (TyFunc (TyPattern "f.case1.pat0.arg1")
                    (TyPattern "f.case1.pat0")))
       -- case1 expr
     , TypeEquals (TyExpression "f.case1.expr") (TyPattern "f.case1.pat0.arg1")
     ])
  it "interesting expressions" $ do
    (execWriter (cProgram [DefVal "x" B [ Case [] (App (App (Cst "Cons") (Var "hd")) (Var "tl"))]])
     `shouldBe`
     [ TypeEquals (TyGlobal "x") (TyKnown B)
     , TypeEquals (TyGlobal "x") (TyExpression "x.case0.expr")
     , TypeEquals (TyExpression "x.case0.expr.f")
       (TyFunc (TyExpression "x.case0.expr.a")
        (TyExpression "x.case0.expr"))
     , TypeEquals (TyExpression "x.case0.expr.f.f")
       (TyFunc (TyExpression "x.case0.expr.f.a")
        (TyExpression "x.case0.expr.f"))
     , TypeEquals (TyExpression "x.case0.expr.f.f") (TyDataConstructor "Cons")
     , TypeEquals (TyExpression "x.case0.expr.f.a") (TyGlobal "hd")
     , TypeEquals (TyExpression "x.case0.expr.a") (TyGlobal "tl")
     ])


data TypeConstraint = TypeEquals Ty Ty
                    deriving (Show, Eq)

data Ty = TyKnown Type
        | TyGlobal Lowercase
        | TyDataConstructor Uppercase 
        | TyPattern Path
        | TyExpression Path
        | TyFunc Ty Ty
        deriving (Show, Eq)

type Path = String

-- like Python enumerate: label each item with its index
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

cProgram :: Program -> Writer [TypeConstraint] ()
cProgram defs = forM_ defs $ \def ->
  case def of
   DefVal name ann cases -> cDefVal name ann cases
   DefData name variants -> cDefData name variants

---- Type a  =>  types plus leaves a
cDefData :: Uppercase -> [Variant] -> Writer [TypeConstraint] ()
cDefData name variants =  forM_ variants $ \(Variant vname fields) ->
  -- data D = Foo A B C  =>  Foo :: A -> (B -> (C -> D))
  let ty :: Type
      ty = foldr F (T name) fields
  in
  tell [TypeEquals (TyDataConstructor vname) (TyKnown ty)]

cDefVal :: Lowercase -> Type -> [Case] -> Writer [TypeConstraint] ()
cDefVal name ann cases = do
  cDefValAnn name ann
  forM_ (enumerate cases) $ \(idx, case_) ->
    let prefix = name ++ ".case" ++ show idx
    in cDefValCase name prefix case_

cDefValAnn :: Lowercase -> Type -> Writer [TypeConstraint] ()
cDefValAnn name ann = tell [TypeEquals (TyGlobal name) (TyKnown ann)]

cDefValCase :: Lowercase -> Path -> Case -> Writer [TypeConstraint] ()
cDefValCase name prefix (Case pats expr) = do
  -- generate internal constraints on the patterns
  -- (and also an environment from local names to Ty variables)
  let patNames = flip map (enumerate pats) $ \(idx, _) -> prefix ++ ".pat" ++ show idx
  let exprName = prefix ++ ".expr"
  -- global = pat -> ... -> expr
  let funTy = foldr TyFunc (TyExpression exprName) (map TyPattern patNames)
  tell [TypeEquals (TyGlobal name) funTy]
  -- recur pats
  let constraints :: [TypeConstraint]
      env :: Map Lowercase Ty
      (constraints, env) = execWriter (cPats patNames pats)
  tell constraints
  -- recur expr
  cExpr env exprName expr

cPats :: [Path] -> [Pattern] -> Writer ([TypeConstraint], Map Lowercase Ty) ()
cPats prefixes pats = do
  forM_ (zip prefixes pats) $ \(prefix', pat) -> cPat prefix' pat

cPat :: Path -> Pattern -> Writer ([TypeConstraint], Map Lowercase Ty) ()
cPat prefix (Hole x) = let mapping = (x, (TyPattern prefix))
                       in tellEnv (Map.fromList [mapping])
cPat prefix (Constructor c args) = do
  -- the constructor is a function type that matches the argument types
  let argNames = flip map (enumerate args) $ \(idx, _) -> prefix ++ ".arg" ++ show idx
  let funty = foldr TyFunc (TyPattern prefix) (map TyPattern argNames)
  tellConstraints [ TypeEquals (TyDataConstructor c) funty ]
  -- recur into each arg pattern
  forM_ (zip argNames args) $ \(prefix', arg) ->
    cPat prefix' arg


tellEnv :: Map Lowercase Ty -> Writer ([TypeConstraint], Map Lowercase Ty) ()
tellEnv x = tell (mempty, x)

tellConstraints :: [TypeConstraint] -> Writer ([TypeConstraint], Map Lowercase Ty) ()
tellConstraints xs = tell (xs, mempty)
  
  
cExpr :: (Map Lowercase Ty) -> Path -> Expr -> Writer [TypeConstraint] ()
cExpr env prefix (Var x) = case Map.lookup x env of
  Nothing -> tell [TypeEquals (TyExpression prefix) (TyGlobal x)]
  Just t -> tell [TypeEquals (TyExpression prefix) t]
cExpr _env prefix (Cst c) = tell [TypeEquals (TyExpression prefix) (TyDataConstructor c)]
cExpr env prefix (App f a) = do
  let prefixF = prefix ++ ".f"
  let prefixA = prefix ++ ".a"
  tell [ TypeEquals
         -- f :: a -> result
         (TyExpression prefixF)
         (TyFunc (TyExpression prefixA) (TyExpression prefix))]
  cExpr env prefixF f
  cExpr env prefixA a


{-

Constraint solver

TODO:
- go back and rephrase Ty in terms of explicit "type variables".
.   - do this by opening a parameter in Type: (Type Void) is concrete types; (Type TyVar) is solver types.
- define a type-substitution as Map TyVar Type

-}


solveTypeConstraints :: [TypeConstraint] -> Either TypeError ()
solveTypeConstraints = undefined
