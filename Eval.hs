module Eval (
  testEval,
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Model

import Test.Hspec


testEval :: Spec
testEval = do
  it "test some rewrites at root" $ do
    let lengthProg = [
          DefVal "length" Nothing [
             Case [Constructor "Nil" []] (Cst "Zero"),
             Case [Constructor "Cons" [Hole "hd", Hole "tl"]] (App (Cst "Succ")
                                                               (App (Var "length")
                                                                (Var "tl")))
             ]
          ]
    -- no match
    rewriteProgram lengthProg (Var "x") `shouldBe` Nothing
    rewriteProgram lengthProg (Cst "Nil") `shouldBe` Nothing
    rewriteProgram lengthProg (Var "length") `shouldBe` Nothing
    rewriteProgram lengthProg (App (Var "somethingElse") (Cst "Nil"))
      `shouldBe` Nothing
    -- apply rule: length [] === 0
    rewriteProgram lengthProg (App (Var "length") (Cst "Nil"))
      `shouldBe` Just (Cst "Zero")
    -- apply rule: length x:xs === Succ (length xs)
    rewriteProgram lengthProg (App (Var "length") (App (App (Cst "Cons") (Var "x")) (Cst "Nil")))
      `shouldBe` Just (App (Cst "Succ") (App (Var "length") (Cst "Nil")))
    -- rewrites only happen at the root
    rewriteProgram lengthProg (App (Cst "Succ") (App (Var "length") (Cst "Nil")))
      `shouldBe` Nothing



-- Each equation in the program is a rewrite rule.
-- Applying a rewrite rule either fails, or succeeds with a new expression.
type Rewriter = Expr -> Maybe Expr

rewriteFail :: Rewriter
rewriteFail _ = Nothing

-- Applying a rewrite rule has 2 steps:
-- 1. try to match the pattern (or fail), yielding a substitution
-- 2. combine the substitution and template
type Matcher = Expr -> Maybe Subst
type Subst = Map Lowercase Expr

data RList a = Snoc (RList a) a
             | Nope
             deriving (Show, Eq)

rlistFromList :: [a] -> RList a
rlistFromList = foldl Snoc Nope

rewriteProgram :: Program -> Rewriter
rewriteProgram defs = orderedChoices (map rewriteDef defs)

rewriteDef :: Def -> Rewriter
rewriteDef (DefData{}) = rewriteFail
rewriteDef (DefVal name _ cases) = orderedChoices (map (rewriteCase name) cases)

rewriteCase :: Lowercase -> Case -> Rewriter
rewriteCase funcname (Case pats rhs) expr = do
  subst <- matchCall (Var funcname) (rlistFromList pats) expr
  return (applySubst subst rhs)


matchPattern :: Pattern -> Matcher
matchPattern (Hole name) expr = Just (Map.singleton name expr)
matchPattern (Constructor cname argpats) expr = matchCall (Cst cname) (rlistFromList argpats) expr

matchCall :: Expr -> RList Pattern -> Matcher
matchCall callee Nope e = if callee == e then Just Map.empty else Nothing
matchCall callee (Snoc args arglast) (App f a) = do
  substlastarg <- matchPattern arglast a
  substother <- matchCall callee args f
  return (Map.union substlastarg substother)
matchCall _ _ _ = Nothing

applySubst :: Subst -> Expr -> Expr
applySubst s (App f x) = App (applySubst s f) (applySubst s x)
applySubst _ (Cst c) = Cst c
applySubst s (Var x) = Map.findWithDefault (Var x) x s


orderedChoice :: Rewriter -> Rewriter -> Rewriter
orderedChoice rw0 rw1 expr = case rw0 expr of
  Just v -> Just v
  Nothing -> case rw1 expr of
    Just v -> Just v
    Nothing -> Nothing

orderedChoices :: [Rewriter] -> Rewriter
orderedChoices = foldr orderedChoice rewriteFail
