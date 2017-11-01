{-# OPTIONS_GHC -Wall #-}

module Prototype (testAll) where

import Test.Hspec

import Model
import Parser
import TypeCheck
import WellFormedTypes
import CaseCoverage
import Termination
import Eval

import Text.Parsec (many)
import Data.Either (partitionEithers)
import Control.Monad (forM_)
import System.IO
import System.Environment

testAll :: IO ()
testAll = hspec $ do
  context "testModel" testModel
  context "testParse" testParse
  context "testTypeCheck" testTypeCheck
  context "testWellFormedTypes" testWellFormedTypes
  context "testCaseCoverage" testCaseCoverage
  context "testTermination" testTermination
  context "testEval" testEval


main :: IO ()
main = do
  args <- getArgs
  let filename = case args of
                  [fn] -> fn
                  _ -> error ("Usage: runghc prototype.hs <filename>; not " ++ show args)
  withFile filename ReadMode $ \fd -> do
    contents <- hGetContents fd
    let defsAndExprs :: [Either Def Expr]
        defsAndExprs = case iParse (many pDefOrExpr) filename contents of
                        Left err -> error (show err)
                        Right parsed -> parsed
    let (defs, exprs) = partitionEithers defsAndExprs
    check defs TypeCheck.checkProgram TypeCheck.explain
    check defs WellFormedTypes.checkProgram WellFormedTypes.explain
    check defs CaseCoverage.checkProgram CaseCoverage.explain
    check defs Termination.checkProgram show
    forM_ exprs $ \expr ->
      print (fullEval defs expr)

check :: Program -> (Program -> Either err ()) -> (err -> String) -> IO ()
check prog checkProgram explain = case checkProgram prog of
  Right () -> return ()
  Left err -> error (explain err)

  
