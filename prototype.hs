{-# OPTIONS_GHC -Wall #-}

module Prototype (testAll, main) where

import Test.Hspec
import Web.Scotty

import Model
import Parser
import Print
import TypeCheck
import WellFormedTypes
import CaseCoverage
import Termination
import Eval

import Text.Parsec (many)
import Data.Either (partitionEithers)
import System.IO
import System.Environment
import Data.String (fromString)
import Data.ByteString.Lazy.Char8 (unpack)

testAll :: IO ()
testAll = hspec $ do
  context "testModel" testModel
  context "testParse" testParse
  context "testPrint" testPrint
  context "testTypeCheck" testTypeCheck
  context "testWellFormedTypes" testWellFormedTypes
  context "testCaseCoverage" testCaseCoverage
  context "testTermination" testTermination
  context "testEval" testEval


main :: IO ()
main = do
  args <- getArgs
  case args of
   ["--serve"] -> runServer
   [filename] -> runFile filename
   _ -> error ("Usage: runghc prototype.hs <filename>; not " ++ show args)

runServer :: IO ()
runServer = do
  scotty 3000 $ do
    get (fromString "/") $ do
      setHeader (fromString "Content-Type") (fromString "text/html")
      file (fromString "ui/notebook.html")
    get (fromString "/codemirror.css") $ file (fromString "ui/vendor/codemirror.css")
    get (fromString "/codemirror.js") $ file (fromString "ui/vendor/codemirror.js")

    post (fromString "/eval") $ do
      contents <- unpack <$> body
      case evalProgram "<request>" contents of
       Left errmsg -> text (fromString errmsg)
       Right values -> text (fromString (unlines (map printExpr values)))

runContents :: String -> String -> IO ()
runContents filename contents = do
  --mapM_ print defsAndExprs
  --putStrLn ""
  case evalProgram filename contents of
   Left errmsg -> error errmsg
   Right values -> mapM_ (putStrLn . printExpr) values


evalProgram :: String -> String -> Either String [Expr]
evalProgram filename contents = do
  defsAndExprs <- case iParse (many pDefOrExpr) filename contents of
                   Left err -> Left (show err)
                   Right parsed -> Right parsed
  let (defs, exprs) = partitionEithers defsAndExprs
  check defs TypeCheck.checkProgram TypeCheck.explain
  check defs WellFormedTypes.checkProgram WellFormedTypes.explain
  check defs CaseCoverage.checkProgram CaseCoverage.explain
  check defs Termination.checkProgram show
  Right (map (fullEval defs) exprs)


runFile :: String -> IO ()
runFile filename = do
  withFile filename ReadMode $ \fd -> do
    contents <- hGetContents fd
    runContents filename contents

check :: Program -> (Program -> Either err ()) -> (err -> String) -> Either String ()
check prog checker explainer = case checker prog of
  Right () -> Right ()
  Left err -> Left (explainer err)

  
