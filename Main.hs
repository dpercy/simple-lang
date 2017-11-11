{-# OPTIONS_GHC -Wall #-}

module Main (testAll, main) where

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
      let progDone = (runProgramString "<request>" contents)
      text (fromString (printProgram progDone))

runFileContents :: String -> String -> IO ()
runFileContents filename contents = do
  --mapM_ print defsAndExprs
  --putStrLn ""
  let progDone = runProgramString filename contents
  putStrLn (printProgram progDone)

runProgramString :: String -> String -> Program
runProgramString filename contents = do
  let prog = case iParse pProgram filename contents of
              -- TODO do error recovery in the parser instead
              Left err -> [Error (show err)]
              Right parsed -> parsed
  runProgram prog

runProgram :: Program -> Program
runProgram prog = do
  let prog2 = TypeCheck.checkProgram prog
  let prog3 = WellFormedTypes.checkProgram prog2
  --check prog3 CaseCoverage.checkProgram CaseCoverage.explain
  --check prog3 Termination.checkProgram show
  evalProgram prog3


runFile :: String -> IO ()
runFile filename = do
  withFile filename ReadMode $ \fd -> do
    contents <- hGetContents fd
    runFileContents filename contents

check :: Program -> (Program -> Either err ()) -> (err -> String) -> Either String ()
check prog checker explainer = case checker prog of
  Right () -> Right ()
  Left err -> Left (explainer err)

  
