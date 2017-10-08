{-# OPTIONS_GHC -Wall #-}

module Prototype (testAll) where

import Parser
import TypeChecker

import Test.Hspec

testAll :: IO ()
testAll = hspec $ do
  context "testParse" testParse
  context "testGenConstraints" testGenConstraints
  context "testSolveTypeConstraints" testSolveTypeConstraints
  

