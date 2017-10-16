{-# OPTIONS_GHC -Wall #-}

module Prototype (testAll) where

import Test.Hspec

import Model
import Parser
import TypeCheck
import WellFormedTypes
import CaseCoverage
import Termination

testAll :: IO ()
testAll = hspec $ do
  context "testModel" testModel
  context "testParse" testParse
  context "testTypeCheck" testTypeCheck
  context "testWellFormedTypes" testWellFormedTypes
  context "testCaseCoverage" testCaseCoverage
  context "testTermination" testTermination
