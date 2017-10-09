{-# OPTIONS_GHC -Wall #-}

module Prototype (testAll) where

import Test.Hspec

import Model
import Parser
import TypeCheck

testAll :: IO ()
testAll = hspec $ do
  context "testModel" testModel
  context "testParse" testParse
  context "testTypeCheck" testTypeCheck
