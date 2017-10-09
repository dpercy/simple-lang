{-# OPTIONS_GHC -Wall #-}

module Prototype (testAll) where

import Test.Hspec

import Parser
import TypeInference

testAll :: IO ()
testAll = hspec $ do
  context "testParse" testParse
  context "testTypeInference" testTypeInference
  

