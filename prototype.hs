{-# OPTIONS_GHC -Wall #-}

module Prototype (testAll) where

import Test.Hspec

import Parser

testAll :: IO ()
testAll = hspec $ do
  context "testParse" testParse
  

