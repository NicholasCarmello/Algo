{-
 -  MutablePairsSpec.hs
 -
 -  Reference implementation of the toy language MUTABLE_PAIRS from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides a test suite for the MUTABLE_PAIRS interpreter.
 -
 -  Author: Matthew A Johnson
 -}
module Algo (spec) where

import Data.Either (fromRight)
import HOPL.Algo.DataStructures (ExpVal (..))
import HOPL.Algo.Environment (Env (emptyEnv, extendEnv'))
import HOPL.Algo.Interp (interpWith)
import HOPL.Algo.Store (emptyStore)
import Test.Tasty.Hspec
import GHC.IO.Device (IODeviceType(Stream))

testStore = [NumVal 1, NumVal 5, NumVal 10]

testEnv =
  extendEnv'
    ["i", "v", "x"]
    [0, 1, 2]
    emptyEnv

spec =
  describe "Algo Tests" $ do
    describe "Value tests" $ do
      
      specify "equals-test" $
        interp "==(5,5)" `shouldBe` BoolVal True 
      specify "And-test" $
        interp "&&(false,false)" `shouldBe` BoolVal False 
      specify "or-test" $
        interp "||(true,false)" `shouldBe` BoolVal True
      specify "greater-than-test" $
        interp ">(5,4)" `shouldBe` BoolVal True

    describe "Error tests" $ do
      specify "test-unbound-var-1" $
        printInterp "foo" `shouldThrow` anyErrorCall
      specify "test-unbound-var-1" $
        printInterp "-(x,foo)" `shouldThrow` anyErrorCall
  where
    interp = fromRight undefined . interpWith testEnv testStore
    printInterp = print . interpWith testEnv testStore
