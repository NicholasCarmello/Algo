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
      specify "multiplication-test" $
        interp "*(5,4)" `shouldBe` NumVal 20
      specify "Division-test" $
        interp "/(20,5)" `shouldBe` NumVal 4
      specify "minus-test" $
        interp "-(5,4)" `shouldBe` NumVal 1
      specify "addition-test" $
        interp "+(5,4)" `shouldBe` NumVal 9
      specify "greater than or equal to" $
        interp ">=(10,10)" `shouldBe` BoolVal True
      specify "less-than-test" $
        interp "<(2,4)" `shouldBe` BoolVal True
      specify "less than or equal to" $
        interp "<=(5,10)" `shouldBe` BoolVal True

    describe "Error tests" $ do
      specify "test-unbound-var-1" $
        printInterp "foo" `shouldThrow` anyErrorCall
      specify "test-unbound-var-1" $
        printInterp "-(x,foo)" `shouldThrow` anyErrorCall
      
  where
    interp = fromRight undefined . interpWith testEnv testStore
    printInterp = print . interpWith testEnv testStore
