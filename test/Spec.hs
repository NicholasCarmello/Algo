{-
 -  Spec.hs
 -
 -  This module runs test suites for all HOPL language implementations.
 -
 -  Author: Matthew A Johnson
 -}
module Main where

import qualified LetSpec as LET
import Test.Tasty.Hspec (hspec)

main = do
  hspec LET.spec
