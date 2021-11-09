{-
 -  Spec.hs
 -
 -  This module runs test suites for all HOPL language implementations.
 -
 -  Author: Matthew A Johnson
 -}
module Main where

import qualified LetSpec as LET
import qualified ProcSpec as PROC
import qualified LetrecSpec as LETREC
import qualified ExplicitRefsSpec as EXPLICIT_REFS
import qualified ImplicitRefsSpec as IMPLICIT_REFS
import qualified MutablePairsSpec as MUTABLE_PAIRS
import Test.Tasty.Hspec (hspec)

main = do
  hspec LET.spec
  hspec PROC.spec
  hspec LETREC.spec
  hspec EXPLICIT_REFS.spec
  hspec IMPLICIT_REFS.spec
  hspec MUTABLE_PAIRS.spec
