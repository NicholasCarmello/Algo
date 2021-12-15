{-
 -  Spec.hs
 -
 -  This module runs test suites for all HOPL language implementations.
 -
 -  Author: Matthew A Johnson
 -}
module Main where

import qualified CallByNeedSpec as CALL_BY_NEED
import qualified CallByReferenceSpec as CALL_BY_REFERENCE
import qualified CheckedSpec as CHECKED
import qualified ExplicitRefsSpec as EXPLICIT_REFS
import qualified ImplicitRefsSpec as IMPLICIT_REFS
import qualified InferredSpec as INFERRED
import qualified LetSpec as LET
import qualified LetrecSpec as LETREC
import qualified Algo as Algo
import qualified ProcSpec as PROC
import qualified SimpleStatementSpec as SIMPLE_STATEMENT
import Test.Tasty.Hspec (hspec)

main = do
  hspec Algo.spec
