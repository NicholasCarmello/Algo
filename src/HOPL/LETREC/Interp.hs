{-
 -  HOPL/LETREC/Interp.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LETREC.Interp
  ( interp,
    interpWith,
    interpWith',
  )
where

import Data.Either (fromRight)
import HOPL.LETREC.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.LETREC.Environment (Env (..))
import HOPL.LETREC.Lang.Parser (ParseError, parseToplevel)
import HOPL.LETREC.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.Types (Source)
import Prelude hiding (exp)
import Numeric (Floating(expm1))

{- top-level interpreter routines -}

interp :: Source -> Either ParseError ExpVal
interp = interpWith emptyEnv

interpWith' :: Environment -> Source -> ExpVal
interpWith' ρ = fromRight undefined . interpWith ρ

interpWith :: Environment -> Source -> Either ParseError ExpVal
interpWith ρ src = flip valueOfProgram ρ <$> parseToplevel src

{- semantic reduction of a program -}

valueOfProgram :: Pgm -> Environment -> ExpVal
valueOfProgram (Pgm exp) ρ = valueOf exp ρ

{- semantic reductions for expressions -}

valueOf :: Exp -> Environment -> ExpVal
valueOf (ConstExp n) _ = NumVal n
valueOf (VarExp var) ρ = applyEnv ρ var
valueOf (IsZeroExp exp₁) ρ = BoolVal (n == 0)
  where
    NumVal n = valueOf exp₁ ρ
valueOf (DiffExp exp₁ exp₂) ρ = NumVal (n₁ - n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (LetExp var rhs body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv var v ρ
    v = valueOf rhs ρ
valueOf (IfExp exp₁ exp₂ exp₃) ρ = valueOf exp' ρ
  where
    exp' = case valueOf exp₁ ρ of
      BoolVal True -> exp₂
      BoolVal False -> exp₃
valueOf (ProcExp param body) ρ = ProcVal (ClosedProcedure param body ρ)
valueOf (CallExp rator rand) ρ = applyProcedure f arg
  where
    arg = valueOf rand ρ
    f = expvalToProc (valueOf rator ρ)
valueOf (LetrecExp pname param pbody body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv pname (ProcVal (OpenProcedure param pbody)) ρ
valueOf (EvenExp exp₁) ρ = BoolVal (n `mod` 2 == 0)
  where 
    NumVal n = valueOf exp₁ ρ 
valueOf (LetRecEvenExp exp₁) ρ = BoolVal (n `mod` 2 ==0)
  where
    NumVal n = valueOf exp₁ ρ

{- Auxiliary function for applying procedure values -}
applyProcedure :: Procedure -> DenVal -> ExpVal
applyProcedure (ClosedProcedure x body ρ) arg = valueOf body (extendEnv x arg ρ)
applyProcedure _ _ = undefined
