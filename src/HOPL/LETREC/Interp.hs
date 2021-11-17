{-
 -  HOPL/LETREC/Interp.hs
 -
 -  Reference implementation of the toy language LETREC from the
 -  EOPL3 textbook by Mitchell Wand.
 -
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
-- Variable reference
valueOf (VarExp var) ρ = applyEnv ρ var
-- Integer literal
valueOf (ConstExp n) _ = NumVal n
-- Boolean literals
valueOf TrueExp ρ = BoolVal True
valueOf FalseExp ρ = BoolVal False
-- Arithmetic/numeric predicates
valueOf (IsZeroExp exp₁) ρ = BoolVal (n == 0)
  where
    NumVal n = valueOf exp₁ ρ
valueOf (IsPosExp exp₁) ρ = BoolVal (n₁ > 0)
  where
    NumVal n₁ = valueOf exp₁ ρ
valueOf (IsNegExp exp₁) ρ = BoolVal (n₁ < 0)
  where
    NumVal n₁ = valueOf exp₁ ρ
-- Arithmetic operators
valueOf (DiffExp exp₁ exp₂) ρ = NumVal (n₁ - n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (SumExp exp₁ exp₂) ρ = NumVal (n₁ + n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (ProdExp exp₁ exp₂) ρ = NumVal (n₁ * n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (DivExp exp₁ exp₂) ρ = NumVal (n₁ `div` n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (ModExp exp₁ exp₂) ρ = NumVal (n₁ `rem` n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (MinusExp exp₁) ρ = NumVal (- n₁)
  where
    NumVal n₁ = valueOf exp₁ ρ
-- Relational operators
valueOf (IsEqualExp exp₁ exp₂) ρ = BoolVal (n₁ == n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (IsNotEqualExp exp₁ exp₂) ρ = BoolVal (n₁ /= n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (IsLessExp exp₁ exp₂) ρ = BoolVal (n₁ < n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (IsGreaterExp exp₁ exp₂) ρ = BoolVal (n₁ > n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (IsLessEqExp exp₁ exp₂) ρ = BoolVal (n₁ <= n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (IsGreaterEqExp exp₁ exp₂) ρ = BoolVal (n₁ >= n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
-- Logical operators
valueOf (AndExp exp₁ exp₂) ρ = BoolVal (q₁ && q₂)
  where
    BoolVal q₁ = valueOf exp₁ ρ
    BoolVal q₂ = valueOf exp₂ ρ
valueOf (OrExp exp₁ exp₂) ρ = BoolVal (q₁ || q₂)
  where
    BoolVal q₁ = valueOf exp₁ ρ
    BoolVal q₂ = valueOf exp₂ ρ
valueOf (NotExp exp₁) ρ = BoolVal (not q₁)
  where
    BoolVal q₁ = valueOf exp₁ ρ
-- List constructors
valueOf EmptyExp ρ = ListVal []
valueOf (ListConsExp exp₁ exp₂) ρ = ListVal (v : vs)
  where
    v = valueOf exp₁ ρ
    ListVal vs = valueOf exp₂ ρ
valueOf (ListExp exps) ρ = ListVal vs
  where
    vs = map (`valueOf` ρ) exps
-- List observers
valueOf (IsNullExp exp₁) ρ = BoolVal (null vs)
  where
    ListVal vs = valueOf exp₁ ρ
valueOf (CarExp exp₁) ρ = v
  where
    ListVal (v : _) = valueOf exp₁ ρ
valueOf (CdrExp exp₁) ρ = ListVal vs
  where
    ListVal (_ : vs) = valueOf exp₁ ρ
-- Variable declarations
valueOf (LetExp var rhs body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv var v ρ
    v = valueOf rhs ρ
valueOf (LetrecExp pname param pbody body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv pname (ProcVal (OpenProcedure param pbody)) ρ
valueOf (UnpackExp vars rhs body) ρ = undefined -- TODO implement semantics for unpack
-- Control expressions
valueOf (IfExp exp₁ exp₂ exp₃) ρ = valueOf exp' ρ
  where
    exp' = case valueOf exp₁ ρ of
      BoolVal True -> exp₂
      BoolVal False -> exp₃
-- Function definition
valueOf (ProcExp param body) ρ = ProcVal (ClosedProcedure param body ρ)
-- Function call
valueOf (CallExp rator rand) ρ = applyProcedure f arg
  where
    arg = valueOf rand ρ
    f = expvalToProc (valueOf rator ρ)

{- Auxiliary function for applying procedure values -}
applyProcedure :: Procedure -> DenVal -> ExpVal
applyProcedure (ClosedProcedure x body ρ) arg = valueOf body (extendEnv x arg ρ)
applyProcedure _ _ = undefined
