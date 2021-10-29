{-
 -  HOPL/LETREC/Syntax.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides the abstract syntax representation for LET.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LETREC.Lang.Syntax where

import HOPL.Types (Id)

newtype Pgm
  = Pgm Exp
  deriving (Eq, Ord, Show)

-- For each non-terminal appearing on the right-hand side of a production
-- we include a parameter type for the corresponding data constructor.
data Exp
  -- Variable reference
  = VarExp Id
  -- Integer literal
  | ConstExp Integer
  -- Boolean literals
  | TrueExp
  | FalseExp
  -- Arithmetic operators
  | IsZeroExp Exp
  | IsPosExp Exp
  | IsNegExp Exp
  -- Arithmetic/numeric predicates
  | DiffExp Exp Exp
  | SumExp Exp Exp
  | ProdExp Exp Exp
  | DivExp Exp Exp
  | ModExp Exp Exp
  | MinusExp Exp
  -- Relational operators
  | IsEqualExp Exp Exp
  | IsNotEqualExp Exp Exp
  | IsLessExp Exp Exp
  | IsGreaterExp Exp Exp
  | IsLessEqExp Exp Exp
  | IsGreaterEqExp Exp Exp
  -- Logical operators
  | AndExp Exp Exp
  | OrExp Exp Exp
  | NotExp Exp
  -- List constructors
  | EmptyExp
  | ListConsExp Exp Exp
  | ListExp [Exp]
  -- List observers
  | IsNullExp Exp
  | CarExp Exp
  | CdrExp Exp
  -- Variable declarations
  | LetExp Id Exp Exp
  | LetrecExp Id Id Exp Exp
  | UnpackExp [Id] Exp Exp
  -- Control expressions
  | IfExp Exp Exp Exp
  -- Function definition
  | ProcExp Id Exp
  -- Function call
  | CallExp Exp Exp
  deriving (Eq, Ord, Show)
