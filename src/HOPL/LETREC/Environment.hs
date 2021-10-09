{-
 -  HOPL/LETREC/Environment.hs
 -
 -  Reference implementation of the toy language LET by Mitchell Wand.
 -  This module provides an abstract data type for symbol-to-value mappings.
 -  We include several implementations for illustrative purposes, including
 -  a recursive data-structure, an association-list, and an efficient ribcage
 -  representation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LETREC.Environment (Env (..)) where

import HOPL.LETREC.DataStructures (Binding, DenVal, Environment (..), ExpVal (ProcVal), Procedure (..))
import HOPL.Types (Id)

{- Interface for an environment (symbol-to-value mapping) -}

class Env e where
  -- construct an emptyEnv environment
  emptyEnv :: e

  -- construct new environment from existing environment plus a new binding
  extendEnv :: Id -> DenVal -> e -> e

  -- extract from an environment the mapped value if search symbol is present
  applyEnv :: e -> Id -> DenVal

  -- construct new environment from existing environment plus new bindings
  extendEnv' :: [Id] -> [DenVal] -> e -> e

{- Implementation of environment interface using data structure representation -}

instance Env Environment where
  emptyEnv = EmptyEnvironment
  extendEnv = Environment
  applyEnv EmptyEnvironment name = nobinding name
  applyEnv ρ@(Environment name val ρ') name'
    | name' == name = expand ρ val
    | otherwise = applyEnv ρ' name'
    where
      expand ρ (ProcVal (OpenProcedure param body)) = ProcVal (ClosedProcedure param body ρ)
      expand _ v = v

  extendEnv' [] [] = id
  extendEnv' vars vals =
    extendEnv' (tail vars) (tail vals) . extendEnv (head vars) (head vals)

{- Auxiliary functions -}

nobinding :: Id -> a
nobinding = error . ("No binding found for \"" ++) . (++ "\"")
