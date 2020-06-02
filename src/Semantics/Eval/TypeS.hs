module Semantics.Eval.TypeS (evalT) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General

import Semantics.Dom.Equivalence
import Semantics.Dom.General

import Semantics.Exp.General
import Semantics.Exp.TypeS

import Semantics.TSys.General

evalT :: DomEnv -> TagTable -> TTSEnv -> TEnv -> Ev -> Err [()]
evalT de tt tenv env = \case
  Ev e1 e2 x -> case Data.Map.lookup x tenv of
    Nothing -> Bad ("Undeclared variable: transition system " ++ x ++ " not found in the scope.")
    Just (TDom d1 d2 _) -> case results [expT de tt env e1, expT de tt env e2] of
      Bad msg -> Bad msg
      Ok [d1', d2'] -> if deq de d1 d1' then
          if deq de d2 d2' then Ok []
          else Bad ("Incompatible types: expected " ++ show d2 ++ ", but found " ++ show d2' ++ " instead.")
        else Bad ("Incompatible types: expected " ++ show d1 ++ ", but found " ++ show d1' ++ " instead.")
  ExpEv e -> expT de tt env e >> Ok []