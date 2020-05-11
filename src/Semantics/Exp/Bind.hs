module Semantics.Exp.Bind (bindDataT) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General

import Semantics.Dom.General
import Semantics.Dom.Equivalence

import Semantics.Exp.General
import Semantics.Exp.TypeS

bindDataT :: DomEnv -> TagTable -> [Df] -> Err TEnv
bindDataT de tt = Prelude.foldl (bindDataT' de tt) (Ok Data.Map.empty)

bindDataT' :: DomEnv -> TagTable -> Err TEnv -> Df -> Err TEnv
bindDataT' de tt renv DomDf{} = renv
bindDataT' de tt renv TSysDf{} = renv
bindDataT' de tt renv (DataDf x e) = case renv of
  Bad msg -> Bad msg
  Ok env -> case expT de tt env e of
    Bad msg -> Bad msg
    Ok d -> Ok (Data.Map.insert x d env)
bindData' de tt renv (DataRecDf d x e) = case renv of
  Bad msg -> Bad msg
  Ok env -> let env' = Data.Map.insert x d env
    in case expT de tt env' e of
      Bad msg -> Bad msg
      Ok d' -> if deq de (d, d') then Ok env'
        else Bad ("Incompatible recursive data: " ++ x ++ " declared with type " ++ show d ++ " but found " ++ show d' ++ " instead.")
