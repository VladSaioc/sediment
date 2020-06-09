module Semantics.Exp.Bind (
  bindDataT,
  bindData
) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General

import Semantics.Dom.General
import Semantics.Dom.Equivalence

import Semantics.Exp.Eval
import Semantics.Exp.General
import Semantics.Exp.TypeS

bindDataT :: DomEnv -> TagTable -> [Df] -> Err TEnv
bindDataT de tt = Prelude.foldl (bindDataT' de tt) (Ok Data.Map.empty)

bindDataT' :: DomEnv -> TagTable -> Err TEnv -> Df -> Err TEnv
bindDataT' de tt (Bad msg) = \_ -> Bad msg
bindDataT' de tt (Ok env) = \case
  DataDf x e -> expT de tt env e >>= \d -> Ok (Data.Map.insert x d env)
  DataRecDf d x x' e -> case rootDomain de d of
    FuncDom d1 d2 -> do
      let env'' = Data.Map.insert x d env
      let env' = Data.Map.insert x' d1 env''
      d' <- expT de tt env' e
      if deq de d2 d' then Ok env'
      else Bad ("Incompatible recursive data: " ++ x ++ " declared with output type " ++ show d2 ++ " but found " ++ show d' ++ " instead.")
    _ -> Bad ("Invalid letrec definition: expected recursive data to be expressed as a function type, but was instead was declared with domain " ++ show d ++ ".")
  _  -> Ok env

bindData :: [Df] -> Err Env
bindData = Prelude.foldl bindData' (Ok Data.Map.empty)

bindData' :: Err Env -> Df -> Err Env
bindData' (Bad msg) = \_ -> Bad msg
bindData' (Ok env) = \case
  DataDf x e -> do
    v <- expEval env e
    Ok (Data.Map.insert x v env)
  DataRecDf _ x x' e -> let
      env' = Data.Map.insert x (RCloj env x x' e) env
    in Ok env'
  _ -> Ok env