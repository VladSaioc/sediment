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

import Semantics.Conf.Eval
import Semantics.Conf.TypeS

import Semantics.Exp.Eval
import Semantics.Exp.General
import Semantics.Exp.TypeS

bindDataT :: DomEnv -> TagTable -> [Df] -> Err TEnv
bindDataT de tt = Prelude.foldl (bindDataT' de tt) (Ok Data.Map.empty)

bindDataT' :: DomEnv -> TagTable -> Err TEnv -> Df -> Err TEnv
bindDataT' de tt (Bad msg) _ = Bad msg
bindDataT' de tt (Ok env) (Df pos df) = case df of
  DataDf c e -> expT de tt env e >>= confT de tt env c
  DataRecDf d x x' e -> case rootDomain de d of
    Dom _ (FuncDom d1 d2) -> do
      let env'' = Data.Map.insert x d env
      let env' = Data.Map.insert x' d1 env''
      d' <- expT de tt env' e
      if deq de d2 d' then Ok env'
      else errMsg pos ("Incompatible recursive data: " ++ x ++ " declared with output type " ++ show d2 ++ " but found " ++ show d' ++ " instead.")
    _ -> errMsg pos ("Invalid letrec definition: expected recursive data to be expressed as a function type, but was instead was declared with domain " ++ show d ++ ".")
  _  -> Ok env

bindData :: [Df] -> Err Env
bindData = Prelude.foldl bindData' (Ok Data.Map.empty)

bindData' :: Err Env -> Df -> Err Env
bindData' (Bad msg) _ = Bad msg
bindData' (Ok env) (Df pos df) = case df of
  DataDf c e -> expEval env e >>= confEval env c
  DataRecDf _ x x' e -> let
      env' = Data.Map.insert x (RCloj env x x' e) env
    in Ok env'
  _ -> Ok env