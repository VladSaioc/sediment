module Semantics.TypeCheck (typeCheck) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General

import Semantics.Dom.General

import Semantics.Eval.TypeS

import Semantics.Exp.Bind
import Semantics.Exp.General
import Semantics.Exp.TypeS

import Semantics.TSys.Bind
import Semantics.TSys.General
import Semantics.TSys.TypeS

typeCheck :: DomEnv -> TagTable -> Spec -> Err [[()]]
typeCheck de tt (Spec dfs evs) = case bindDataT de tt dfs of
  Bad msg -> Bad msg
  Ok env -> let
      tenv = bindTSysT dfs
    in results (tcdf de tt tenv env dfs ++ tcev de tt tenv env evs)

tcdf :: DomEnv -> TagTable -> TTSEnv -> TEnv -> [Df] -> [Err [()]]
tcdf de tt tenv env = Prelude.map (tcdf' de tt tenv env)

tcdf' :: DomEnv -> TagTable -> TTSEnv -> TEnv -> Df -> Err [()]
tcdf' de tt tenv env (TSysDf td _ rs) = let
    tenv' = Data.Map.insert thisTSys td tenv
  in tsysT de tt tenv' env rs
tcdf' de tt tenv env DataDf{} = Ok []
tcdf' de tt tenv env DataRecDf{} = Ok []
tcdf' de tt tenv env DomDf{} = Ok []

tcev :: DomEnv -> TagTable -> TTSEnv -> TEnv -> [Ev] -> [Err [()]]
tcev de tt tenv env = Prelude.map (evalT de tt tenv env)