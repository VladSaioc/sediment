module Semantics.TSys.Bind (
  bindTSys,
  bindTSysT
) where

import Data.Map

import Syntax.Ast

import Semantics.TSys.General

bindTSysT :: [Df] -> TTSEnv
bindTSysT = Prelude.foldl bindTSysT' Data.Map.empty

bindTSysT' :: TTSEnv -> Df -> TTSEnv
bindTSysT' env DomDf{} = env
bindTSysT' env DataDf{} = env
bindTSysT' env DataRecDf{} = env
bindTSysT' env (TSysDf td x _) = Data.Map.insert x td env

bindTSys :: [Df] -> TSEnv
bindTSys = Prelude.foldl bindTSys' Data.Map.empty

bindTSys' :: TSEnv -> Df -> TSEnv
bindTSys' env DomDf{} = env
bindTSys' env DataDf{} = env
bindTSys' env DataRecDf{} = env
bindTSys' env (TSysDf _ x tsys) = Data.Map.insert x tsys env
