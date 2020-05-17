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
bindTSysT' env = \case
  (TSysDf td x _) -> Data.Map.insert x td env
  _ -> env

bindTSys :: [Df] -> TSEnv
bindTSys = Prelude.foldl bindTSys' Data.Map.empty

bindTSys' :: TSEnv -> Df -> TSEnv
bindTSys' env = \case
  TSysDf _ x tsys -> Data.Map.insert x tsys env
  _ -> env
