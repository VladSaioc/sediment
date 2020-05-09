module Semantics.Bind(
  bindTSys
) where

import Syntax.Ast
import Semantics.Env
import Data.Map

bindTSys :: Spec -> (TTSEnv, TSEnv)
bindTSys (Spec dfs _) = Prelude.foldl bindTSysInd (empty, empty) dfs

bindTSysInd :: (TTSEnv, TSEnv) -> Df -> (TTSEnv, TSEnv)
bindTSysInd (te, e) DomDf{} = (te, e)
bindTSysInd (te, e) DataDf{} = (te, e)
bindTSysInd (te, e) DataRecDf{} = (te, e)
bindTSysInd (te, e) (TSysDf td i t) = (insert i td te, insert i t e)

-- bindData :: DomEnv -> Spec -> (TEnv, Env)
-- bindData (Spec dfs _) = Prelude.foldl 

-- bindDataInd :: DomEnv -> (TEnv, Env) -> Spec ->  (TEnv, Env)
-- bindDataInd ()
