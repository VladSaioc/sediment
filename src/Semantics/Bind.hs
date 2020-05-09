module Semantics.Bind(
  bindDom,
  bindTSys
) where

import Syntax.Ast
import Semantics.Env
import Data.Map

bindDom :: Spec -> (DomEnv, [String], [String])
bindDom (Spec dfs _) = Prelude.foldl bindDomInd (empty, [], []) dfs

bindDomInd :: (DomEnv, [String], [String]) -> Df -> (DomEnv, [String], [String])
bindDomInd (de, x1s, x2s) (DomDf i (UnionDom ts)) = (insert i (UnionDom ts) de, x1s, i : x2s)
bindDomInd (de, x1s, x2s) (DomDf i d) = (insert i d de, i : x1s, x2s)
bindDomInd (de, x1s, x2s) (DataDf i d) = (de, x1s, x2s)
bindDomInd (de, x1s, x2s) (DataRecDf dd i d) = (de, x1s, x2s)
bindDomInd (de, x1s, x2s) (TSysDf td i d) = (de, x1s, x2s)

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
