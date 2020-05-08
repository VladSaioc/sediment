module BindSystems(
  bindDom
) where

import AstSediment
import Environments
import Data.Map

bindDom :: Spec -> (DomEnv, [String], [String])
bindDom (Spec dfs evs)= Prelude.foldl bindDomInd (empty, [], []) dfs

bindDomInd :: (DomEnv, [String], [String]) -> Df -> (DomEnv, [String], [String])
bindDomInd (de, x1s, x2s) (DomDf i (UnionDom ts))  = (insert i (UnionDom ts) de, x1s, i : x2s)
bindDomInd (de, x1s, x2s) (DomDf i d)  = (insert i d de, i : x1s, x2s)
bindDomInd (de, x1s, x2s) (DataDf i d)  = (de, x1s, x2s)
bindDomInd (de, x1s, x2s) (DataRecDf dd i d)  = (de, x1s, x2s)
bindDomInd (de, x1s, x2s) (TSysDf td i d)  = (de, x1s, x2s)