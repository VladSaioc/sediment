module Semantics.Dom.Bind(
  bindDom
) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.Dom.General

bindDom :: Spec -> Err (DomEnv, [String], [String])
bindDom (Spec dfs _) = Ok (Prelude.foldl bindDom' (empty, [], []) dfs)
bindDom (SpecError msg) = Bad msg

bindDom' :: (DomEnv, [String], [String]) -> Df -> (DomEnv, [String], [String])
bindDom' (de, x1s, x2s) (DomDf i (UnionDom ts)) = (insert i (UnionDom ts) de, x1s, i : x2s)
bindDom' (de, x1s, x2s) (DomDf i d) = (insert i d de, i : x1s, x2s)
bindDom' (de, x1s, x2s) DataDf{} = (de, x1s, x2s)
bindDom' (de, x1s, x2s) DataRecDf{} = (de, x1s, x2s)
bindDom' (de, x1s, x2s) TSysDf{} = (de, x1s, x2s)