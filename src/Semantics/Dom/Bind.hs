module Semantics.Dom.Bind(
  bindDom,
  makeTagTable
) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.Dom.General

bindDom :: Spec -> Err (DomEnv, [String], [String])
bindDom = \case
  Spec dfs _ -> Ok (Prelude.foldl bindDom' (empty, [], []) dfs)
  SpecError msg -> Bad msg

bindDom' :: (DomEnv, [String], [String]) -> Df -> (DomEnv, [String], [String])
bindDom' (de, x1s, x2s) = \case
  DomDf i (UnionDom ds stx) -> (insert i (UnionDom ds stx) de, x1s, i : x2s)
  DomDf i d -> (insert i d de, i : x1s, x2s)
  DataDf{} -> (de, x1s, x2s)
  DataRecDf{} -> (de, x1s, x2s)
  TSysDf{} -> (de, x1s, x2s)

makeTagTable :: DomEnv -> [String] -> TagTable
makeTagTable de [] = Data.Map.empty
makeTagTable de (x : xs) = let
    Just (UnionDom ds _) = Data.Map.lookup x de
  in Data.Map.union (Prelude.foldl (\tab (t, d) -> Data.Map.insert t (x, d) tab) Data.Map.empty ds) (makeTagTable de xs)