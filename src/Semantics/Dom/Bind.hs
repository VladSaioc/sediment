module Semantics.Dom.Bind(
  bindDom,
  makeTagTable
) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Dom.General

bindDom :: Spec -> Err (DomEnv, [PosVar], [PosVar])
bindDom = \case
  Spec dfs _ -> Prelude.foldl bindDom' (Ok (empty, [], [])) dfs
  SpecError msg -> Bad msg

bindDom' :: Err (DomEnv, [PosVar], [PosVar]) -> Df -> Err (DomEnv, [PosVar], [PosVar])
bindDom' = \case
  Ok (de, x1s, x2s) -> \df@(Df pos df') -> case df' of
    DomDf x d@(Dom _ (UnionDom ds stx)) -> case Data.Map.lookup x de of
      Nothing -> Ok (insert x d de, x1s, (x, pos) : x2s)
      _ -> if stx then errMsg pos ("Name conflict: abstract syntax " ++ x ++ " shares a name with another abstract syntax or domain.")
        else errMsg pos ("Name conflict: domain " ++ x ++ " shares a name with another domain or abstract syntax.")
    DomDf x d -> case Data.Map.lookup x de of
      Nothing -> Ok (insert x d de, (x, pos) : x1s, x2s)
      _ -> errMsg pos ("Name conflict: domain " ++ x ++ " shares a name with another domain or abstract syntax.")
    _ -> Ok (de, x1s, x2s)
  Bad msg -> \_ -> Bad msg

makeTagTable :: DomEnv -> [PosVar] -> TagTable
makeTagTable de [] = Data.Map.empty
makeTagTable de ((x, _) : xs) = let
    Just (Dom _ (UnionDom ds _)) = Data.Map.lookup x de
  in Data.Map.union (Prelude.foldl (\tab (t, d) -> Data.Map.insert t (x, d) tab) Data.Map.empty ds) (makeTagTable de xs)