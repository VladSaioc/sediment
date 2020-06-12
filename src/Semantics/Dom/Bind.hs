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
  Spec dfs _ -> Prelude.foldl bindDom' (Ok (empty, [], [])) dfs
  SpecError msg -> Bad msg

bindDom' :: Err (DomEnv, [String], [String]) -> Df -> Err (DomEnv, [String], [String])
bindDom' = \case
  Ok (de, x1s, x2s) -> \case
    DomDf x (UnionDom ds stx) -> case Data.Map.lookup x de of
      Nothing -> Ok (insert x (UnionDom ds stx) de, x1s, x : x2s)
      _ -> if stx then Bad ("Name conflict: abstract syntax " ++ x ++ " shares a name with another abstract syntax or a domain.")
        else Bad ("Name conflict: domain " ++ x ++ " shares a name with another domain or an abstract syntax.")
    DomDf x d -> case Data.Map.lookup x de of
      Nothing -> Ok (insert x d de, x : x1s, x2s)
      _ -> Bad ("Name conflict: domain " ++ x ++ " shares a name with another domain or an abstract syntax.")
    _ -> Ok (de, x1s, x2s)
  Bad msg -> \_ -> Bad msg

makeTagTable :: DomEnv -> [String] -> TagTable
makeTagTable de [] = Data.Map.empty
makeTagTable de (x : xs) = let
    Just (UnionDom ds _) = Data.Map.lookup x de
  in Data.Map.union (Prelude.foldl (\tab (t, d) -> Data.Map.insert t (x, d) tab) Data.Map.empty ds) (makeTagTable de xs)