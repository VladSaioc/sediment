module Semantics.Domains.Wellformed.UniqueTags (
  uniqueTags
) where

import Data.Set
import Data.Map
import Syntax.Ast
import Semantics.Env

uniqueTags :: DomEnv -> [String] -> Maybe (Set String)
uniqueTags de = Prelude.foldl (uniqueTagsInd de) (Just Data.Set.empty)

uniqueTagsInd :: DomEnv -> Maybe (Set String) -> String -> Maybe (Set String)
uniqueTagsInd de Nothing x = Nothing
uniqueTagsInd de (Just s) x = let
  Just (UnionDom tds) = Data.Map.lookup x de
  ts = Data.Set.fromList (Prelude.map fst tds)
  in if disjoint ts s && Data.Set.size ts == length tds then Just (ts `Data.Set.union` s)
  else Nothing