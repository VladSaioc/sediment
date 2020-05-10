module Semantics.Dom.General where

import Syntax.Ast
import Semantics.Env
import Data.Map

-- Domain environment
type DomEnv = Map String Dom

isUnion :: Dom -> Bool
isUnion (UnionDom _) = True
isUnion d = False

hasTag :: String -> [(String, Dom)] -> Maybe Dom
hasTag t [] = Nothing
hasTag t ((t', d) : tds) = if t == t' then Just d
  else hasTag t tds

deepDomain :: DomEnv -> Dom -> Dom
deepDomain de (VarDom x) = case Data.Map.lookup x de of
  Just d -> deepDomain de d
deepDomain _ d = d
