module Semantics.Dom.General where

import Syntax.Ast
import Semantics.Env
import Data.Map

-- Domain environment
type DomEnv = Map String Dom

type TagTable = Map String (String, Dom)

isUnion :: Dom -> Bool
isUnion (UnionDom _) = True
isUnion d = False

getTagDom :: String -> [(String, Dom)] -> Maybe Dom
getTagDom t [] = Nothing
getTagDom t ((t', d) : tds) = if t == t' then Just d
  else getTagDom t tds

deepDomain :: DomEnv -> Dom -> Dom
deepDomain de (VarDom x) = case Data.Map.lookup x de of
  Just d -> deepDomain de d
deepDomain _ d = d
