module Semantics.Domains.Operations where

import Syntax.Ast

isUnion :: Dom -> Bool
isUnion (UnionDom _) = True
isUnion d = False

hasTag :: String -> [(String, Dom)] -> Maybe Dom
hasTag t [] = Nothing
hasTag t ((t', d) : tds) = if t == t' then Just d
  else hasTag t tds