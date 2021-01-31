module Semantics.Dom.General where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General

-- Domain environment
type DomEnv = Map String Dom

type TagTable = Map String (String, Dom)

isUnion :: DomEnv -> Dom -> Bool
isUnion de (Dom _ (VarDom x)) = case Data.Map.lookup x de of
  Just d -> isUnion de d
isUnion _ (Dom _ UnionDom{}) = True
isUnion _ d = False

rootDomain :: DomEnv -> Dom -> Dom
rootDomain de (Dom _ (VarDom x)) = case Data.Map.lookup x de of
  Just (Dom pos UnionDom{}) -> Dom pos (VarDom x)
  Just d -> rootDomain de d
rootDomain _ d = d

rootDomains :: DomEnv -> [Err Dom] -> Err [Dom]
rootDomains de = results . Prelude.map (\case
  Bad msg -> Bad msg
  Ok d -> Ok (rootDomain de d))