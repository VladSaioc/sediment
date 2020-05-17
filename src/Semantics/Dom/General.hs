module Semantics.Dom.General where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General

-- Domain environment
type DomEnv = Map String Dom

type TagTable = Map String (String, Dom)

isUnion :: DomEnv -> Dom -> Bool
isUnion de (VarDom x) = case Data.Map.lookup x de of
  Just d -> isUnion de d
isUnion _ UnionDom{} = True
isUnion _ d = False

rootDomain :: DomEnv -> Dom -> Dom
rootDomain de (VarDom x) = case Data.Map.lookup x de of
  Just UnionDom{} -> VarDom x
  Just d -> rootDomain de d
rootDomain _ d = d

rootDomains :: DomEnv -> [Err Dom] -> Err [Dom]
rootDomains de = results . Prelude.map (\case
  Bad msg -> Bad msg
  Ok d -> Ok (rootDomain de d))