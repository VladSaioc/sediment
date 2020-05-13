module Semantics.Dom.General where

import Data.Map

import Syntax.Ast

-- Domain environment
type DomEnv = Map String Dom

type TagTable = Map String (String, Dom)

deepDomain :: DomEnv -> Dom -> Dom
deepDomain de (VarDom x) = case Data.Map.lookup x de of
  Just d -> deepDomain de d
deepDomain _ d = d
