module Semantics.Dom.General where

import Syntax.Ast
import Semantics.Env
import Data.Map

-- Domain environment
type DomEnv = Map String Dom

type TagTable = Map String (String, Dom)

deepDomain :: DomEnv -> Dom -> Dom
deepDomain de (VarDom x) = case Data.Map.lookup x de of
  Just d -> deepDomain de d
deepDomain _ d = d
