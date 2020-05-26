module Semantics.Exp.General where

import Data.Map

import Syntax.Ast

-- Data environments
type TEnv = Map String Dom
  