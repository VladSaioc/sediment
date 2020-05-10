module Semantics.Exp.General where

import Data.Map

import Syntax.Ast

-- Data environments
type TEnv = Map String Dom
type Env = Map String Value

data Value = VInt Integer | VStr String | VBool Bool | VSym String
  | VPair Value Value
  | VTag String | VTagE String Value
  | Cloj Env String Exp
  | RCloj Env String String Exp
  deriving (Eq, Ord, Show, Read)
  