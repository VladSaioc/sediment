module Semantics.Expressions.Value (
  Value
) where

import Syntax.Ast
import Semantics.Env

data Value = VNum Integer | VStr String | VBool Bool | VSym String
  | VPair Value Value
  | VTag String | VTagE String Value
  deriving (Eq, Ord, Show, Read)
-- | Cloj Env Exp
-- | RCloj Env String Exp
