module Generation.Haskell.Conf where

import Syntax.Ast

import Generation.Haskell.General
import Generation.Haskell.Exp

conHas = \case
  ECon -> ""
  TagCon t c -> "(" ++ tagHas t ++ " " ++ conHas c ++ ")"
  PairCon c1 c2 -> "(" ++ conHas c1 ++ ", " ++ conHas c2 ++ ")"
  VarCon x -> varHas x
  ConstCon c -> constHas c