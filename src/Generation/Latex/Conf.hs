module Generation.Latex.Conf where

import Data.Char (isDigit)

import Syntax.Ast
import Generation.Latex.Exp

conTex = \case
  ConstCon c -> constTex c
  PairCon c1 c2 -> conTex c1 ++ " , " ++ conTex c2
  ECon -> ""
  VarCon x -> expTex (Var x)
  TagCon t c -> "\\sv{" ++ t ++ "} [ " ++ conTex c ++ " ] "