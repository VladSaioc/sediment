module Generation.Latex.Conf where

import Data.Char (isDigit)

import Syntax.Ast
import Generation.Latex.General

conTex = \case
  ConstCon c -> constTex c
  PairCon c1 c2 -> conTex c1 ++ " , " ++ conTex c2
  ECon -> ""
  VarCon x -> varTex x
  TagCon t c -> "\\sv{" ++ t ++ "} [ " ++ conTex c ++ " ] "