module Generation.Haskell.General where

import Syntax.Ast

constHas = \case
  Bot _ -> "()"
  Int i -> show i
  Str s -> "\"" ++ s ++ "\""
  Sym y -> "\"" ++ y ++ "\""
  BConst b -> show b

varHas = ("mv_" ++)

tagHas = ("T_" ++)

varDomHas = ("D_" ++)

varTsysHas = ("ts_" ++)