module Generation.Haskell.Dom where

import Data.List

import Syntax.Ast

import Generation.Haskell.General

domHas (Dom _ d) = case d of
  IntDom -> " Integer "
  BoolDom -> " Bool "
  StrDom -> " String "
  SymDom -> " String "
  EDom -> ""
  VarDom x -> varDomHas x
  FuncDom d1 d2 -> "(Updatable " ++ domHas d1 ++ " " ++ domHas d2 ++ ")"
  ProdDom d1 d2 -> "(" ++ domHas d1 ++ " , " ++ domHas d2 ++ ")"
  UnionDom ds _ -> intercalate " | " (map unHas ds)

unHas (t, d) = tagHas t ++ " " ++ domHas d