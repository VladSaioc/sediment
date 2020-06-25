module Generation.Haskell.Exp where

import Syntax.Ast
import Semantics.Exp.General

import Semantics.General

import Generation.Haskell.General
import Generation.Haskell.Conf

expHas = \case
  Pair e1 e2 -> "(" ++ expHas e1 ++ ", " ++ expHas e2 ++ ")"
  Lambda _ x e -> "(Updatable (\\" ++ varHas x ++ " -> " ++ expHas e ++ ") Data.Map.empty)"
  Let c e1 e2 -> "(let " ++ conHas c ++ " = " ++ expHas e1 ++ " in " ++ expHas e2 ++ ")"
  Letr _ x1 x2 e1 e2 -> "(let " ++ varHas x1 ++ " = Updatable (\\" ++ varHas x2 ++ " -> " ++ expHas e1  ++ ") Data.Map.empty" ++ " in " ++ expHas e2 ++ " )"
  If e1 e2 e3 -> "(if " ++ expHas e1 ++ " then " ++ expHas e2 ++ " else " ++ expHas e3 ++ ")"
  Concat e1 e2 -> "(" ++ expHas e1 ++ ") ++ (" ++ expHas e2 ++ ")"
  Plus e1 e2 -> "(" ++ expHas e1 ++ ") + (" ++ expHas e2 ++ ")"
  Minus e1 e2 -> "(" ++ expHas e1 ++ ") - (" ++ expHas e2 ++ ")"
  Prod e1 e2 -> "(" ++ expHas e1 ++ ") * (" ++ expHas e2 ++ ")"
  Div e1 e2 -> "(" ++ expHas e1 ++ ") `div` (" ++ expHas e2 ++ ")"
  Mod e1 e2 -> "(" ++ expHas e1 ++ ") `mod` (" ++ expHas e2 ++ ")"
  Or e1 e2 -> "(" ++ expHas e1 ++ ") || (" ++ expHas e2 ++ ")"
  And e1 e2 -> "(" ++ expHas e1 ++ ") && (" ++ expHas e2 ++ ")"
  Equal e1 e2 -> "(" ++ expHas e1 ++ ") == (" ++ expHas e2 ++ ")"
  NotEqual e1 e2 -> "(" ++ expHas e1 ++ ") /= (" ++ expHas e2 ++ ")"
  LessThan e1 e2 -> "(" ++ expHas e1 ++ ") < (" ++ expHas e2 ++ ")"
  LessEqThan e1 e2 -> "(" ++ expHas e1 ++ ") <= (" ++ expHas e2 ++ ")"
  GreaterThan e1 e2 -> "(" ++ expHas e1 ++ ") > (" ++ expHas e2 ++ ")"
  GreaterEqThan e1 e2 -> "(" ++ expHas e1 ++ ") >= (" ++ expHas e2 ++ ")"
  Inject t EExp -> "(" ++ tagHas t ++ ")"
  Inject t e -> "(" ++ tagHas t ++ " (" ++ expHas e ++ "))"
  IsTag e t -> "(case " ++ expHas e ++ " of " ++ tagHas t ++ "{} -> True; _ -> False)"
  Inverse e -> "-(" ++ expHas e ++ ")"
  Neg e -> "not (" ++ expHas e ++ ")"
  Project e t -> "(let " ++ tagHas t ++ " proj_ = " ++ expHas e ++ " in proj_)"
  Head e -> "fst (" ++ expHas e ++ ")"
  Tail e -> "snd (" ++ expHas e ++ ")"
  App e1 e2 -> "(apply " ++ expHas e1 ++ " " ++ expHas e2 ++ ")"
  Update e1 e2 e3 -> "(let Updatable f map = " ++ expHas e3 ++ " in Updatable f (Data.Map.insert " ++ expHas e1 ++ " " ++ expHas e2 ++ " map))"
  Var x -> varHas x
  ConstE c -> constHas c
