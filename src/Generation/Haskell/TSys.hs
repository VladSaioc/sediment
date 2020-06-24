module Generation.Haskell.TSys where

import Syntax.Ast

import Generation.Haskell.General
import Generation.Haskell.Exp
import Generation.Haskell.Conf

ruleHas ts (Rule _ c1 c2 e prs) = let
  input = case c1 of
    ECon -> conHas c2
    _ -> "(" ++ conHas c1 ++ ", " ++ conHas c2 ++ ")"
  in input ++ " -> do\n" ++ prsHas ts e "    " prs

prsHas ts res i = let
  iplus = i ++ "  "
  iplusplus = iplus ++ "  "
  in \case
    [] -> i ++ "  " ++ expHas res
    (pr : prs) -> case pr of
      IfPr e -> i ++ "case (" ++ expHas e ++ ") of\n" ++ iplus ++ "True -> do\n" ++ prsHas ts res iplusplus prs
      LetPr c e-> i ++ "let " ++ conHas c ++ " = " ++ expHas e ++ "\n" ++ prsHas ts res i prs
      LetrPr _ x1 x2 e -> i ++ "let " ++ varHas x1 ++ " = Updatable (\\" ++ varHas x2 ++ " -> " ++ expHas e ++ ") Data.Map.empty\n" ++ prsHas ts res i prs
      TrPr e1 e2 ts' c -> let
          input = case e1 of
            EExp -> "(" ++ expHas e2 ++ ")"
            _ -> " (" ++ expHas e1 ++ ", " ++ expHas e2 ++ ")"
        in case ts' of
        "/" -> i ++ "case " ++ varTsysHas ts ++ input ++ " of\n" ++ iplus ++ conHas c ++ " -> do\n" ++ prsHas ts res iplusplus prs
        _ -> i ++ "case " ++ varTsysHas ts' ++ input ++ " of\n" ++ iplus ++ conHas c ++ " -> do\n" ++ prsHas ts res iplusplus prs
