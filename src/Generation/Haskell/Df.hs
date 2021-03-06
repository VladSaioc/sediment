module Generation.Haskell.Df where

import Data.List

import Syntax.Ast

import Generation.Haskell.General
import Generation.Haskell.Dom
import Generation.Haskell.Exp
import Generation.Haskell.Conf
import Generation.Haskell.TSys

prologueDom = "module Specification.Dom where\n\n"
  ++ "import Tooling.Updatable\n\n"

prologueData = "module Specification.Data where\n\n"
  ++ "import Data.Map\n\n"
  ++ "import Tooling.Updatable\n"
  ++ "import Specification.Dom\n\n"

prologueTSys = "module Specification.Systems where\n\n"
  ++ "import Data.Map\n\n"
  ++ "import Tooling.Updatable\n\n"
  ++ "import Specification.Dom\n"
  ++ "import Specification.Data\n\n"

printDomDfs :: [Df] -> String
printDomDfs dfs = prologueDom ++ intercalate "\n\n" (map printDomDf dfs)

printDomDf :: Df -> String
printDomDf (Df _ df)= case df of
  DomDf x d@(Dom _ d') -> case d' of
    UnionDom{} -> "data " ++ varDomHas x ++ " = " ++ domHas d
    _ -> "type " ++ varDomHas x ++ " = " ++ domHas d
  _ -> ""

printTSysDfs :: [Df] -> String
printTSysDfs dfs = prologueTSys ++ intercalate "\n\n" (map printTSysDf dfs)

printTSysDf :: Df -> String
printTSysDf (Df _ d) = case d of
  TSysDf _ x rs -> varTsysHas x ++ " input = case input of\n  " ++ intercalate "\n  " (map (ruleHas x) rs) ++ "\n\n"
  _ -> ""

printDataDfs :: [Df] -> String
printDataDfs dfs = prologueData ++ intercalate "\n\n" (map printDataDf dfs)

printDataDf :: Df -> String
printDataDf (Df _ d)= case d of
  DataDf c e -> conHas c ++ " = " ++ expHas e
  DataRecDf _ x1 x2 e -> varHas x1 ++ " = Updatable (\\" ++ varHas x2 ++ " -> " ++ expHas e ++ ") Data.Map.empty"
  _ -> ""
