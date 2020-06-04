module Generation.Latex.Df where

import Syntax.Ast

import Generation.Latex.Dom
import Generation.Latex.Exp
import Generation.Latex.TSys

printDomDfs :: [Df] -> String
printDomDfs dfs = "\\begin{align*}\n"
  ++  concatMap printDomDf dfs
  ++ "\\end{align*}\n\n"

printDomDf :: Df -> String
printDomDf = \case
  DomDf x d -> let
      eq = case d of
        UnionDom _ True -> "::="
        _ -> "="
    in x ++ " & " ++ eq ++ " " ++ domTex d ++ "\\\\\n"
  _ -> ""

printDataDfs :: [Df] -> String
printDataDfs dfs = "\\begin{align*}\n"
  ++ concatMap printDataDf dfs
  ++ "\\end{align*}\n\n"

printDataDf = \case
  DataDf x e -> x ++ " & := " ++ expTex e ++ "\\\\\n"
  DataRecDf d x e -> x ++ " \\in " ++ domTex d ++ " & := " ++ expTex e ++ "\\\\\n"
  _ -> ""

printTSysDfs :: [Df] -> String
printTSysDfs = concatMap printTSysDf

printTSysDf = \case
  TSysDf td x rs -> "\\begin{align*}\n"
    ++ "\\tsys{" ++ x ++ "} : " ++ tdomTex td
    ++ "\n\\end{align*}\n\n\n"
    ++ "\\begin{align*}\n"
    ++ concatMap ruleTex rs
    ++ "\\end{align*}\n\n"
  _ -> ""
