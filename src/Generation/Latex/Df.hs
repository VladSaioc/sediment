module Generation.Latex.Df where

import Syntax.Ast

import Generation.Latex.Dom
import Generation.Latex.Exp
import Generation.Latex.TSys

printDomDfs :: [Df] -> String
printDomDfs dfs = let
  body' = concatMap printDomDf dfs
  body = take (length body' - 3) body'
  in if body /= "" then "\\begin{align*}\n"
  ++ body
  ++ "\\end{align*}\n\n"
  else ""

printDomDf :: Df -> String
printDomDf = \case
  DomDf x d -> let
    eq = case d of
      UnionDom _ True -> "::="
      _ -> "="
    in x ++ " & " ++ eq ++ " " ++ domTex d ++ "\\\\\\\\\n"
  _ -> ""

printDataDfs :: [Df] -> String
printDataDfs dfs = let
  body' = concatMap printDataDf dfs
  body = take (length body' - 3) body'
  in if body /= "" then "\\begin{align*}\n"
  ++ body
  ++ "\\end{align*}\n\n"
  else ""

printDataDf = \case
  DataDf x e -> expTex (Var x) ++ " & = \\begin{array}{ll}\n" ++ expTex e ++ "\n\\end{array}\\\\\\\\\n"
  DataRecDf d x x' e -> expTex (Var x) ++ " \\in " ++ domTex d ++ " & = " ++  "\\begin{array}{ll}\n \\lambda " ++ expTex (Var x') ++ " .\n" ++ expTex e ++ "\n\\end{array}\\\\\\\\\n"
  _ -> ""

printTSysDfs :: [Df] -> String
printTSysDfs = concatMap printTSysDf

printTSysDf = \case
  TSysDf td x rs -> let
    body' = concatMap ruleTex rs
    body = take (length body' - 5) body'
    in "\\begin{align*}\n"
    ++ "\\tsys{" ++ x ++ "} : " ++ tdomTex td
    ++ "\n\\end{align*}\n\n\n"
    ++ "\\begin{align*}\n"
    ++ body
    ++ "\\end{align*}\n\n"
  _ -> ""