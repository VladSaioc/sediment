module Generation.Latex.Df where

import Syntax.Ast

import Generation.Latex.General
import Generation.Latex.Dom
import Generation.Latex.Exp
import Generation.Latex.Conf
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
printDomDf (Df _ d)= case d of
  DomDf x d@(Dom _ d') -> let
    eq = case d' of
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

printDataDf (Df _ d) = case d of
  DataDf c e -> conTex c ++ " & = \\begin{array}{ll}\n" ++ expTex e ++ "\n\\end{array}\\\\\\\\\n"
  DataRecDf d x x' e -> varTex x ++ " \\in " ++ domTex d ++ " & = " ++  "\\begin{array}{ll}\n \\lambda " ++ varTex x' ++ " .\n" ++ expTex e ++ "\n\\end{array}\\\\\\\\\n"
  _ -> ""

printTSysDfs :: [Df] -> String
printTSysDfs = concatMap printTSysDf

printTSysDf (Df _ d) = case d of
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
