module Generation.Latex.TSys where

import Syntax.Ast

import Generation.Latex.General
import Generation.Latex.Conf
import Generation.Latex.Dom
import Generation.Latex.Exp

tdomTex :: TDom -> String
tdomTex (TDom d1@(Dom _ d1') d2 d3) =
    let
      binding = if d1' == EDom then ""
        else domTex d1 ++ " |- "
    in binding ++ domTex d2 ++ " \\Downarrow " ++ domTex d3

ruleTex (Rule (l, pos) c1@(Con _ c1') c2 e prs) =
  let
    bindingEnvironment = if c1' /= ECon then conTex c1 ++ " |- "
      else ""
    conclusion = bindingEnvironment ++ checkPairCon c2 ++ " \\Downarrow " ++ checkPairExp e ++ "\n"
    body = let          
        prems = prsTex prs
      in if prems == "" then conclusion
        else "\\inference{\n"
          ++ prems
          ++ "}{\n"
          ++ conclusion
          ++ "}"
    conditions = let
        sides = sidesTex prs
      in if sides == "" then " & "
        else "\\\\ & \\begin{array}{ll}\n"
          ++ sides
          ++ "\\end{array}\n"
  in "\\namedRule{" ++ l ++ "}{\n"
      ++ body
      ++ conditions
      ++ "}\\\\\\\\\n"

prsTex = \case
  [] -> ""
  pr : prs -> let
      result = prTex pr
    in if result /= "" then result ++ "\\\\\n" ++ prsTex prs
      else prsTex prs

prTex (Pr _ pr)= case pr of
  TrPr e1@(Exp _ e1') e2 "/" c -> let
      binding = case e1' of
        EExp -> ""
        _ -> expTex e1 ++ " |- "
    in binding ++ checkPairExp e2 ++ " \\Downarrow " ++ checkPairCon c
  _ -> ""

sidesTex = \case
  [] -> ""
  pr : prs -> let
      result = sideTex pr
    in if result /= "" then result ++ "\\\\\n" ++ sidesTex prs
      else sidesTex prs

sideTex (Pr _ pr) = case pr of
  IfPr e -> "\\textrm{if } " ++ expTex e
  LetPr c e -> "\\textrm{where } " ++ conTex c ++ " = " ++ expTex e
  LetrPr d x1 x2 e -> "\\textrm{where } " ++ varTex x1 ++ " \\in " ++ domTex d ++ "\\\\\n"
    ++ "\\textrm{and } " ++ varTex x2 ++ " = \\lambda " ++ varTex x2 ++ " . " ++ expTex e
  TrPr e1@(Exp _ e1') e2 x c -> if x /= "/" then let
        binding = case e1' of
          EExp -> ""
          _ -> expTex e1 ++ " |- "
      in binding ++ checkPairExp e2 ++ " \\Downarrow_\\tsys{" ++ x ++ "}" ++ checkPairCon c
    else ""

checkPairExp e@(Exp _ e') = case e' of
  Pair{} -> "\\langle " ++ expTex e ++ " \\rangle"
  _ -> expTex e

checkPairCon c@(Con _ c') = case c' of
  PairCon{} -> "\\langle " ++ conTex c ++ " \\rangle"
  _ -> conTex c
