module Generation.Latex.Dom where

import Syntax.Ast

domTex :: Dom -> String
domTex = \case
  IntDom -> "\\mathbb Z"
  BoolDom -> "\\{ \\ltrue, \\lfalse \\}"
  StrDom -> "\\Sigma^{*}"
  SymDom -> "\\chi^{*}"
  EDom -> ""
  VarDom x -> x
  FuncDom d1 d2 -> domTex d1 ++ " -> " ++ domTex d2
  ProdDom d1 d2 -> domTex d1 ++ " \\times " ++ domTex d2
  UnionDom ts False -> "\\bigcup(" ++ drop 3 (unionTex ts) ++ ")"
  UnionDom ts True -> drop 4 (synTex ts)

unionTex = let
    newl = "&\\ "
  in \case
    [] -> newl ++ "\n"
    [(t, d)] -> newl ++ "\\sv{" ++ t ++ "} [" ++ domTex d ++ "]" ++ "\n"
    ((t, d) : ts) -> newl ++ "\\sv{" ++ t ++ "} [" ++ domTex d ++ "]" ++ " + " ++ "\\\\\n" ++ unionTex ts


synTex = let
    newl = "&|\\ "
  in \case
    [] -> newl ++ "\n"
    [(t, d)] -> newl ++"\\sv{" ++ t ++ "}\\ " ++ domTex d ++ "\n"
    ((t, d) : ts) -> newl ++ "\\sv{" ++ t ++ "}\\ " ++ domTex d ++ "\\\\\n" ++ synTex ts