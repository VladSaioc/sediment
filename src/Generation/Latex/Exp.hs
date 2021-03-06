module Generation.Latex.Exp where

import Syntax.Ast

import Generation.Latex.Dom

import Generation.Latex.General
import Generation.Latex.Conf

putParens :: Exp -> Exp -> String
putParens e1@(Exp _ e1') e2@(Exp _ e2') = let
    parensEnc e = " ( " ++ expTex e ++ " ) "
  in case (e1', e2') of
    (Equal{}, Var{}) -> expTex e2
    (Equal{}, ConstE{}) -> expTex e2
    (Equal{}, Update{}) -> expTex e2
    (Equal{}, Inject{}) -> expTex e2
    (NotEqual{}, Var{}) -> expTex e2
    (NotEqual{}, ConstE{}) -> expTex e2
    (NotEqual{}, Update{}) -> expTex e2
    (NotEqual{}, Inject{}) -> expTex e2
    (LessThan{}, Var{}) -> expTex e2
    (LessThan{}, ConstE{}) -> expTex e2
    (LessThan{}, Update{}) -> expTex e2
    (LessThan{}, Inject{}) -> expTex e2
    (LessEqThan{}, Var{}) -> expTex e2
    (LessEqThan{}, ConstE{}) -> expTex e2
    (LessEqThan{}, Update{}) -> expTex e2
    (LessEqThan{}, Inject{}) -> expTex e2
    (GreaterThan{}, Var{}) -> expTex e2
    (GreaterThan{}, ConstE{}) -> expTex e2
    (GreaterThan{}, Update{}) -> expTex e2
    (GreaterThan{}, Inject{}) -> expTex e2
    (GreaterEqThan{}, Var{}) -> expTex e2
    (GreaterEqThan{}, ConstE{}) -> expTex e2
    (GreaterEqThan{}, Update{}) -> expTex e2
    (GreaterEqThan{}, Inject{}) -> expTex e2
    (Update{}, Update{}) -> expTex e2
    (Update{}, Var{}) -> expTex e2
    (Pair{}, Pair{}) -> parensEnc e2
    (_, Let{}) -> expTex e2
    (_, Letr{}) -> expTex e2
    (_, If{}) -> expTex e2
    (Let{}, Lambda{}) -> expTex e2
    (If{}, Lambda{}) -> expTex e2
    (Plus{}, Plus{}) -> expTex e2
    (Plus{}, Minus{}) -> expTex e2
    (Minus{}, Plus{}) -> expTex e2
    (Minus{}, Minus{}) -> expTex e2
    (Prod{}, Prod{}) -> expTex e2
    (_, Div{}) -> expTex e2
    (Prod{}, Mod{}) -> expTex e2
    (Mod{}, Prod{}) -> expTex e2
    (Head{}, Tail{}) -> expTex e2
    (Tail{}, Head{}) -> expTex e2
    _ -> if e1 > e2 then parensEnc e2
      else expTex e2

expTex :: Exp -> String
expTex e'@(Exp _ e) = case e of
  Var x -> varTex x
  EExp -> " \\epsilon "
  ConstE c -> constTex c
  Lambda d x e -> " \\lambda " ++ varTex x ++ " \\in " ++ domTex d ++ " .\\\\ " ++ putParens e' e
  App e1 e2 -> putParens e' e1 ++ "(" ++ expTex e2 ++ ")"
  Let c e1 e2 -> "\\begin{array}{l} \\sv{let}\\ " ++ conTex c ++ " = " ++ putParens e' e1 ++ " \\\\\n\\sv{in}\\ " ++ putParens e' e2 ++ "\n\\end{array}"
  Letr d x1 x2 e1 e2 -> "\\begin{array}{l} \n\\sv{let*}\\ " ++ varTex x1 ++ " \\in " ++ show d  ++ " = \\lambda " ++ varTex x2 ++ "." ++ putParens e' e1 ++ "\\\\\n\\sv{in}\\ " ++ putParens e' e2 ++ "\n\\end{array}"
  If e1 e2 e3 -> " \\begin{cases}\n" ++ expTex e2 ++ " & \\textrm{if } " ++ expTex e1 ++ "\\\\\n" ++ expTex e3 ++ " & \\textrm{otherwise }\n\\end{cases}"
  Update e1 e2 e3 -> putParens e' e3 ++ "[" ++ expTex e1 ++ " \\mapsto " ++ expTex e2 ++ "]"
  Concat e1 e2 -> putParens e' e1 ++ " \\| " ++ expTex e2
  Inverse e -> " - " ++ expTex e
  Plus e1 e2 -> putParens e' e1 ++ " + " ++ putParens e' e2
  Minus e1 e2 -> putParens e' e1 ++ " - " ++ putParens e' e2
  Prod e1 e2 -> putParens e' e1 ++ " \\cdot " ++ putParens e' e2
  Div e1 e2 -> "\\frac{" ++ expTex e1 ++ "}{" ++ expTex e2 ++ "}"
  Mod e1 e2 -> "\\textrm{mod}_{" ++ expTex e2 ++ "}" ++ putParens e' e1
  Neg e -> "\\neg " ++ putParens e' e
  Or e1 e2 -> putParens e' e1 ++ " \\lor " ++ putParens e' e2
  And e1 e2 -> putParens e' e1 ++ " \\land " ++ putParens e' e2
  Equal e1 e2 -> putParens e' e1 ++ " = " ++ putParens e' e2
  NotEqual e1 e2 -> putParens e' e1 ++ " \\neq " ++ putParens e' e2
  LessThan e1 e2 -> putParens e' e1 ++ " < " ++ putParens e' e2
  GreaterThan e1 e2 -> putParens e' e1 ++ " > " ++ putParens e' e2
  LessEqThan e1 e2 -> putParens e' e1 ++ " \\leq " ++ putParens e' e2
  GreaterEqThan e1 e2 -> putParens e' e1 ++ " \\geq " ++ putParens e' e2
  Pair e1 e2 -> putParens e' e1 ++ " , " ++ expTex e2
  Head e -> " \\sv{head}( " ++ expTex e ++ " )"
  Tail e -> " \\sv{tail}( " ++ expTex e ++ " )"
  Project e t -> " \\prod_\\sv{" ++ t ++ "} " ++ putParens e' e
  Inject t e -> " \\sv{" ++ t ++ "} \\{ " ++ expTex e ++ " \\} "
  IsTag e t -> putParens e' e ++ " \\ \\sv{is}\\ " ++ " \\sv{" ++ t ++ "}"