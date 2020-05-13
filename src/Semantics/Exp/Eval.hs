module Semantics.Exp.Eval (expEval) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Exp.General

getConst :: Const -> Err Value
getConst (Bot _) = Bad "Undefined: tried to evaluate undefined value."
getConst (Int i) = Ok (VInt i)
getConst (Str s) = Ok (VStr s)
getConst (Sym y) = Ok (VSym y)
getConst (BConst b) = Ok (VBool b)

expEval :: Env -> Exp -> Err Value
-- Constants and variables
expEval env (ConstE c) = getConst c
expEval env (Var x) = let
    Just v = Data.Map.lookup x env
  in Ok v
-- Arithmetic expressions
expEval env (Plus e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VInt v1, VInt v2] -> Ok (VInt (v1 + v2))
expEval env (Minus e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VInt v1, VInt v2] -> Ok (VInt (v1 - v2))
expEval env (Prod e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VInt v1, VInt v2] -> Ok (VInt (v1 * v2))
expEval env (Div e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VInt v1, VInt 0] -> Bad "Attempted to divide by 0.";
  Ok [VInt v1, VInt v2] -> Ok (VInt (v1 `div` v2))
expEval env (Mod e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VInt v1, VInt 0] -> Bad "Attempted to divide by 0.";
  Ok [VInt v1, VInt v2] -> Ok (VInt (v1 `mod` v2))
expEval env (Inverse e) = case expEval env e of
  Bad msg -> Bad msg
  Ok (VInt v) -> Ok (VInt (-v))
-- Boolean expressions
expEval env (Neg e) = case expEval env e of
  Bad msg -> Bad msg
  Ok (VBool v) -> Ok (VBool (not v))
expEval env (Or e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VBool v1, VBool v2] -> Ok (VBool (v1 || v2))
expEval env (And e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VBool v1, VBool v2] -> Ok (VBool (v1 && v2))
-- Relational expressions
expEval env (Equal e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [v1, v2] -> Ok (VBool (v1 == v2))
expEval env (NotEqual e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [v1, v2] -> Ok (VBool (v1 /= v2))
expEval env (LessThan e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VInt v1, VInt v2] -> Ok (VBool (v1 < v2))
expEval env (LessEqThan e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VInt v1, VInt v2] -> Ok (VBool (v1 <= v2))
expEval env (GreaterThan e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VInt v1, VInt v2] -> Ok (VBool (v1 > v2))
expEval env (GreaterEqThan e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VInt v1, VInt v2] -> Ok (VBool (v1 >= v2))
-- String expressions
expEval env (Concat e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [VStr v1, VStr v2] -> Ok (VStr (v1 ++ v2))
-- Simple \-calculus
expEval env (Lambda _ x e) = Ok (Cloj env x e)
expEval env (App e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [Cloj env' x e', v] -> expEval (Data.Map.insert x v env') e'
  Ok [RCloj env' x x' e', v] -> let
      env''' = Data.Map.insert x (RCloj env' x x' e') env'
      env'' = Data.Map.insert x' v env'''
    in expEval env'' e'
-- Extended \-calculus
expEval env (Let x e1 e2) = case expEval env e1 of
  Bad msg -> Bad msg
  Ok v -> expEval (Data.Map.insert x v env) e2
expEval env (Letr _ x e1 e2) = case expEval env e1 of
  Bad msg -> Bad msg
  Ok (Cloj env' x' e') -> let
      env'' = Data.Map.insert x (RCloj env' x x' e') env
    in expEval env'' e2
expEval env (If e1 e2 e3) = case expEval env e1 of
  Bad msg -> Bad msg
  Ok (VBool True) -> expEval env e2
  Ok (VBool False) -> expEval env e3
expEval env (Update e1 e2 e3) = case expEval env e3 of
  Bad msg -> Bad msg
  Ok (Cloj env' x e) -> Ok (Cloj env' x (If (Equal (Var x) e1) e2 e))
  Ok (RCloj env' x x' e) -> Ok (RCloj env' x x' (If (Equal (Var x) e1) e2 e))
-- Operations on pairs
expEval env (Pair e1 e2) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [v1, v2] -> Ok (VPair v1 v2)
expEval env (Head e) = case expEval env e of
  Bad msg -> Bad msg
  Ok (VPair v1 v2) -> Ok v1
expEval env (Tail e) = case expEval env e of
  Bad msg -> Bad msg
  Ok (VPair v1 v2) -> Ok v2
-- Operations with tags
expEval env (Inject t EExp) = Ok (VTag t)
expEval env (Inject t e) = case expEval env e of
  Bad msg -> Bad msg
  Ok v -> Ok (VTagE t v)
expEval env (Project e t) = case expEval env e of
  Bad msg -> Bad msg
  Ok (VTagE t' v) -> if t == t' then Ok v
    else Bad ("Invalid projection: attempted to project on tag " ++ t ++ " but the expression is wrapped in tag " ++ t' ++ " instead")
expEval env (IsTag e t) = case expEval env e of
  Bad msg -> Bad msg
  Ok (VTag t') -> Ok (VBool (t == t'))
  Ok (VTagE t' v) -> Ok (VBool (t == t'))
