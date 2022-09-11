module Semantics.Exp.Eval (expEval) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Exp.General

import Semantics.Conf.Eval

getConst :: Const -> Err Value
getConst (Const pos c) = case c of
  Bot _ -> errMsg pos "Undefined: tried to evaluate undefined value."
  Int i -> Ok (VInt i)
  Str s -> Ok (VStr s)
  Sym y -> Ok (VSym y)
  BConst b -> Ok (VBool b)

expEval :: Env -> Exp -> Err Value
expEval env (Exp pos e) = let
  -- Evaluate with changing the environment
  eval = expEval env
  in case e of
  -- Constants and variables
  EExp -> Ok VEmpty
  ConstE c -> getConst c
  Var x -> case Data.Map.lookup x env of
    Just v -> Ok v
    Nothing -> errMsg pos ("Undeclared variable: " ++ x)
  -- Arithmetic expressions
  Plus e1 e2 -> results [eval e1, eval e2] >>=
    \[VInt v1, VInt v2] -> Ok (VInt (v1 + v2))
  Minus e1 e2 -> results [eval e1, eval e2] >>=
    \[VInt v1, VInt v2] -> Ok (VInt (v1 - v2))
  Prod e1 e2 -> results [eval e1, eval e2] >>=
    \[VInt v1, VInt v2] -> Ok (VInt (v1 * v2))
  Div e1 e2 -> results [eval e1, eval e2] >>= \case
    [VInt v1, VInt 0] -> errMsg pos "Arithmetic error: Attempted to divide by 0.";
    [VInt v1, VInt v2] -> Ok (VInt (v1 `div` v2))
  Mod e1 e2 -> results [eval e1, eval e2] >>= \case
    [VInt v1, VInt 0] -> errMsg pos "Arithmetic error: Attempted modulo 0.";
    [VInt v1, VInt v2] -> Ok (VInt (v1 `mod` v2))
  Inverse e -> eval e >>= \(VInt v) -> Ok (VInt (-v))
  -- Boolean expressions
  Neg e -> eval e >>= \(VBool v) -> Ok (VBool (not v))
  Or e1 e2 -> eval e1 >>= \(VBool v1) ->
    if v1 then Ok (VBool True) else eval e2
  And e1 e2 -> eval e1 >>= \(VBool v1) ->
    if v1 then eval e2 else Ok (VBool False)
-- Relational expressions
  Equal e1 e2 -> results [eval e1, eval e2] >>=
    \[v1, v2] -> Ok (VBool (v1 == v2))
  NotEqual e1 e2 -> results [eval e1, eval e2] >>=
    \[v1, v2] -> Ok (VBool (v1 /= v2))
  LessThan e1 e2 -> results [eval e1, eval e2] >>=
    \[VInt v1, VInt v2] -> Ok (VBool (v1 < v2))
  LessEqThan e1 e2 -> results [eval e1, eval e2] >>=
    \[VInt v1, VInt v2] -> Ok (VBool (v1 <= v2))
  GreaterThan e1 e2 -> results [eval e1, eval e2] >>=
    \[VInt v1, VInt v2] -> Ok (VBool (v1 > v2))
  GreaterEqThan e1 e2 -> results [eval e1, eval e2] >>=
    \[VInt v1, VInt v2] -> Ok (VBool (v1 >= v2))
-- String expressions
  Concat e1 e2 -> results [eval e1, eval e2] >>=
    \[VStr v1, VStr v2] -> Ok (VStr (v1 ++ v2))
-- Simple \-calculus
  Lambda _ x e -> Ok (Cloj env x e)
  App e1 e2 -> results [eval e1, eval e2] >>= \case
    [Cloj env' x e', v] -> expEval (Data.Map.insert x v env') e'
    [RCloj env' x x' e', v] -> let
      env''' = Data.Map.insert x (RCloj env' x x' e') env'
      env'' = Data.Map.insert x' v env'''
      in expEval env'' e'
-- Extended \-calculus
  Let c e1 e2 -> eval e1 >>= confEval env c >>=
    \env' -> expEval env' e2
  Letr _ x x' e1 e2 -> let
      env' = Data.Map.insert x (RCloj env x x' e1) env
    in expEval env' e2
  If e1 e2 e3 -> eval e1 >>= \case
    VBool True -> eval e2
    VBool False -> eval e3
  Update e1 e2 e3 -> eval e3 >>= \case
    Cloj env' x e -> Ok (Cloj
        env' x
        (Exp pos (If
          (Exp pos (Equal
            (Exp pos (Var x))
            (Exp pos (Closure env e1))))
          (Exp pos (Closure env e2))
          e)))
    RCloj env' x x' e -> Ok (RCloj
        env' x x'
        (Exp pos (If
          (Exp pos (Equal
            (Exp pos (Var x))
            (Exp pos (Closure env e1))))
          (Exp pos (Closure env e2))
          e)))
  Closure env' e -> expEval env' e
-- Operations on pairs
  Pair e1 e2 -> results [eval e1, eval e2] >>=
    \[v1, v2] -> Ok (VPair v1 v2)
  Head e -> eval e >>= \(VPair v1 v2) -> Ok v1
  Tail e -> eval e >>= \(VPair v1 v2) -> Ok v2
-- Operations with tags
  Inject t (Exp _ EExp) -> Ok (VTag t)
  Inject t e -> eval e >>= \v -> Ok (VTagE t v)
  Project e t -> eval e >>=
    \(VTagE t' v) -> if t == t' then Ok v
      else errMsg pos ("Invalid projection: attempted to project on tag " ++ t ++ " but the expression is wrapped in tag " ++ t' ++ " instead")
  IsTag e t -> eval e >>= \case
    VTag t' -> Ok (VBool (t == t'))
    VTagE t' _ -> Ok (VBool (t == t'))