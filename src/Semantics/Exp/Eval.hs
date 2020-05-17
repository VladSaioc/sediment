module Semantics.Exp.Eval (expEval) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Exp.General

getConst :: Const -> Err Value
getConst = \case
  Bot _ -> Bad "Undefined: tried to evaluate undefined value."
  Int i -> Ok (VInt i)
  Str s -> Ok (VStr s)
  Sym y -> Ok (VSym y)
  BConst b -> Ok (VBool b)

expEval :: Env -> Exp -> Err Value
expEval env = let
  this = expEval env
  in \case
  -- Constants and variables
  EExp -> Ok VEmpty
  ConstE c -> getConst c
  Var x -> case Data.Map.lookup x env of
    Just v -> Ok v
    Nothing -> Bad x
  -- Arithmetic expressions
  Plus e1 e2 -> results [this e1, this e2] >>=
    \[VInt v1, VInt v2] -> Ok (VInt (v1 + v2))
  Minus e1 e2 -> results [this e1, this e2] >>=
    \[VInt v1, VInt v2] -> Ok (VInt (v1 - v2))
  Prod e1 e2 -> results [this e1, this e2] >>=
    \[VInt v1, VInt v2] -> Ok (VInt (v1 * v2))
  Div e1 e2 -> results [this e1, this e2] >>= \case
    [VInt v1, VInt 0] -> Bad "Attempted to divide by 0.";
    [VInt v1, VInt v2] -> Ok (VInt (v1 `div` v2))
  Mod e1 e2 -> results [this e1, this e2] >>= \case
    [VInt v1, VInt 0] -> Bad "Attempted to divide by 0.";
    [VInt v1, VInt v2] -> Ok (VInt (v1 `mod` v2))
  Inverse e -> this e >>= \(VInt v) -> Ok (VInt (-v))
  -- Boolean expressions
  Neg e -> this e >>= \(VBool v) -> Ok (VBool (not v))
  Or e1 e2 -> results [this e1, this e2] >>=
    \[VBool v1, VBool v2] -> Ok (VBool (v1 || v2))
  And e1 e2 -> results [this e1, this e2] >>=
    \[VBool v1, VBool v2] -> Ok (VBool (v1 && v2))
-- Relational expressions
  Equal e1 e2 -> results [this e1, this e2] >>=
    \[v1, v2] -> Ok (VBool (v1 == v2))
  NotEqual e1 e2 -> results [this e1, this e2] >>=
    \[v1, v2] -> Ok (VBool (v1 /= v2))
  LessThan e1 e2 -> results [this e1, this e2] >>=
    \[VInt v1, VInt v2] -> Ok (VBool (v1 < v2))
  LessEqThan e1 e2 -> results [this e1, this e2] >>=
    \[VInt v1, VInt v2] -> Ok (VBool (v1 <= v2))
  GreaterThan e1 e2 -> results [this e1, this e2] >>=
    \[VInt v1, VInt v2] -> Ok (VBool (v1 > v2))
  GreaterEqThan e1 e2 -> results [this e1, this e2] >>=
    \[VInt v1, VInt v2] -> Ok (VBool (v1 >= v2))
-- String expressions
  Concat e1 e2 -> results [this e1, this e2] >>=
    \[VStr v1, VStr v2] -> Ok (VStr (v1 ++ v2))
-- Simple \-calculus
  Lambda _ x e -> Ok (Cloj env x e)
  App e1 e2 -> results [this e1, this e2] >>= \case
    [Cloj env' x e', v] -> expEval (Data.Map.insert x v env') e'
    [RCloj env' x x' e', v] -> let
      env''' = Data.Map.insert x (RCloj env' x x' e') env'
      env'' = Data.Map.insert x' v env'''
      in expEval env'' e'
-- Extended \-calculus
  Let x e1 e2 -> this e1 >>=
    \v -> let env' = Data.Map.insert x v env
      in expEval env' e2
  Letr _ x e1 e2 -> this e1 >>=
    \(Cloj env' x' e') -> let
      env'' = Data.Map.insert x (RCloj env' x x' e') env
    in expEval env'' e2
  If e1 e2 e3 -> this e1 >>= \case
    VBool True -> this e2
    VBool False -> this e3
  Update e1 e2 e3 -> this e3 >>= \case
    Cloj env' x e -> Ok (Cloj env' x (If (Equal (Var x) e1) e2 e))
    RCloj env' x x' e -> Ok (RCloj env' x x' (If (Equal (Var x) e1) e2 e))
-- Operations on pairs
  Pair e1 e2 -> results [this e1, this e2] >>=
    \[v1, v2] -> Ok (VPair v1 v2)
  Head e -> this e >>= \(VPair v1 v2) -> Ok v1
  Tail e -> this e >>= \(VPair v1 v2) -> Ok v2
-- Operations with tags
  Inject t EExp -> Ok (VTag t)
  Inject t e -> this e >>= \v -> Ok (VTagE t v)
  Project e t -> this e >>=
    \(VTagE t' v) -> if t == t' then Ok v
      else Bad ("Invalid projection: attempted to project on tag " ++ t ++ " but the expression is wrapped in tag " ++ t' ++ " instead")
  IsTag e t -> this e >>= \case
    VTag t' -> Ok (VBool (t == t'))
    VTagE t' _ -> Ok (VBool (t == t'))