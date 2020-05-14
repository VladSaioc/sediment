module Semantics.TSys.Eval where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General

import Semantics.Conf.Eval

import Semantics.Exp.General
import Semantics.Exp.Eval

import Semantics.TSys.General

tsysEval :: TSEnv -> Env -> Value -> Value -> [Rule] -> Err Value
tsysEval tenv env v ve [] = Bad "Failed evaluation: no rules matched."
tsysEval tenv env v ve (r:rs) = case ruleEval tenv env v ve r of
  Bad _ -> tsysEval tenv env v ve rs
  Ok v -> Ok v

ruleEval :: TSEnv -> Env -> Value -> Value -> Rule -> Err Value
ruleEval tenv env v ve (Rule _ ce c e prs) = case confEval env ce ve of
  Bad msg -> Bad msg
  Ok env' -> case confEval env' c v of
    Bad msg -> Bad msg
    Ok env'' -> case prEval tenv env'' prs of
      Bad msg -> Bad msg
      Ok env''' -> expEval env''' e

prEval :: TSEnv -> Env -> [Pr] -> Err Env
prEval tenv env = Prelude.foldl (prEval' tenv) (Ok env)

prEval' :: TSEnv -> Err Env -> Pr -> Err Env
prEval' tenv (Bad msg) _ = Bad msg
prEval' tenv (Ok env) (IfPr e) = case expEval env e of
  Bad msg -> Bad msg
  Ok v -> case v of
    VBool True -> Ok env
    VBool False -> Bad "Side condition is false"
prEval' tenv (Ok env) (LetPr x e) = case expEval env e of
  Bad msg -> Bad msg
  Ok v -> Ok (Data.Map.insert x v env)
prEval' tenv (Ok env) (LetrPr _ x e) = case expEval env e of
  Bad msg -> Bad msg
  Ok (Cloj env' x' e') -> Ok (Data.Map.insert x (RCloj env' x x' e') env)
prEval' tenv (Ok env) (TrPr e1 e2 x c) = case results [expEval env e1, expEval env e2] of
  Bad msg -> Bad msg
  Ok [ve, v] -> case Data.Map.lookup x tenv of
    Just tsys -> let
        tenv' = Data.Map.insert thisTSys tsys tenv
      in case tsysEval tenv' env v ve tsys of
        Bad msg -> Bad msg
        Ok v -> confEval env c v