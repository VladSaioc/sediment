module Semantics.TSys.Eval (tsysEval) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General

import Semantics.Conf.Eval

import Semantics.Exp.General
import Semantics.Exp.Eval

import Semantics.TSys.General

tsysEval :: String -> TSEnv -> Env -> Value -> Value -> [Rule] -> Err Value
tsysEval errm tenv env v ve [] = Bad  (errm ++ "\n" ++ "Failed evaluation: no rules matched." ++ "\n" ++ "Values evaluated against: " ++ show ve ++ " |- " ++ show v ++ "\n")
tsysEval errm tenv env v ve (r@(Rule label _ _ _ _) : rs) = case ruleEval tenv env v ve r of
  Bad msg -> tsysEval (errm ++ "\n" ++ "[[" ++ label ++ "]]: " ++ msg) tenv env v ve rs
  Ok v -> Ok v

ruleEval :: TSEnv -> Env -> Value -> Value -> Rule -> Err Value
ruleEval tenv env v ve (Rule _ ce c e prs) = do
  env' <- confEval env ce ve
  env'' <- confEval env' c v
  env''' <- prEval tenv env'' prs
  expEval env''' e

prEval :: TSEnv -> Env -> [Pr] -> Err Env
prEval tenv env = Prelude.foldl (prEval' tenv) (Ok env)

prEval' :: TSEnv -> Err Env -> Pr -> Err Env
prEval' tenv (Bad msg) = \_ -> Bad msg
prEval' tenv (Ok env) = \case
  IfPr e -> expEval env e >>= \case
    VBool True -> Ok env
    VBool False -> Bad "Side condition is false"
  LetPr x e -> expEval env e >>= \v -> Ok (Data.Map.insert x v env)
  LetrPr _ x e -> expEval env e >>= \(Cloj env' x' e') -> Ok (Data.Map.insert x (RCloj env' x x' e') env)
  TrPr e1 e2 x c -> results [expEval env e1, expEval env e2] >>= \[ve, v] -> do
    let Just tsys = Data.Map.lookup x tenv
    let tenv' = Data.Map.insert thisTSys tsys tenv
    v <- tsysEval ("|-> " ++ x ++ " :\n") tenv' env v ve tsys
    confEval env c v