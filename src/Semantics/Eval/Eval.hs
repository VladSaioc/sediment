module Semantics.Eval.Eval (evalEval) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General

import Semantics.Exp.General
import Semantics.Exp.Eval

import Semantics.TSys.General
import Semantics.TSys.Eval

evalEval :: TSEnv -> Env -> Ev -> Err Value
evalEval tenv env (Ev e1 e2 x) = do
  ve <- expEval env e1
  v <- expEval env e2
  let Just tsys = Data.Map.lookup x tenv
  let tenv' = Data.Map.insert thisTSys tsys tenv
  tsysEval "" tenv' env v ve tsys