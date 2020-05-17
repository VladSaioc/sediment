module Semantics.Execute (execute) where


import Syntax.Ast
import Syntax.ErrM

import Semantics.General

import Semantics.Exp.Bind
import Semantics.Exp.General
import Semantics.Exp.Eval

import Semantics.Eval.Eval

import Semantics.TSys.Bind
import Semantics.TSys.General
import Semantics.TSys.Eval

execute :: Spec -> Err [Value]
execute (Spec dfs evs) = do
  let tenv = bindTSys dfs
  env <- bindData dfs
  evaluate tenv env evs

evaluate :: TSEnv -> Env -> [Ev] -> Err [Value]
evaluate tenv env = results . Prelude.map (evalEval tenv env)