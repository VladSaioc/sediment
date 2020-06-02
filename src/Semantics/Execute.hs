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

execute :: Spec -> [Err String]
execute (Spec dfs evs) = let tenv = bindTSys dfs
  in case bindData dfs of
    Ok env -> Prelude.map (evalEval tenv env) evs
    Bad msg -> [Bad msg]