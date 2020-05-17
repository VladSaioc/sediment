module Semantics.Playground where

import Data.Map

import Syntax.Ast

import Semantics.Exp.General
import Semantics.Exp.Eval

testUpdate = expEval Data.Map.empty (Update (ConstE (Str "a")) (ConstE (Int 5)) (Lambda EDom "x" (ConstE (Bot EDom))))