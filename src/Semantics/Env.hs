module Semantics.Env (
  TTSEnv,
  TSEnv,
) where

import Data.Map

import Syntax.Ast

import Semantics.Exp.General

-- Transition system environments
type TTSEnv = Map String TDom
type TSEnv = Map String [Rule]