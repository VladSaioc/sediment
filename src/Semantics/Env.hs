module Semantics.Env (
  DomEnv,
  TTSEnv,
  TSEnv,
  -- TEnv,
  -- Env,
) where

import Syntax.Ast
import Data.Map

-- Domain environment
type DomEnv = Map String Dom

-- Transition system environments
type TTSEnv = Map String TDom
type TSEnv = Map String [Rule]

-- Data environments
-- type TEnv = Map String Dom
-- type Env = Map String Value
