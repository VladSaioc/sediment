module Semantics.TSys.General where

import Data.Map

import Syntax.Ast

type TTSEnv = Map String TDom
type TSEnv = Map String [Rule]

thisTSys = "/"