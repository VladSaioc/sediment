module Semantics.Exp.General where

import Data.Map

import Syntax.Ast

-- Data environments
type TEnv = Map String Dom
type Env = Map String Value

data Value = VEmpty | VInt Integer | VStr String | VBool Bool | VSym String
  | VPair Value Value
  | VTag String | VTagE String Value
  | Cloj Env String Exp
  | RCloj Env String String Exp
  deriving (Eq, Ord, Read)

instance Show Value where
  show = \case
    VEmpty -> "_"
    VInt i -> show i
    VStr s -> "\"" ++ s ++ "\""
    VSym y -> "`" ++ y ++ "`"
    VPair v1 v2 -> "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
    VTag t -> t ++ "[]"
    VTagE t v -> t ++ "[" ++ show v ++ "]"
    Cloj env x e -> "{" ++ show env ++ ", " ++ x ++ ", " ++ show e ++ "}"
    RCloj env x x' e -> "{" ++ show env ++ ", " ++ x ++ ", " ++ x' ++ ", " ++ show e ++ "}"
  