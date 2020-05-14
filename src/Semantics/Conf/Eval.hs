module Semantics.Conf.Eval (confEval) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Exp.General

constCompare :: Const -> Value -> Bool
constCompare (Int v) (VInt v') = v == v'
constCompare (Str v) (VStr v') = v == v'
constCompare (BConst v) (VBool v') = v == v'
constCompare (Sym v) (VSym v') = v == v'
constCompare _ _ = False

confEval :: Env -> Con -> Value -> Err Env
confEval env ECon _ = Ok env
confEval env (ConstCon c) v = if constCompare c v then Ok env
  else Bad ("Pattern evaluation mismatch: expected constant " ++ show c ++ " but found " ++ show v ++ " instead.")
confEval env (VarCon x) v = Ok (Data.Map.insert x v env)
confEval env (TagCon t c) v = case v of
  VTag t' -> if t == t' then Ok env
    else Bad ("Pattern evaluation mismatch: pattern expects tag " ++ t ++ ", but found " ++ t' ++ "instead.")
  VTagE t' v' -> if t == t' then confEval env c v'
    else Bad ("Pattern evaluation mismatch: pattern expects tag " ++ t ++ ", but found " ++ t' ++ "instead.")
confEval env (PairCon c1 c2) (VPair v1 v2) = case confEval env c1 v1 of
  Bad msg -> Bad msg
  Ok env' -> confEval env' c2 v2