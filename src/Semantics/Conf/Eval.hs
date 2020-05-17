module Semantics.Conf.Eval (confEval) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Exp.General

constCompare :: Const -> Value -> Bool
constCompare c v = case (c,v) of
  (Int v, VInt v') -> v == v'
  (Str v, VStr v') -> v == v'
  (BConst v, VBool v') -> v == v'
  (Sym v, VSym v') -> v == v'
  _ -> False

confEval :: Env -> Con -> Value -> Err Env
confEval env = \case
  ECon -> \_ -> Ok env
  ConstCon c -> \v -> if constCompare c v then Ok env
    else Bad ("Pattern evaluation mismatch: expected constant " ++ show c ++ " but found " ++ show v ++ " instead.")
  VarCon x -> \v -> Ok (Data.Map.insert x v env)
  TagCon t c -> \case
    VTag t' -> if t == t' then Ok env
      else Bad ("Pattern evaluation mismatch: pattern expects tag " ++ t ++ ", but found " ++ t' ++ " instead.")
    VTagE t' v' -> if t == t' then confEval env c v'
      else Bad ("Pattern evaluation mismatch: pattern expects tag " ++ t ++ ", but found " ++ t' ++ " instead.")
  PairCon c1 c2 -> \(VPair v1 v2) -> do
    env' <- confEval env c1 v1
    confEval env' c2 v2