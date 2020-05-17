module Semantics.Conf.TypeS (confT) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.Dom.Equivalence
import Semantics.Dom.General

import Semantics.Exp.General

getConst :: Const -> Err Dom
getConst (Bot _) = Bad "Invalid pattern: disallowed usage of the bottom element in patterns."
getConst (Int _) = Ok IntDom
getConst (Str _) = Ok StrDom
getConst (Sym _) = Ok SymDom
getConst (BConst _) = Ok BoolDom

confT :: DomEnv -> TagTable -> TEnv -> Con -> Dom -> Err TEnv
confT de tt env = let
  this = confT de tt env
  in \case
  ECon -> \case
    EDom -> Ok env
    d -> Bad ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show EDom ++ " but found " ++ show d ++ " instead.")
  ConstCon c -> \d -> getConst c >>= \d' -> if deq de d d' then Ok env
    else Bad ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show d ++ ", but found " ++ show d' ++ " instead.")
  VarCon x -> \case
    EDom -> Bad "Invalid pattern: cannot use variable in patterns when the expected domain is empty."
    d -> Ok (Data.Map.insert x d env)
  TagCon t c -> case Data.Map.lookup t tt of
    Nothing -> \_ -> Bad ("Invalid pattern: unrecognized tag " ++ t ++ ".")
    Just (x', d') -> \case
      VarDom x -> if x == x' then this c d'
        else Bad ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ x ++ ", but found " ++ x' ++ " instead.")
      d -> Bad ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show d ++ ", but found " ++ x' ++ " instead.")
  PairCon c1 c2 -> \case
    ProdDom d1 d2 -> this c1 d1 >>= \env' -> confT de tt env' c2 d2 >>= Ok
    d -> Bad ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show d ++ ", but found a product domain instead.")