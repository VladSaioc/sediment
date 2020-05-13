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
confT de tt env ECon d = case d of
  EDom -> Ok env
  _ -> Bad ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show EDom ++ " but found " ++ show d ++ " instead.")
confT de tt env (ConstCon c) d = case getConst c of
  Bad -> Bad msg
  Ok d' -> if deq de (d, d') then Ok env
    else Bad ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show d ++ ", but found " ++ show d' ++ " instead.")
confT de tt env (VarCon x) d = case d of
  EDom -> Bad "Invalid pattern: cannot use variable in patterns when the expected domain is empty."
  _ -> Ok (Data.Map.insert x d env)
confT de tt env (TagCon t c) d = case Data.Map.lookup t tt of
  Nothing -> Bad ("Invalid pattern: unrecognized tag " ++ t ++ ".")
  Just (x', d') -> case d of
    VarDom x -> if x == x' then confT de tt env c d'
      else Bad ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ x ++ ", but found " ++ x' ++ " instead.")
    _ -> Bad ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show d ++ ", but found " ++ x' ++ " instead.")
confT de tt env (PairCon c1 c2) d = case d of
  ProdDom d1 d2 -> case confT de tt env c1 d1 of
    Bad msg -> Bad msg
    Ok env' -> case confT de tt env' c2 d2 of
      Bad msg -> a
  _ -> Bad ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show d ++ ", but found a product domain instead.")