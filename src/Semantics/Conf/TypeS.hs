module Semantics.Conf.TypeS (confT) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General

import Semantics.Dom.Equivalence
import Semantics.Dom.General

import Semantics.Exp.General

getConst :: Const -> Err Dom
getConst (Const pos c) = case c of
  Bot _ -> errMsg pos "Invalid pattern: disallowed usage of the bottom element in patterns."
  Int _ -> Ok (Dom pos IntDom)
  Str _ -> Ok (Dom pos StrDom)
  Sym _ -> Ok (Dom pos SymDom)
  BConst _ -> Ok (Dom pos BoolDom)

confT :: DomEnv -> TagTable -> TEnv -> Con -> Dom -> Err TEnv
confT de tt env = let
  this = confT de tt env
  in \c@(Con pos c') -> case c' of
  ECon -> \case
    Dom _ EDom -> Ok env
    d -> errMsg pos ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show EDom ++ " but found " ++ show d ++ " instead.")
  ConstCon c -> \d -> getConst c >>= \d' -> if deq de d d' then Ok env
    else errMsg pos ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show d ++ ", but found " ++ show d' ++ " instead.")
  VarCon x -> \case
    Dom _ EDom -> errMsg pos "Invalid pattern: cannot use variable in patterns when the expected domain is empty."
    d -> Ok (Data.Map.insert x d env)
  TagCon t c -> case Data.Map.lookup t tt of
    Nothing -> \_ -> errMsg pos ("Invalid pattern: unrecognized tag " ++ t ++ ".")
    Just (x', d') -> \case
      Dom _ (VarDom x) -> if x == x' then this c d'
        else errMsg pos ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ x ++ ", but found " ++ x' ++ " instead.")
      d -> errMsg pos ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show d ++ ", but found " ++ x' ++ " instead.")
  PairCon c1 c2 -> \case
    Dom _ (ProdDom d1 d2) -> this c1 d1 >>= \env' -> confT de tt env' c2 d2 >>= Ok
    d -> errMsg pos ("Invalid pattern: mismatched domains between pattern and transition system. Expected " ++ show d ++ ", but found a product domain instead.")