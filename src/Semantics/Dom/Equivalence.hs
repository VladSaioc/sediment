module Semantics.Dom.Equivalence (deq) where

import Data.Map

import Syntax.Ast

import Semantics.Dom.General

deq :: DomEnv -> (Dom, Dom) -> Bool
deq de ds = case ds of
  (EDom, EDom) -> True
  (IntDom, IntDom) -> True
  (BoolDom, BoolDom) -> True
  (StrDom, StrDom) -> True
  (SymDom, SymDom) -> True
  (FuncDom d1 d2, FuncDom d1' d2') -> deq de (d1, d1') && deq de (d2, d2')
  (ProdDom d1 d2, ProdDom d1' d2') -> deq de (d1, d1') && deq de (d2, d2')
  (VarDom x, d) -> let
      Just d' = Data.Map.lookup x de
    in case d' of
      UnionDom{} -> case d of
        VarDom x' -> x == x'
        _ -> False
      _ -> deq de (d', d)
  (d, VarDom x) -> let
      Just d' = Data.Map.lookup x de
    in case d' of
      UnionDom{} -> case d of
        VarDom x' -> x == x'
        _ -> False
      _ -> deq de (d, d')
  _ -> False