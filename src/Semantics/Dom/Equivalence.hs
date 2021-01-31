module Semantics.Dom.Equivalence (deq) where

import Data.Map

import Syntax.Ast

import Semantics.Dom.General

deq :: DomEnv -> Dom -> Dom -> Bool
deq de d1@(Dom _ d1') d2@(Dom _ d2') = let
  this = deq de
  in case (d1', d2') of
  (EDom, EDom) -> True
  (IntDom, IntDom) -> True
  (BoolDom, BoolDom) -> True
  (StrDom, StrDom) -> True
  (SymDom, SymDom) -> True
  (FuncDom d1 d2, FuncDom d1' d2') -> this d1 d1' && this d2 d2'
  (ProdDom d1 d2, ProdDom d1' d2') -> this d1 d1' && this d2 d2'
  (VarDom x, d) -> let
      Just d' = Data.Map.lookup x de
    in case d' of
      Dom _ UnionDom{} -> case d of
        VarDom x' -> x == x'
        _ -> False
      _ -> this d' d2
  (d, VarDom x) -> let
      Just d' = Data.Map.lookup x de
    in case d' of
      Dom _ UnionDom{} -> case d of
        VarDom x' -> x == x'
        _ -> False
      _ -> this d1 d'
  _ -> False