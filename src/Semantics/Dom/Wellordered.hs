module Semantics.Dom.Wellordered (wellordered) where

import Data.Map
import Data.Set

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Dom.General

type VisitedTags = Set String

wellordered :: DomEnv -> [PosVar] -> Err ()
wellordered de xs = let
    results = Prelude.map (\(x, pos) -> case wellordered' de Data.Set.empty (Dom pos (VarDom x)) of
      Bad msg -> Bad msg
      _ -> Ok ()) xs
  in
    Prelude.foldl (\r r' -> case r' of
      Ok{} -> r
      Bad msg' -> case r of
        Bad msg -> Bad (msg ++ "\n" ++ msg')
        _ -> Bad msg') (Ok ()) results

wellordered' :: DomEnv -> VisitedTags -> Dom -> Err ()
wellordered' de ts (Dom pos d) = case d of
  IntDom -> Ok ()
  BoolDom -> Ok ()
  StrDom -> Ok ()
  SymDom -> Ok ()
  EDom -> Ok ()
  FuncDom d1 d2 -> let
      r1 = wellordered' de ts d1
      r2 = wellordered' de ts d2
    in
      case (r1, r2) of
        (Bad msg1, Bad msg2) -> Bad (msg1 ++ "\n" ++ msg2)
        (Bad msg, _) -> Bad msg
        (_, Bad msg) -> Bad msg
        (_, _) -> Ok ()
  ProdDom d1 d2 -> let
      r1 = wellordered' de ts d1
      r2 = wellordered' de ts d2
    in
      case (r1, r2) of
        (Bad msg1, Bad msg2) -> Bad (msg1 ++ "\n" ++ msg2)
        (Bad msg, _) -> Bad msg
        (_, Bad msg) -> Bad msg
        (_, _) -> Ok ()
  VarDom x -> let
      Just d' = Data.Map.lookup x de
    in
      case d' of
      Dom _ (UnionDom tds _) -> wellordered'' de ts (x, pos) tds
      _ -> wellordered' de ts d'

wellordered'' :: DomEnv -> VisitedTags -> PosVar -> [(String, Dom)] -> Err ()
wellordered'' de ts (x, pos) [] = errMsg pos ("Unions must be well-ordered: no well-ordered branch found for " ++ x ++ ".")
wellordered'' de ts x ((t, d) : tds) = if not (Data.Set.member t ts) then
    let
      r = wellordered' de (Data.Set.insert t ts) d
    in case r of
      Bad msg -> wellordered'' de ts x tds
      Ok{} -> Ok ()
  else wellordered'' de ts x tds