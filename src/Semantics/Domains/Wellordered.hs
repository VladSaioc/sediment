module Semantics.Domains.Wellordered (wellordered) where

import Data.Map
import Data.Set
import Syntax.Ast
import Semantics.Env
import Semantics.General
import Semantics.Domains.General

type VisitedTags = Set String

wellordered :: DomEnv -> [String] -> Result ()
wellordered de xs = let
    results = Prelude.map (\x -> case wellordered' de Data.Set.empty (VarDom x) of
      Bad msg -> Bad msg
      _ -> Ok ()) xs
  in
    Prelude.foldl (\r r' -> case r' of
      Ok{} -> r
      Bad msg' -> case r of
        Bad msg -> Bad (msg ++ "\n" ++ msg')
        _ -> Bad msg') (Ok ()) results

wellordered' :: DomEnv -> VisitedTags -> Dom -> Result ()
wellordered' de ts d = case d of
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
      UnionDom tds -> wellordered'' de ts x tds
      _ -> wellordered' de ts d'

wellordered'' :: DomEnv -> VisitedTags -> String -> [(String, Dom)] -> Result ()
wellordered'' de ts x [] = Bad ("Unions must be well-ordered: no well-ordered branch found for union " ++ x ++ ".")
wellordered'' de ts x ((t, d) : tds) = if Data.Set.member t ts then
    let
      r = wellordered' de (Data.Set.insert t ts) d
    in case r of
      Bad msg -> wellordered'' de ts x tds
      Ok{} -> Ok ()
  else wellordered'' de ts x tds