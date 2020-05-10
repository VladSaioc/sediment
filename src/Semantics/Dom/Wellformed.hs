module Semantics.Dom.Wellformed (wellformed) where

import Data.Map
import Data.Set

import Syntax.Ast
import Syntax.ErrM

import Semantics.Dom.General

type Parents = Map String (Set String)
type Host = String

wellformed :: DomEnv -> [String] -> Err Parents
wellformed de = let
    p0 = Ok Data.Map.empty
    forEach pj x = let
        Just d = Data.Map.lookup x de
      in
        case pj of
          Ok p -> wellformed' de x p d
          Bad msg -> Bad msg
  in
    Prelude.foldl forEach p0

wellformed' :: DomEnv -> Host -> Parents -> Dom -> Err Parents
wellformed' de x p d = case d of
  IntDom -> Ok p
  BoolDom -> Ok p
  StrDom -> Ok p
  SymDom -> Ok p
  EDom -> Ok p
  FuncDom d1 d2 -> let
      pr1 = wellformed' de x p d1
    in case pr1 of
      Ok p' -> wellformed' de x p' d2
      Bad msg -> Bad msg
  ProdDom d1 d2 -> let
      pr1 = wellformed' de x p d1
    in case pr1 of
      Ok p' -> wellformed' de x p' d2
      Bad msg -> Bad msg
  VarDom x' -> if x' == x then Bad ("Disallowed non-union self-reference: domain " ++ x' ++ " references itself in its definition.")
    else let 
      Just d' = Data.Map.lookup x' de
      in case d' of
        UnionDom{} -> Ok p
        _ -> let
            Just px = Data.Map.lookup x p
            isParent = Data.Set.member x' px
          in
            if isParent then Bad ("Disallowed non-union circular references: domain variables " ++ x' ++ " and " ++ x ++ " create a circular reference.")
            else let
              Just px' = Data.Map.lookup x' p
              p' = Data.Map.insert x' (px `Data.Set.union` px') p
            in
              wellformed' de x' p' d'
  