module Semantics.Dom.Wellformed (wellformed) where

import Data.Map
import Data.Set
import Data.Maybe

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Dom.General

type Parents = Map String (Set String)
type Host = String

wellformed :: DomEnv -> [PosVar] -> Err Parents
wellformed de = let
    forEach pj (x, _) = let
        Just d = Data.Map.lookup x de
      in case pj of
          Ok p -> wellformed' de x p d
          Bad msg -> Bad msg
  in Prelude.foldl forEach (Ok Data.Map.empty)

wellformed' :: DomEnv -> Host -> Parents -> Dom -> Err Parents
wellformed' de x p (Dom pos d) = case d of
  FuncDom d1 d2 -> wellformed' de x p d1 >>= \p' -> wellformed' de x p' d2
  ProdDom d1 d2 -> wellformed' de x p d1 >>= \p' -> wellformed' de x p' d2
  VarDom x' -> if x' == x then errMsg pos ("Disallowed non-union self-reference: domain " ++ x' ++ " references itself in its definition.")
    else do
      let Just d' = Data.Map.lookup x' de
      case d' of
        Dom _ UnionDom{} -> Ok p
        _ -> let
            px = Data.Maybe.fromMaybe Data.Set.empty (Data.Map.lookup x p)
          in if Data.Set.member x' px then
            errMsg pos ("Disallowed non-union circular references: domain variables " ++ x' ++ " and " ++ x ++ " create a circular reference.")
          else let
            px' = Data.Maybe.fromMaybe Data.Set.empty (Data.Map.lookup x' p)
            p' = let
                pnew = px `Data.Set.union` (px' `Data.Set.union` Data.Set.singleton x)
              in Data.Map.insert x' pnew p
          in wellformed' de x' p' d'
  _ -> Ok p  