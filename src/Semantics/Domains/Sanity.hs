module Semantics.Domains.Sanity (
  uniqueTags,
  unionAliasCheck
) where

import Data.Set
import Data.Map
import Data.List
import Syntax.Ast
import Semantics.Env
import Semantics.General
import Semantics.Domains.General

uniqueTags :: DomEnv -> [String] -> Result (Set String)
uniqueTags de = Prelude.foldl (uniqueTags' de) (Ok Data.Set.empty)

uniqueTags' :: DomEnv -> Result (Set String) -> String -> Result (Set String)
uniqueTags' de (Ok s) x = let
    Just (UnionDom tds) = Data.Map.lookup x de
    ts = Data.Set.fromList (Prelude.map fst tds)
  in if Data.Set.size ts /= length tds then Bad ("Domain " ++ x ++ " has duplicated tags.")
    else if not (disjoint ts s) then
      let
        duplicates = Data.Set.toList (Data.Set.intersection ts s)
        listOfTags = intercalate ", " duplicates
      in
        Bad ("The following tags in domain " ++ x ++ " are duplicated from other domains: " ++ listOfTags)
    else Ok (ts `Data.Set.union` s)
uniqueTags' _ err _ = err

unionAliasCheck :: DomEnv -> [String] -> Result ()
unionAliasCheck de = let
    forEach b x = case b of
      Ok{} -> case deepDomain de (VarDom x) of
        UnionDom{} -> Bad ("Disallowed union aliasing: domain " ++ x ++ " is a union alias.")
        _ -> Ok ()
      _ -> b
  in Prelude.foldl forEach (Ok ())