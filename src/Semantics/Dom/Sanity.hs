module Semantics.Dom.Sanity (
  uniqueTags,
  unionAliasCheck,
  domDeclCheck
) where

import Data.Set
import Data.Map
import Data.List

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Dom.General

uniqueTags :: DomEnv -> [String] -> Err (Set String)
uniqueTags de = Prelude.foldl (uniqueTags' de) (Ok Data.Set.empty)

uniqueTags' :: DomEnv -> Err (Set String) -> String -> Err (Set String)
uniqueTags' de (Ok s) x = let
    Just (UnionDom tds) = Data.Map.lookup x de
    ts = Data.Set.fromList (Prelude.map fst tds)
  in if Data.Set.size ts /= length tds then Bad ("Duplicate tags: domain " ++ x ++ " uses the same tag for multiple definitions.")
    else if not (disjoint ts s) then
      let
        duplicates = Data.Set.toList (Data.Set.intersection ts s)
        listOfTags = intercalate ", " duplicates
      in
        Bad ("The following tags in domain " ++ x ++ " are duplicated from other domains: " ++ listOfTags)
    else Ok (ts `Data.Set.union` s)
uniqueTags' _ err _ = err

unionAliasCheck :: DomEnv -> [String] -> Err ()
unionAliasCheck de = let
    forEach b x = case b of
      Ok{} -> if isUnion de (VarDom x) then Bad ("Disallowed union aliasing: domain " ++ x ++ " is a union alias.")
        else Ok ()
      _ -> b
  in Prelude.foldl forEach (Ok ())

domDeclCheck :: DomEnv -> Spec -> Err ()
domDeclCheck de (Spec dfs _) = case results (Prelude.map (ddc de) dfs) of
  Bad msg -> Bad msg
  Ok _ -> Ok ()

ddc :: DomEnv -> Df -> Err ()
ddc de (DomDf _ d) = ddcd de d
ddc de (DataDf _ e) = ddce de e
ddc de (DataRecDf d _ e) = case results [ddcd de d, ddce de e] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddc de (TSysDf (TDom d1 d2 d3) _ rs) = case results ([
    ddcd de d1,
    ddcd de d2,
    ddcd de d3
  ] ++ Prelude.map (ddcr de) rs) of
  Bad msg -> Bad msg
  Ok _ -> Ok ()

ddcd :: DomEnv -> Dom -> Err ()
ddcd de (VarDom x) = case Data.Map.lookup x de of
  Nothing -> Bad ("Undeclared variable: domain variable " ++ x ++ " not in the scope of the specification")
  Just _ -> Ok ()
ddcd de (UnionDom ds) = Prelude.foldl (\r (_,d) -> case r of
  Bad msg -> Bad msg
  Ok () -> case ddcd de d of
    Bad msg -> Bad msg
    Ok () -> Ok ()) (Ok ()) ds
ddcd de (FuncDom d1 d2) = case results [ddcd de d1, ddcd de d2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddcd de (ProdDom d1 d2) = case results [ddcd de d1, ddcd de d2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddcd de _ = Ok ()

ddce :: DomEnv -> Exp -> Err ()
ddce de (Lambda d _ e) = case results [ddcd de d, ddce de e] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (App e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Let _ e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Letr d _ e1 e2) = case results [ddcd de d, ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (If e1 e2 e3) = case results [ddce de e1, ddce de e2, ddce de e3] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Update e1 e2 e3) = case results [ddce de e1, ddce de e2, ddce de e3] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Concat e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Inverse e) = ddce de e
ddce de (Plus e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Minus e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Prod e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Div e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Mod e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Neg e) = ddce de e
ddce de (Or e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (And e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Equal e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (NotEqual e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (LessThan e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (LessEqThan e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (GreaterThan e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (GreaterEqThan e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Pair e1 e2) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()
ddce de (Head e) = ddce de e
ddce de (Tail e) = ddce de e
ddce de (Project e _) = ddce de e
ddce de (Inject _ e) = ddce de e
ddce de (IsTag e _) = ddce de e
ddce de _ = Ok ()

ddcr :: DomEnv -> Rule -> Err ()
ddcr de (Rule _ _ _ e prs) = case results (ddce de e : Prelude.map (ddcp de) prs) of
  Bad msg -> Bad msg
  Ok _ -> Ok ()

ddcp :: DomEnv -> Pr -> Err ()
ddcp de (IfPr e) = ddce de e
ddcp de (LetPr _ e) = ddce de e
ddcp de (LetrPr d _ e) = case results [ddcd de d, ddce de e] of
  Ok _ -> Ok ()
ddcp de (TrPr e1 e2 _ _) = case results [ddce de e1, ddce de e2] of
  Bad msg -> Bad msg
  Ok _ -> Ok ()