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

uniqueTags :: DomEnv -> [PosVar] -> Err (Set String)
uniqueTags de = Prelude.foldl (uniqueTags' de) (Ok Data.Set.empty)

uniqueTags' :: DomEnv -> Err (Set String) -> PosVar -> Err (Set String)
uniqueTags' de (Ok s) (x, pos) = let
    Just (Dom _ (UnionDom tds stx)) = Data.Map.lookup x de
    ts = Data.Set.fromList (Prelude.map fst tds)
  in if Data.Set.size ts /= length tds then errMsg pos (if stx
      then "Duplicate syntax constructors: abstract syntax definition " ++ x ++ " uses the same constructor for multiple formation rules. Constructors:\n\t"
        ++ show ts ++ "\n" ++ show tds
      else "Duplicate tags: domain " ++ x ++ " uses the same tag for multiple definitions.")
    else if not (disjoint ts s) then
      let
        duplicates = Data.Set.toList (Data.Set.intersection ts s)
        listOfTags = intercalate ", " duplicates
      in
        errMsg pos (if stx then "The following constructors in abstract syntax " ++ x ++ " are found as tags or constructors in other domain or abstract syntax definitions: " ++ listOfTags
          else "The following tags in union " ++ x ++ " are found as tags or constructors in other domain or abstract syntax definitions: " ++ listOfTags)
    else Ok (ts `Data.Set.union` s)
uniqueTags' _ err _ = err

unionAliasCheck :: DomEnv -> [PosVar] -> Err ()
unionAliasCheck de = let
    forEach b (x, pos) = case b of
      Ok{} -> if isUnion de (Dom pos (VarDom x)) then errMsg pos ("Disallowed union aliasing: domain " ++ x ++ " is a union alias.")
        else Ok ()
      _ -> b
  in Prelude.foldl forEach (Ok ())

domDeclCheck :: DomEnv -> Spec -> Err ()
domDeclCheck de (Spec dfs evs) = case results (Prelude.map (ddcdf de) dfs ++ Prelude.map (ddcev de) evs) of
  Bad msg -> Bad msg
  Ok _ -> Ok ()

ddcdf :: DomEnv -> Df -> Err ()
ddcdf de (Df _ df) = case df of
  DomDf _ d -> ddcd de d
  DataDf _ e -> ddce de e
  DataRecDf d _ _ e -> results [ddcd de d, ddce de e] >> Ok ()
  TSysDf (TDom d1 d2 d3) _ rs -> results ([
      ddcd de d1,
      ddcd de d2,
      ddcd de d3
    ] ++ Prelude.map (ddcr de) rs) >> Ok ()

ddcd :: DomEnv -> Dom -> Err ()
ddcd de (Dom pos d) = case d of
  VarDom x -> case Data.Map.lookup x de of
    Nothing -> errMsg pos ("Undeclared variable: domain variable " ++ x ++ " not in the scope of the specification")
    Just _ -> Ok ()
  UnionDom ds _ -> Prelude.foldl (\r (_,d) -> r >> ddcd de d >> Ok ()) (Ok ()) ds
  FuncDom d1 d2 -> results [ddcd de d1, ddcd de d2] >> Ok ()
  ProdDom d1 d2 -> results [ddcd de d1, ddcd de d2] >> Ok ()
  _ -> Ok ()

ddce :: DomEnv -> Exp -> Err ()
ddce de (Exp _ e) = case e of
  Lambda d _ e -> results [ddcd de d, ddce de e] >> Ok ()
  App e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  Let _ e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  Letr d _ _ e1 e2 -> results [ddcd de d, ddce de e1, ddce de e2] >> Ok ()
  If e1 e2 e3 -> results [ddce de e1, ddce de e2, ddce de e3] >> Ok ()
  Update e1 e2 e3 -> results [ddce de e1, ddce de e2, ddce de e3] >> Ok ()
  Concat e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  Inverse e -> ddce de e
  Plus e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  Minus e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  Prod e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  Div e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  Mod e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  Neg e -> ddce de e
  Or e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  And e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  Equal e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  NotEqual e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  LessThan e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  LessEqThan e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  GreaterThan e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  GreaterEqThan e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  Pair e1 e2 -> results [ddce de e1, ddce de e2] >> Ok ()
  Head e -> ddce de e
  Tail e -> ddce de e
  Project e _ -> ddce de e
  Inject _ e -> ddce de e
  IsTag e _ -> ddce de e
  _ -> Ok ()

ddcr :: DomEnv -> Rule -> Err ()
ddcr de (Rule _ _ _ e prs) = results (ddce de e : Prelude.map (ddcp de) prs) >> Ok ()

ddcp :: DomEnv -> Pr -> Err ()
ddcp de (Pr _ pr) = case pr of
  IfPr e -> ddce de e
  LetPr _ e -> ddce de e
  LetrPr d _ _ e -> results [ddcd de d, ddce de e] >> Ok ()
  TrPr e1 e2 _ _ -> results [ddce de e1, ddce de e2] >> Ok()

ddcev :: DomEnv -> Ev -> Err ()
ddcev de (EvP _ ev) = case ev of
  Ev e1 e2 _ -> results [ddce de e1, ddce de e2] >> Ok ()
  ExpEv e -> ddce de e
