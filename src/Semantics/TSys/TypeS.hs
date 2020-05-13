module Semantics.TSys.TypeS (
  tsysT
) where

import Data.Map

import Syntax.Ast
import Syntax.ErrM

import Semantics.General

import Semantics.Dom.Equivalence
import Semantics.Dom.General

import Semantics.Exp.General
import Semantics.Exp.TypeS

import Semantics.Conf.TypeS

import Semantics.TSys.General

tsysT :: DomEnv -> TagTable -> TTSEnv -> TEnv -> [Rule] -> Err [()]
tsysT de tt tenv env rs = results (Prelude.map (tsysT' de tt tenv env) rs)

tsysT' :: DomEnv -> TagTable -> TTSEnv -> TEnv -> Rule -> Err ()
tsysT' de tt tenv env (Rule label c1 c2 e prs) = let
    Just (TDom d1 d2 d3) = Data.Map.lookup thisTSys tenv
  in case confT de tt env c1 d1 of
    Bad msg -> Bad msg
    Ok env' -> case confT de tt env' c2 d2 of
      Bad msg -> Bad msg
      Ok env'' -> case prT de tt tenv env'' prs of
          Bad msg -> Bad msg
          Ok env''' -> case expT de tt env''' e of
            Bad msg -> Bad msg
            Ok _ -> Ok ()

prT :: DomEnv -> TagTable -> TTSEnv -> TEnv -> [Pr] -> Err TEnv
prT de tt tenv env = Prelude.foldl (prT' de tt tenv) (Ok env)

prT' :: DomEnv -> TagTable -> TTSEnv -> Err TEnv -> Pr -> Err TEnv
prT' de tt tenv (Bad msg) _ = Bad msg
prT' de tt tenv (Ok env) (IfPr e) = case expT de tt env e of
  Bad msg -> Bad msg
  Ok BoolDom -> Ok env
  Ok d -> Bad ("Incompatible conditional statment: expected to find `bool`. Found " ++ show d ++ " instead.")
prT' de tt tenv (Ok env) (LetPr x e) = case expT de tt env e of
  Bad msg -> Bad msg
  Ok d -> Ok (Data.Map.insert x d env)
prT' de tt tenv (Ok env) (LetrPr d x e) = case d of
  FuncDom d1 d2 -> let
      env' = Data.Map.insert x d env
    in case expT de tt env' e of
      Bad msg -> Bad msg
      Ok d' -> if deq de (d, d') then Ok env'
        else Bad ("Incompatible types: recursive variable " ++ x ++ " declared with domain " ++ show d ++ " but found type " ++ show d' ++ " instead.")
  _ -> Bad ("Incompatible letrec definition: expected a function dmain. Found " ++ show d ++ " instead.")
prT' de tt tenv (Ok env) (TrPr e1 e2 x c1) = case Data.Map.lookup x tenv of
  Nothing -> Bad ("Undeclared variable: could find transition system " ++ x ++ ".")
  Just (TDom d1 d2 d3) -> case results [expT de tt env e1, expT de tt env e2] of
    Bad msg -> Bad msg
    Ok [d1', d2'] -> if deq de (d1, d1') then
        if deq de (d2, d2') then confT de tt env c1 d3
        else Bad ("Incompatible types: expected domain " ++ show d2 ++ ". Found " ++ show d2' ++ " instead.")
      else Bad ("Incompatible types: expected domain " ++ show d1 ++ ". Found " ++ show d1' ++ " instead.")

