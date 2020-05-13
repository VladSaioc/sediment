module Semantics.Exp.TypeS (expT) where

import Data.Map
import Text.Show

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Dom.General
import Semantics.Dom.Equivalence
import Semantics.Exp.General

getConst :: Const -> Err Dom
getConst (Bot d) = Ok d
getConst (Int _) = Ok IntDom
getConst (Str _) = Ok StrDom
getConst (Sym _) = Ok SymDom
getConst (BConst _) = Ok BoolDom

expT :: DomEnv -> TagTable -> TEnv -> Exp -> Err Dom
-- Constants and variables
expT de tt env EExp = Ok EDom
expT de tt env (ConstE c) = getConst c
expT de tt env (Var x) = case Data.Map.lookup x env of
  Nothing -> Bad ("Undeclared variable: could not find variable " ++ x ++ " in scope.")
  Just ty -> Ok ty
-- Arithmetic expressions
expT de tt env (Plus e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [IntDom, IntDom] -> Ok IntDom
  Ok [d1, d2] -> Bad ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
expT de tt env (Minus e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [IntDom, IntDom] -> Ok IntDom
  Ok [d1, d2] -> Bad ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
expT de tt env (Prod e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [IntDom, IntDom] -> Ok IntDom
  Ok [d1, d2] -> Bad ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
expT de tt env (Div e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [IntDom, IntDom] -> Ok IntDom
  Ok [d1, d2] -> Bad ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
expT de tt env (Mod e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [IntDom, IntDom] -> Ok IntDom
  Ok [d1, d2] -> Bad ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
expT de tt env (Inverse e) = case expT de tt env e of
  Bad msg -> Bad msg
  Ok IntDom -> Ok IntDom
  Ok d -> Bad ("Incompatible types: arithmetic expression evaluated with type " ++ show d ++ ".")
-- Boolean expressions
expT de tt env (Neg e) = case expT de tt env e of
  Bad msg -> Bad msg
  Ok BoolDom -> Ok BoolDom
  Ok d -> Bad ("Incompatible types: boolean expression evaluated with type " ++ show d ++ ".")
expT de tt env (And e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [BoolDom, BoolDom] -> Ok BoolDom
  Ok [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
expT de tt env (Or e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [BoolDom, BoolDom] -> Ok BoolDom
  Ok [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
-- Relational expressions
expT de tt env (Equal e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [SymDom, SymDom] -> Ok BoolDom
  Ok [IntDom, IntDom] -> Ok BoolDom
  Ok [StrDom, StrDom] -> Ok BoolDom
  Ok [BoolDom, BoolDom] -> Ok BoolDom
  Ok [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
expT de tt env (NotEqual e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [SymDom, SymDom] -> Ok BoolDom
  Ok [IntDom, IntDom] -> Ok BoolDom
  Ok [StrDom, StrDom] -> Ok BoolDom
  Ok [BoolDom, BoolDom] -> Ok BoolDom
  Ok [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
expT de tt env (LessThan e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [SymDom, SymDom] -> Ok BoolDom
  Ok [IntDom, IntDom] -> Ok BoolDom
  Ok [StrDom, StrDom] -> Ok BoolDom
  Ok [BoolDom, BoolDom] -> Ok BoolDom
  Ok [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
expT de tt env (LessEqThan e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [SymDom, SymDom] -> Ok BoolDom
  Ok [IntDom, IntDom] -> Ok BoolDom
  Ok [StrDom, StrDom] -> Ok BoolDom
  Ok [BoolDom, BoolDom] -> Ok BoolDom
  Ok [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
expT de tt env (GreaterThan e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [SymDom, SymDom] -> Ok BoolDom
  Ok [IntDom, IntDom] -> Ok BoolDom
  Ok [StrDom, StrDom] -> Ok BoolDom
  Ok [BoolDom, BoolDom] -> Ok BoolDom
  Ok [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
expT de tt env (GreaterEqThan e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [SymDom, SymDom] -> Ok BoolDom
  Ok [IntDom, IntDom] -> Ok BoolDom
  Ok [StrDom, StrDom] -> Ok BoolDom
  Ok [BoolDom, BoolDom] -> Ok BoolDom
  Ok [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
-- String expressions
expT de tt env (Concat e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [StrDom, StrDom] -> Ok StrDom
  Ok [d1, d2] -> Bad ("Incompatible types: concatenation expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
-- Simple \-calculus
expT de tt env (Lambda d x e) = case deepDomain de d of
  FuncDom d1 d2 -> let
      env' = Data.Map.insert x d1 env
    in case expT de tt env' e of
      Bad msg -> Bad msg
      Ok d2' -> if deq de (d2, d2') then Ok d
        else Bad ("Incompatible types: lambda-expression return type " ++ show d2 ++ " incompatible with the evaluated expression type " ++ show d2' ++ " of the body.")
  _ -> Bad ("Incompatible types: lambda-expression declared with incopatible type " ++ show d ++ ".")
expT de tt env (App e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [d1, d2] -> case deepDomain de d1 of
    FuncDom d2' d ->  if deq de (d2', d2) then Ok d
      else Bad ("Incompatible types: supplied expression with type " ++ show d2 ++ " as input to function with type " ++ show d1 ++ ".")
    _ -> Bad ("Incompatible types: non-lambda-expression used as a function. Found domain " ++ show d1 ++ " instead.")
-- Extended \-calculus
expT de tt env (Let x e1 e2) = case expT de tt env e1 of
  Bad msg -> Bad msg
  Ok d -> let
      env' = Data.Map.insert x d env
    in expT de tt env' e2
expT de tt env (Letr d x e1 e2) = let
    env'' = Data.Map.insert x d env
  in case results [expT de tt env'' e1, expT de tt env'' e2] of
    Bad msg -> Bad msg
    Ok [d1, d2] -> if deq de (d, d1) then Ok d2
      else Bad ("Incompatible letrec definition: recursive variable " ++ x ++ " declared with domain " ++ show d ++ " but found type " ++ show d1 ++ " instead.")
expT de tt env (If e1 e2 e3) = case results [expT de tt env e1, expT de tt env e2, expT de tt env e3] of
  Bad msg -> Bad msg
  Ok [BoolDom, d, d'] -> if deq de (d, d') then Ok d
    else Bad ("Incompatible conditional branches: if statement branches are of domains " ++ show d ++ " and " ++ show d' ++ ". Expected the same type.")
  Ok (d : _) -> Bad ("Incompatible conditional statement: expected to find `bool`, found " ++ show d ++ " instead.")
expT de tt env (Update e1 e2 e3) = case results [expT de tt env e1, expT de tt env e2, expT de tt env e3] of
  Bad msg -> Bad msg
  Ok [d1, d2, d3] -> if deq de (d3, FuncDom d1 d2) then let
        isBasic de d = case deepDomain de d of
          IntDom -> True
          BoolDom -> True
          StrDom -> True
          SymDom -> True
          _ -> False
      in if not (isBasic de d1) then Bad ("Incompatible types: domain expression " ++ show d1 ++ " is not a basic domain, or basic domain alias.")
      else Ok d3
    else Bad ("Incompatible types: update failed. Expected type " ++ show d3 ++ ". Found type " ++ show (FuncDom d1 d2) ++ " instead.")
-- Operations on pairs
expT de tt env (Pair e1 e2) = case results [expT de tt env e1, expT de tt env e2] of
  Bad msg -> Bad msg
  Ok [d1, d2] -> Ok (ProdDom d1 d2)
expT de tt env (Head e) = case expT de tt env e of
  Bad msg -> Bad msg
  Ok (ProdDom d _) -> Ok d
  Ok d -> Bad ("Incompatible head operation: expected a pair domain, found " ++ show d ++ " instead.")
expT de tt env (Tail e) = case expT de tt env e of
  Bad msg -> Bad msg
  Ok (ProdDom _ d) -> Ok d
  Ok d -> Bad ("Incompatible tail operation: expected a pair domain, found " ++ show d ++ " instead.")
-- Operations on tags
expT de tt env (Inject t e) = case Data.Map.lookup t tt of
  Nothing -> Bad ("Invalid tag injection: unrecognized tag " ++ t ++ ".")
  Just (x, d) -> case expT de tt env e of
    Bad msg -> Bad msg
    Ok d' -> if deq de (d, d') then Ok (VarDom x)
      else Bad ("Invalid tag injection: mismatched expression type. Expected " ++ show d ++ ". Found " ++ show d' ++ " instead.")
expT de tt env (Project e t) = case Data.Map.lookup t tt of
  Nothing -> Bad ("Invalid tag projection: unrecognized tag " ++ t ++ ".")
  Just (x, d) -> case expT de tt env e of
    Bad msg -> Bad msg
    Ok (VarDom x') -> if x == x' then case d of
        EDom -> Bad ("Invalid tag projection: tag " ++ t ++ " is domain-less, hence it cannot be projected upon.")
        _ -> Ok d
      else Bad ("Invalid tag projection: mismatched expression type for projecting on " ++ t ++ ". Expected " ++ x ++ ". Found " ++ x' ++ " instead.")
    Ok d -> Bad ("Invalid tag projection: expected union domain " ++ x ++ ". Found " ++ show d ++ " instead.")
expT de tt env (IsTag e t) = case Data.Map.lookup t tt of
  Nothing -> Bad ("Invalid tag projection: unrecognized tag " ++ t ++ ".")
  Just (x, _) -> case expT de tt env e of
    Bad msg -> Bad msg
    Ok (VarDom x') -> if x == x' then Ok BoolDom
      else Bad ("Invalid tag check: expected union domain " ++ x ++ " when checking for tag " ++ t ++ ". Found " ++ x' ++ " instead.")
    Ok d -> Bad ("Invalid tag check: expected union domain " ++ x ++ " when checking for tag " ++ t ++ ". Found " ++ show d  ++ " instead.")