module Semantics.Exp.TypeS (expT) where

import Data.Map
import Text.Show

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Dom.General
import Semantics.Dom.Equivalence
import Semantics.Exp.General

getConst :: Const -> Dom
getConst (Bot d) = d
getConst (Int _) = IntDom
getConst (Str _) = StrDom
getConst (Sym _) = SymDom
getConst (BConst _) = BoolDom

expT :: DomEnv -> TagTable -> TEnv -> Exp -> Err Dom
-- Constants and variables
expT de tt env = let
  rootDs = rootDomains de
  this = expT de tt env
  rootD d = Ok (rootDomain de d)
  in \case
  EExp -> Ok EDom
  ConstE c -> Ok (getConst c)
  Var x -> case Data.Map.lookup x env of
    Nothing -> Bad ("Undeclared variable: could not find variable " ++ x ++ " in scope.")
    Just ty -> Ok ty
  -- Arithmetic expressions
  Plus e1 e2 -> rootDs [this e1, this e2] >>= \case
    [IntDom, IntDom] -> Ok IntDom
    [d1, d2] -> Bad ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Minus e1 e2 -> rootDs [this e1, this e2] >>= \case
    [IntDom, IntDom] -> Ok IntDom
    [d1, d2] -> Bad ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Prod e1 e2 -> rootDs [this e1, this e2] >>= \case
    [IntDom, IntDom] -> Ok IntDom
    [d1, d2] -> Bad ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Div e1 e2 -> rootDs [this e1, this e2] >>= \case
    [IntDom, IntDom] -> Ok IntDom
    [d1, d2] -> Bad ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Mod e1 e2 -> rootDs [this e1, this e2] >>= \case
    [IntDom, IntDom] -> Ok IntDom
    [d1, d2] -> Bad ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Inverse e -> this e >>= rootD >>= \case
    IntDom -> Ok IntDom
    d -> Bad ("Incompatible types: arithmetic expression evaluated with type " ++ show d ++ ".")
  -- Boolean expresssions
  Neg e -> this e >>= \case
    BoolDom -> Ok BoolDom
    d -> Bad ("Incompatible types: boolean expression evaluated with type " ++ show d ++ ".")
  And e1 e2 -> rootDs [this e1, this e2] >>= \case
    [BoolDom, BoolDom] -> Ok BoolDom
    [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Or e1 e2 -> rootDs [this e1, this e2] >>= \case
    [BoolDom, BoolDom] -> Ok BoolDom
    [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  -- Relational expressions
  Equal e1 e2 -> rootDs [this e1, this e2] >>= \case
    [SymDom, SymDom] -> Ok BoolDom
    [IntDom, IntDom] -> Ok BoolDom
    [StrDom, StrDom] -> Ok BoolDom
    [BoolDom, BoolDom] -> Ok BoolDom
    [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  NotEqual e1 e2 -> rootDs [this e1, this e2] >>= \case
    [SymDom, SymDom] -> Ok BoolDom
    [IntDom, IntDom] -> Ok BoolDom
    [StrDom, StrDom] -> Ok BoolDom
    [BoolDom, BoolDom] -> Ok BoolDom
    [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  LessThan e1 e2 -> rootDs [this e1, this e2] >>= \case
    [SymDom, SymDom] -> Ok BoolDom
    [IntDom, IntDom] -> Ok BoolDom
    [StrDom, StrDom] -> Ok BoolDom
    [BoolDom, BoolDom] -> Ok BoolDom
    [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  LessEqThan e1 e2 -> rootDs [this e1, this e2] >>= \case
    [SymDom, SymDom] -> Ok BoolDom
    [IntDom, IntDom] -> Ok BoolDom
    [StrDom, StrDom] -> Ok BoolDom
    [BoolDom, BoolDom] -> Ok BoolDom
    [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  GreaterThan e1 e2 -> rootDs [this e1, this e2] >>= \case
    [SymDom, SymDom] -> Ok BoolDom
    [IntDom, IntDom] -> Ok BoolDom
    [StrDom, StrDom] -> Ok BoolDom
    [BoolDom, BoolDom] -> Ok BoolDom
    [d1, d2] -> Bad ("Incompatible types: relational expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  GreaterEqThan e1 e2 -> rootDs [this e1, this e2] >>= \case
    [SymDom, SymDom] -> Ok BoolDom
    [IntDom, IntDom] -> Ok BoolDom
    [StrDom, StrDom] -> Ok BoolDom
    [BoolDom, BoolDom] -> Ok BoolDom
    [d1, d2] -> Bad ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  -- String expressions
  Concat e1 e2 -> rootDs [this e1, this e2] >>= \case
    [StrDom, StrDom] -> Ok StrDom
    [d1, d2] -> Bad ("Incompatible types: concatenation expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  -- Simple \-calculus
  Lambda d x e -> let
      env' = Data.Map.insert x d env
      this = expT de tt env'
    in this e >>= \d' -> Ok (FuncDom d d')
  App e1 e2 -> rootDs [this e1, this e2] >>= \case
    [FuncDom d2' d1, d2] -> if deq de d2' d2 then Ok d1
      else Bad ("Incompatible types: supplied expression with type " ++ show d2 ++ " as input to function with type " ++ show (FuncDom d2' d1) ++ ".")
    [d1, d2] -> Bad ("Incompatible types: non-lambda-expression used as a function. Found domain " ++ show d1 ++ " instead.")
  -- Extended \-calculus
  Let x e1 e2 -> this e1 >>= \d -> let
      env' = Data.Map.insert x d env
    in expT de tt env' e2
  Letr d x x' e1 e2 -> case rootD d of
    Ok (FuncDom d1' d2') -> let
        env' = Data.Map.insert x d env
        env'' = Data.Map.insert x' d1' env'
      in results [expT de tt env'' e1, expT de tt env'' e2] >>= \[d1, d2] -> if deq de d2' d1 then Ok d2
        else Bad ("Incompatible letrec definition: recursive function " ++ x ++ " declared with output in domain " ++ show d2' ++ " but found " ++ show d1 ++ " .")
    _ -> Bad ("Invalid letrec definition: expected recursive data to be expressed as a function type, but was instead was declared with domain " ++ show d ++ ".")
  If e1 e2 e3 -> rootDs [expT de tt env e1, expT de tt env e2, expT de tt env e3] >>= \case
    [BoolDom, d2, d3] -> if deq de d2 d3 then Ok d2
      else Bad ("Incompatible conditional branches: if statement branches are of domains " ++ show d2 ++ " and " ++ show d3 ++ ". Expected the same type.")
    (d : _) -> Bad ("Incompatible conditional statement: expected to find `bool`, found " ++ show d ++ " instead.")
  Update e1 e2 e3 -> rootDs [this e1, this e2, this e3] >>= \case
    [d1, d2, FuncDom d1' d2'] -> if deq de (FuncDom d1 d2) (FuncDom d1' d2') then let
        isBasic = \case
          IntDom -> True
          BoolDom -> True
          StrDom -> True
          SymDom -> True
          _ -> False
        in if not (isBasic d1) then Bad ("Incompatible types: domain expression " ++ show d1 ++ " is not a basic domain, or basic domain alias.")
        else Ok (FuncDom d1' d2')
      else Bad ("Incompatible types: invalid function update. Expected a function with type " ++ show (FuncDom d1' d2') ++ ". Found " ++ show (FuncDom d1 d2) ++ " instead.")
    [_, _, d] -> Bad ("Incompatible types: invalid function update. Expected a function domain. Found " ++ show d ++ " instead.")
  -- Operations on pairs
  Pair e1 e2 -> rootDs [this e1, this e2] >>= \[d1, d2] -> Ok (ProdDom d1 d2)
  Head e -> this e >>= rootD >>= \case
    ProdDom d' _ -> Ok d'
    d -> Bad ("Incompatible head operation: expected a pair domain, found " ++ show d ++ " instead.")
  Tail e -> this e >>= rootD >>= \case
    ProdDom _ d' -> Ok d'
    d -> Bad ("Incompatible tail operation: expected a pair domain, found " ++ show d ++ " instead.")
  -- Operations on tags
  Inject t e -> case Data.Map.lookup t tt of
    Nothing -> Bad ("Invalid tag injection: unrecognized tag " ++ t ++ ".")
    Just (x, d) -> this e >>= \d' -> if deq de d d' then Ok (VarDom x)
      else Bad ("Invalid tag injection: mismatched expression type. Expected " ++ show d ++ ". Found " ++ show d' ++ " instead.")
  Project e t -> case Data.Map.lookup t tt of
    Nothing -> Bad ("Invalid tag projection: unrecognized tag " ++ t ++ ".")
    Just (x, d) -> this e >>= \case
      VarDom x' -> if x == x' then case d of
          EDom -> Bad ("Invalid tag projection: tag " ++ t ++ " is domain-less, hence it cannot be projected upon.")
          _ -> Ok d
        else Bad ("Invalid tag projection: mismatched expression type for projecting on " ++ t ++ ". Expected " ++ x ++ ". Found " ++ x' ++ " instead.")
      d -> Bad ("Invalid tag projection: expected union domain " ++ x ++ ". Found " ++ show d ++ " instead.")
  IsTag e t -> case Data.Map.lookup t tt of
    Nothing -> Bad ("Invalid tag check: unrecognized tag " ++ t ++ ".")
    Just (x, _) -> this e >>= \case
      VarDom x' -> if x == x' then Ok BoolDom
        else Bad ("Invalid tag check: expected union domain " ++ x ++ " when checking for tag " ++ t ++ ". Found " ++ x' ++ " instead.")
      d -> Bad ("Invalid tag check: expected union domain " ++ x ++ " when checking for tag " ++ t ++ ". Found " ++ show d  ++ " instead.")