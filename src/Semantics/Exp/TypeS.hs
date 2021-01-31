module Semantics.Exp.TypeS (expT) where

import Data.Map
import Text.Show

import Syntax.Ast
import Syntax.ErrM

import Semantics.General
import Semantics.Dom.General
import Semantics.Dom.Equivalence
import Semantics.Exp.General
import Semantics.Conf.TypeS

getConst :: Const -> Dom
getConst (Const pos c) = case c of
  Bot d -> d
  Int _ -> Dom pos IntDom
  Str _ -> Dom pos StrDom
  Sym _ -> Dom pos SymDom
  BConst _ -> Dom pos BoolDom

expT :: DomEnv -> TagTable -> TEnv -> Exp -> Err Dom
-- Constants and variables
expT de tt env (Exp pos e)= let
  rootDs = rootDomains de
  this = expT de tt env
  rootD d = Ok (rootDomain de d)
  in case e of
  EExp -> Ok (Dom pos EDom)
  ConstE c -> Ok (getConst c)
  Var x -> case Data.Map.lookup x env of
    Nothing -> errMsg pos ("Undeclared variable: could not find variable " ++ x ++ " in scope.")
    Just ty -> Ok ty
  -- Arithmetic expressions
  Plus e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ IntDom, Dom _ IntDom] -> Ok (Dom pos IntDom)
    [d1, d2] -> errMsg pos ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Minus e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ IntDom, Dom _ IntDom] -> Ok (Dom pos IntDom)
    [d1, d2] -> errMsg pos ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Prod e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ IntDom, Dom _ IntDom] -> Ok (Dom pos IntDom)
    [d1, d2] -> errMsg pos ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Div e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ IntDom, Dom _ IntDom] -> Ok (Dom pos IntDom)
    [d1, d2] -> errMsg pos ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Mod e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ IntDom, Dom _ IntDom] -> Ok (Dom pos IntDom)
    [d1, d2] -> errMsg pos ("Incompatible types: arithmetic expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Inverse e -> this e >>= rootD >>= \case
    Dom pos IntDom -> Ok (Dom pos IntDom)
    d -> errMsg pos ("Incompatible types: arithmetic expression evaluated with type " ++ show d ++ ".")
  -- Boolean expresssions
  Neg e -> this e >>= \case
    Dom _ BoolDom -> Ok (Dom pos BoolDom)
    d -> errMsg pos ("Incompatible types: boolean expression evaluated with type " ++ show d ++ ".")
  And e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ BoolDom, Dom _ BoolDom] -> Ok (Dom pos BoolDom)
    [d1, d2] -> errMsg pos ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  Or e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ BoolDom, Dom _ BoolDom] -> Ok (Dom pos BoolDom)
    [d1, d2] -> errMsg pos ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  -- Relational expressions
  Equal e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ SymDom, Dom _ SymDom] -> Ok (Dom pos BoolDom)
    [Dom _ IntDom, Dom _ IntDom] -> Ok (Dom pos BoolDom)
    [Dom _ StrDom, Dom _ StrDom] -> Ok (Dom pos BoolDom)
    [Dom _ BoolDom, Dom _ BoolDom] -> Ok (Dom pos BoolDom)
    [d1, d2] -> errMsg pos ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  NotEqual e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ SymDom, Dom _ SymDom] -> Ok (Dom pos BoolDom)
    [Dom _ IntDom, Dom _ IntDom] -> Ok (Dom pos BoolDom)
    [Dom _ StrDom, Dom _ StrDom] -> Ok (Dom pos BoolDom)
    [Dom _ BoolDom, Dom _ BoolDom] -> Ok (Dom pos BoolDom)
    [d1, d2] -> errMsg pos ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  LessThan e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ SymDom, Dom _ SymDom] -> Ok (Dom pos BoolDom)
    [Dom _ IntDom, Dom _ IntDom] -> Ok (Dom pos BoolDom)
    [Dom _ StrDom, Dom _ StrDom] -> Ok (Dom pos BoolDom)
    [Dom _ BoolDom, Dom _ BoolDom] -> Ok (Dom pos BoolDom)
    [d1, d2] -> errMsg pos ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  LessEqThan e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ SymDom, Dom _ SymDom] -> Ok (Dom pos BoolDom)
    [Dom _ IntDom, Dom _ IntDom] -> Ok (Dom pos BoolDom)
    [Dom _ StrDom, Dom _ StrDom] -> Ok (Dom pos BoolDom)
    [Dom _ BoolDom, Dom _ BoolDom] -> Ok (Dom pos BoolDom)
    [d1, d2] -> errMsg pos ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  GreaterThan e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ SymDom, Dom _ SymDom] -> Ok (Dom pos BoolDom)
    [Dom _ IntDom, Dom _ IntDom] -> Ok (Dom pos BoolDom)
    [Dom _ StrDom, Dom _ StrDom] -> Ok (Dom pos BoolDom)
    [Dom _ BoolDom, Dom _ BoolDom] -> Ok (Dom pos BoolDom)
    [d1, d2] -> errMsg pos ("Incompatible types: relational expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  GreaterEqThan e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ SymDom, Dom _ SymDom] -> Ok (Dom pos BoolDom)
    [Dom _ IntDom, Dom _ IntDom] -> Ok (Dom pos BoolDom)
    [Dom _ StrDom, Dom _ StrDom] -> Ok (Dom pos BoolDom)
    [Dom _ BoolDom, Dom _ BoolDom] -> Ok (Dom pos BoolDom)
    [d1, d2] -> errMsg pos ("Incompatible types: boolean expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  -- String expressions
  Concat e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ StrDom, Dom _ StrDom] -> Ok (Dom pos StrDom)
    [d1, d2] -> errMsg pos ("Incompatible types: concatenation expression evaluated with types " ++ show d1 ++ " and " ++ show d2 ++ ".")
  -- Simple \-calculus
  Lambda d x e -> let
      env' = Data.Map.insert x d env
      this = expT de tt env'
    in this e >>= \d' -> Ok (Dom pos (FuncDom d d'))
  App e1 e2 -> rootDs [this e1, this e2] >>= \case
    [Dom _ (FuncDom d2' d1), d2] -> if deq de d2' d2 then Ok d1
      else errMsg pos ("Incompatible types: supplied expression with type " ++ show d2 ++ " as input to function with type " ++ show (FuncDom d2' d1) ++ ".")
    [d1, d2] -> errMsg pos ("Incompatible types: non-lambda-expression used as a function. Found domain " ++ show d1 ++ " instead.")
  -- Extended \-calculus
  Let c e1 e2 -> this e1 >>= confT de tt env c >>= \env' -> expT de tt env' e2
  Letr d x x' e1 e2 -> case rootD d of
    Ok (Dom _ (FuncDom d1' d2')) -> let
        env' = Data.Map.insert x d env
        env'' = Data.Map.insert x' d1' env'
      in results [expT de tt env'' e1, expT de tt env'' e2] >>= \[d1, d2] -> if deq de d2' d1 then Ok d2
        else errMsg pos ("Incompatible letrec definition: recursive function " ++ x ++ " declared with output in domain " ++ show d2' ++ " but found " ++ show d1 ++ " .")
    _ -> errMsg pos ("Invalid letrec definition: expected recursive data to be expressed as a function type, but was instead was declared with domain " ++ show d ++ ".")
  If e1 e2 e3 -> rootDs [expT de tt env e1, expT de tt env e2, expT de tt env e3] >>= \case
    [Dom _ BoolDom, d2, d3] -> if deq de d2 d3 then Ok d2
      else errMsg pos ("Incompatible conditional branches: if statement branches are of domains " ++ show d2 ++ " and " ++ show d3 ++ ". Expected the same type.")
    (d : _) -> errMsg pos ("Incompatible conditional statement: expected to find Boolean guard, found " ++ show d ++ " instead.")
  Update e1 e2 e3 -> rootDs [this e1, this e2, this e3] >>= \case
    [d1, d2, Dom _ (FuncDom d1' d2')] -> if deq de (Dom pos (FuncDom d1 d2)) (Dom pos (FuncDom d1' d2')) then let
        isBasic = \case
          Dom _ IntDom -> True
          Dom _ BoolDom -> True
          Dom _ StrDom -> True
          Dom _ SymDom -> True
          _ -> False
        in if not (isBasic d1) then errMsg pos ("Incompatible types: domain expression " ++ show d1 ++ " is not a basic domain, or basic domain alias.")
        else Ok (Dom pos (FuncDom d1' d2'))
      else errMsg pos ("Incompatible types: invalid function update. Expected a function with type " ++ show (FuncDom d1' d2') ++ ". Found " ++ show (FuncDom d1 d2) ++ " instead.")
    [_, _, d] -> errMsg pos ("Incompatible types: invalid function update. Expected a function domain. Found " ++ show d ++ " instead.")
  -- Operations on pairs
  Pair e1 e2 -> rootDs [this e1, this e2] >>= \[d1, d2] -> Ok (Dom pos (ProdDom d1 d2))
  Head e -> this e >>= rootD >>= \case
    Dom _ (ProdDom d' _) -> Ok d'
    d -> errMsg pos ("Incompatible head operation: expected a pair domain, found " ++ show d ++ " instead.")
  Tail e -> this e >>= rootD >>= \case
    Dom _ (ProdDom _ d') -> Ok d'
    d -> errMsg pos ("Incompatible tail operation: expected a pair domain, found " ++ show d ++ " instead.")
  -- Operations on tags
  Inject t e -> case Data.Map.lookup t tt of
    Nothing -> errMsg pos ("Invalid tag injection: unrecognized tag " ++ t ++ ".")
    Just (x, d) -> this e >>= \d' -> if deq de d d' then Ok (Dom pos (VarDom x))
      else errMsg pos ("Invalid tag injection: mismatched expression type. Expected " ++ show d ++ ". Found " ++ show d' ++ " instead.")
  Project e t -> case Data.Map.lookup t tt of
    Nothing -> errMsg pos ("Invalid tag projection: unrecognized tag " ++ t ++ ".")
    Just (x, d) -> this e >>= \case
      Dom _ (VarDom x') -> if x == x' then case d of
          Dom _ EDom -> errMsg pos ("Invalid tag projection: tag " ++ t ++ " has an empty domain, hence it cannot be projected upon.")
          _ -> Ok d
        else errMsg pos ("Invalid tag projection: mismatched expression type for projecting on " ++ t ++ ". Expected " ++ x ++ ". Found " ++ x' ++ " instead.")
      d -> errMsg pos ("Invalid tag projection: expected union domain " ++ x ++ ". Found " ++ show d ++ " instead.")
  IsTag e t -> case Data.Map.lookup t tt of
    Nothing -> errMsg pos ("Invalid tag check: unrecognized tag " ++ t ++ ".")
    Just (x, _) -> this e >>= \case
      Dom _ (VarDom x') -> if x == x' then Ok (Dom pos BoolDom)
        else errMsg pos ("Invalid tag check: expected union domain " ++ x ++ " when checking for tag " ++ t ++ ". Found " ++ x' ++ " instead.")
      d -> errMsg pos ("Invalid tag check: expected union domain " ++ x ++ " when checking for tag " ++ t ++ ". Found " ++ show d  ++ " instead.")