module Syntax.GetAst (getAst) where

import Syntax.AbsSediment
import Syntax.ParSediment
import Syntax.ErrM
import Syntax.Ast

epos = (-1, -1)

pIdent :: PIdent -> PosVar
pIdent (PIdent (pos,s)) = (s,pos)
pLabel :: Label -> PosVar
pLabel (Label (pos,l)) = let
    label = drop 2 (take (length l - 2) l)
    trimFirst l = if head l == ' ' then trimFirst (drop 1 l)
        else l
    trimLast l = if l!!(length l - 1) == ' ' then trimLast (take (length l - 1) l)
        else l
  in (trimFirst (trimLast label), pos)
pSymbol :: Symbol -> Const
pSymbol (Symbol (pos,s)) = Const pos (Sym (drop 1 (take (length s - 1) s)))
pConsStr :: PConstructor -> String
pConsStr (PConstructor (_,s)) = drop 1 (take (length s - 1) s)

getRawAst s =
  let
    tokens = myLexer s
    ast = pSpecification_ tokens
  in ast

polishAst = \case
  Ok (Spec_ dfs evs) -> Spec
    (map pDef dfs)
    (map pEval evs)
  Bad err -> SpecError err

getAst = polishAst . getRawAst

-- Definition polish
pDef :: Def_ -> Df
pDef = \case
  DomDef_ i' de -> 
    let (i, pos) = pIdent i'
    in Df pos (DomDf i (pDomDefExp pos de))
  SyntaxDef_ i' frs ->
    let (i, pos) = pIdent i'
    in Df pos (DomDf i (Dom pos (UnionDom (map pFormRule frs) True)))
  TSystem_ i' td rs ->
    let (i, pos) = pIdent i'
    in Df pos (TSysDf (pTDom td) i (map pRule rs))
  Data_ c' e ->
    let Con pos c = pConfig c'
    in Df pos (DataDf (Con pos c) (pExp e))
  DataRec_ i1' d i2' e ->
    let (i1, pos) = pIdent i1'
    in let (i2, _) = pIdent i2'
    in Df pos (DataRecDf (pDom d) i1 i2 (pExp e))

-- Domain definition polish
pDomDefExp :: Pos -> DomDefExp_ -> Dom
pDomDefExp pos = \case
  DefUnion_ (UnionDom_ ubs) -> Dom pos (UnionDom (map pUnionBranch ubs) False)
  DefNonUnion_ d -> pDom d

-- -- Union branch polish
pUnionBranch :: UnionBranch_ -> (String, Dom)
pUnionBranch = \case
  TagDom_ i d -> (fst (pIdent i), pDom d)
  BareTagDom_ i' ->
    let (i, pos) = pIdent i'
    in (i, Dom pos EDom)

-- -- Non-union domain polish
pDom :: Dom_ -> Dom
pDom = \case
  StrDom_ (PString (pos, _)) -> Dom pos StrDom
  BoolDom_ (PBool (pos, _)) -> Dom pos BoolDom
  SymDom_ (PSymbol (pos, _)) -> Dom pos SymDom
  IntDom_ (PInt (pos, _)) -> Dom pos IntDom
  VarDom_ i' ->
    let (i, pos) = pIdent i'
    in Dom pos (VarDom i)
  FuncDom_ d1 (PArrow (pos, _)) d2 -> Dom pos (FuncDom (pDom d1) (pDom d2))
  ProdDom_ d1 (PStar (pos, _)) d2 -> Dom pos (ProdDom (pDom d1) (pDom d2))

-- Syntax definition polish
pFormRule :: FormRule_ -> (String, Dom)
pFormRule = \case
  FormRuleAtoms_ as -> (pFormRuleCons as, pFormRuleDom as)

pFormRuleCons :: [FormRuleAtom_] -> String
pFormRuleCons as =
  let str = (\case {
    [] -> [];
    FormRuleCons_ p : as -> pConsStr p : str as;
    FormRuleDom_ _ : as -> "_" : str as })
  in unwords (str as)

pFormRuleDom :: [FormRuleAtom_] -> Dom
pFormRuleDom = \case
  [] -> Dom epos EDom
  FormRuleCons_ _ : as -> pFormRuleDom as
  FormRuleDom_ d_ : as -> case pFormRuleDom as of
    Dom _ EDom -> pDom d_
    d' -> let Dom pos d = pDom d_ in Dom pos (ProdDom (Dom pos d) d')

-- Transition system domain polish
pTDom :: TDom_ -> TDom
pTDom = \case
  BETDom_ d1 d2 d3 -> TDom (pDom d1) (pDom d2) (pDom d3)
  NBETDom_ d1' d2 ->
    let Dom pos d1 = pDom d1'
    in TDom (Dom pos EDom) (Dom pos d1) (pDom d2)

-- Transition system polish
-- -- Rule polish
pRule :: Rule_ -> Rule
pRule = \case
  BEAxiom_ i c1 c2 e -> Rule (pLabel i) (pConfig c1) (pConfig c2) (pExp e) []
  NBEAxiom_ i c e ->
    let (label, pos) = pLabel i
    in Rule (label, pos) (Con pos ECon) (pConfig c) (pExp e) []
  BERule_ i c1 c2 e ps -> Rule (pLabel i) (pConfig c1) (pConfig c2) (pExp e) (map pPremise ps)
  NBERule_ i c e ps ->
    let (label, pos) = pLabel i
    in Rule (label, pos) (Con pos ECon) (pConfig c) (pExp e) (map pPremise ps)

-- -- Premise polish
pPremise :: Premise_ -> Pr
pPremise = \case
  IfPremise_ (PIf (pos, _)) e -> Pr pos (IfPr (pExp e))
  LetPremise_ c' e ->
    let Con pos c = pConfig c'
    in Pr pos (LetPr (Con pos c) (pExp e))
  LetrecPremise_ i1' d i2' e ->
    let (i1, pos) = pIdent i1'
    in let (i2, _) = pIdent i2'
    in Pr pos (LetrPr (pDom d) i1 i2 (pExp e))
  BETrInternalPremise_ e1' e2 c ->
    let Exp pos e1 = pExp e1'
    in Pr pos (TrPr (Exp pos e1) (pExp e2) "/" (pConfig c))
  BETrExternalPremise_ e1' e2 i' c ->
    let Exp pos e1 = pExp e1'
    in let (i, _) = pIdent i'
    in Pr pos (TrPr (Exp pos e1) (pExp e2) i  (pConfig c))
  NBETrInternalPremise_ e' c ->
    let Exp pos e = pExp e'
    in Pr pos (TrPr (Exp epos EExp) (Exp pos e) "/" (pConfig c))
  NBETrExternalPremise_ e' i' c ->
    let Exp pos e = pExp e'
    in let (i, _) = pIdent i'
    in Pr pos (TrPr (Exp epos EExp) (Exp pos e) i (pConfig c))

-- -- Configuration polish
pConfig :: Config_ -> Con
pConfig = \case
  BareTagConfig_ i' ->
    let (i, pos) = pIdent i'
    in Con pos (TagCon i (Con pos ECon))
  VarConfig_ i' ->
    let (i, pos) = pIdent i'
    in Con pos (VarCon i)
  ConstConfig_ c' ->
    let Const pos c = pConst c'
    in Con pos (ConstCon (Const pos c))
  TagConfig_ i' c ->
    let (i, pos) = pIdent i'
    in Con pos (TagCon i (pConfig c))
  PairConfig_ c' cs ->
    let Con pos c = pConfig c'
    in Con pos (PairCon (Con pos c) (pConfigTreeify cs))
  SyntaxConfigTerm_ as -> pConfigSyntax as

pConfigSyntax :: [SyntaxConfigAtom_] -> Con
pConfigSyntax as = let Con pos c = pConfigSyntaxConf as
  in Con pos (TagCon (pConfigSyntaxCons as) (Con pos c))

pConfigSyntaxCons :: [SyntaxConfigAtom_] -> String
pConfigSyntaxCons as =
  let str = (\case {
    [] -> [];
    SyntaxConfigCons_ p : as -> pConsStr p : str as;
    SyntaxConfig_ _ : as -> "_" : str as })
  in unwords (str as)

pConfigSyntaxConf :: [SyntaxConfigAtom_] -> Con
pConfigSyntaxConf = \case
  [] -> Con epos ECon
  SyntaxConfigCons_ _ : as -> pConfigSyntaxConf as
  SyntaxConfig_ c_ : as -> case pConfigSyntaxConf as of
    Con _ ECon -> pConfig c_
    c -> let Con pos c' = pConfig c_ in Con pos (PairCon (Con pos c') c)

pConfigTreeify :: [Config_] -> Con
pConfigTreeify [c] = pConfig c
pConfigTreeify (c':cs) =
  let Con pos c = pConfig c'
  in Con pos (PairCon (Con pos c) (pConfigTreeify cs))

-- Expression polish
pExp :: Exp_ -> Exp
pExp = \case
  Var_ i' ->
    let (i, pos) = pIdent i'
    in Exp pos (Var i)
  ConstE_ c' ->
    let Const pos c = pConst c'
    in Exp pos (ConstE (Const pos c))
-- -- Basic \-calculus expressions
  Lambda_ i' d e ->
    let (i, pos) = pIdent i'
    in Exp pos (Lambda (pDom d) i (pExp e))
  App_ e1' e2 ->
    let Exp pos e1 = pExp e1'
    in Exp pos (App (Exp pos e1) (pExp e2))
-- -- Extended \-calculus expressions
  Let_ c' e1 e2 ->
    let Con pos c = pConfig c'
    in Exp pos (Let (Con pos c) (pExp e1) (pExp e2))
  Letrec_ i1' d i2' e1 e2 ->
    let (i1, pos) = pIdent i1'
    in let (i2, _) = pIdent i2'
    in Exp pos (Letr (pDom d) i1 i2 (pExp e1) (pExp e2))
  If_ (PIf (pos, _)) e1 e2 e3 -> Exp pos (If (pExp e1) (pExp e2) (pExp e3))
  Update_ e1 e2 (PArrow (pos, _)) e3 -> Exp pos (Update (pExp e2) (pExp e3) (pExp e1))
-- -- String expressions
  Concat_ e1 (PConcat (pos, _)) e2 -> Exp pos (Concat (pExp e1) (pExp e2))
-- -- Arithmetic
  Inverse_ (PMinus (pos, _)) e -> Exp pos (Inverse (pExp e))
  Plus_ e1 e2' ->
    let Exp pos e2 = pExp e2'
    in Exp pos (Plus (pExp e1) (Exp pos e2))
  Minus_ e1 (PMinus (pos, _)) e2 -> Exp pos (Minus (pExp e1) (pExp e2))
  Prod_ e1 (PStar (pos, _)) e2 -> Exp pos (Prod (pExp e1) (pExp e2))
  Div_ e1 (PDiv (pos, _)) e2 -> Exp pos (Div (pExp e1) (pExp e2))
  Mod_ e1 (PMod (pos, _)) e2 -> Exp pos (Mod (pExp e1) (pExp e2))
-- -- Relational
  Equal_ e1 (PEq (pos, _)) e2 -> Exp pos (Equal (pExp e1) (pExp e2))
  NotEqual_ e1 (PNeq (pos, _)) e2 -> Exp pos (NotEqual (pExp e1) (pExp e2))
  LessThan_ e1 (PLt (pos, _)) e2 -> Exp pos (LessThan (pExp e1) (pExp e2))
  LessEqThan_ e1 (PLe (pos, _)) e2 -> Exp pos (LessEqThan (pExp e1) (pExp e2))
  GreaterThan_ e1 (PGt (pos, _)) e2 -> Exp pos (GreaterThan (pExp e1) (pExp e2))
  GreaterEqThan_ e1 (PGe (pos, _)) e2 -> Exp pos (GreaterEqThan (pExp e1) (pExp e2))
-- -- Boolean
  And_ e1 (PAnd (pos, _)) e2 -> Exp pos (And (pExp e1) (pExp e2))
  Or_ e1 e2' ->
    let Exp pos e2 = pExp e2'
    in Exp pos (Or (pExp e1) (Exp pos e2))
  Neg_ (PNeg (pos, _)) e -> Exp pos (Neg (pExp e))
-- -- Tag and syntax operations
  BareTag_ i' ->
    let (i, pos) = pIdent i'
    in Exp pos (Inject i (Exp pos EExp))
  Inject_ i' e ->
    let (i, pos) = pIdent i'
    in Exp pos (Inject i (pExp e))
  SyntaxExpTerm_ as -> pExpSyntax as
  IsTag_ e (PIs (pos, _)) t -> Exp pos (IsTag (pExp e) (pTag t))
  Project_ e (PProj (pos, _)) t -> Exp pos (Project (pExp e) (pTag t))
-- -- Pair operations
  Head_ (PLeft (pos, _)) e -> Exp pos (Head (pExp e))
  Tail_ (PRight (pos, _)) e -> Exp pos (Tail (pExp e))
  Pair_ e' es ->
    let Exp pos e = pExp e'
    in Exp pos (Pair (Exp pos e) (pExpTreeify es))

-- -- Syntax expressions
pExpSyntax :: [SyntaxExpAtom_] -> Exp
pExpSyntax as = let Exp pos e = pExpSyntaxExp as
  in Exp pos (Inject (pExpSyntaxCons as) (Exp pos e))

pExpSyntaxCons :: [SyntaxExpAtom_] -> String
pExpSyntaxCons as =
  let str = (\case {
    [] -> [];
    SyntaxExpCons_ p : as -> pConsStr p : str as;
    SyntaxExp_ _ : as -> "_" : str as })
  in unwords (str as)

pExpSyntaxExp :: [SyntaxExpAtom_] -> Exp
pExpSyntaxExp = \case
  [] -> Exp epos EExp
  SyntaxExpCons_ _ : as -> pExpSyntaxExp as
  SyntaxExp_ e_ : as -> case pExpSyntaxExp as of
    Exp _ EExp -> pExp e_
    e -> let Exp pos e' = pExp e_ in Exp pos (Pair (Exp pos e') e)

pExpTreeify :: [Exp_] -> Exp
pExpTreeify [e] = pExp e
pExpTreeify (e':es) =
  let Exp pos e = pExp e'
  in Exp pos (Pair (Exp pos e) (pExpTreeify es))

-- Constant polish
pConst :: Const_ -> Const
pConst = \case
  Bot_ (PBot (pos, _)) d -> Const pos (Bot (pDom d))
  Int_ (PInteger (pos, i)) -> Const pos (Int (read i::Integer))
  Str_ (PosString (pos, s)) -> Const pos (Str s)
  Sym_ y -> pSymbol y
  BTrue_ (PTrue (pos, _)) -> Const pos (BConst True)
  BFalse_ (PFalse (pos, _)) -> Const pos (BConst False)

-- Tag polish
pTag :: Tag_ -> String
pTag = \case
  TagIdent_ t ->  fst (pIdent t)
  TagSyntax_ as -> pTagSyntax as

pTagSyntax :: [SyntaxTagAtom_] -> String
pTagSyntax as = 
  let str = (\case {
    [] -> [];
    SyntaxTagCons_ c : as -> pConsStr c : str as;
    SyntaxTagHole_ _ : as -> "_" : str as })
  in unwords (str as)

-- Evaluation polish
pEval :: Eval_ -> Ev
pEval = \case
  BEEval_ e1' e2 i' ->
    let Exp pos e1 = pExp e1'
    in let (i, _) = pIdent i'
    in EvP pos (Ev (Exp pos e1) (pExp e2) i)
  NBEEval_ e' i' ->
    let Exp pos e = pExp e'
    in let (i, _) = pIdent i'
    in EvP pos (Ev (Exp pos EExp) (Exp pos e) i)
  ExpEval_ e' ->
    let Exp pos e = pExp e'
    in EvP pos (ExpEv (Exp pos e))