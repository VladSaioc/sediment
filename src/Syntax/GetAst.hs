module Syntax.GetAst (getAst) where

import Syntax.AbsSediment
import Syntax.ParSediment
import Syntax.ErrM
import Syntax.Ast

pIdent :: Ident -> String
pIdent (Ident s) = s
pSymbol :: Symbol -> Const
pSymbol (Symbol s) = Sym (drop 1 (take (length s - 1) s))

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
  DomDef_ i de -> DomDf (pIdent i) (pDomDefExp de)
  SyntaxDef_ i frs -> DomDf (pIdent i) (UnionDom (map pFormRule frs))
  TSystem_ td i rs -> TSysDf (pTDom td) (pIdent i) (map pRule rs)
  Data_ i e -> DataDf (pIdent i) (pExp e)
  DataRec_ d i e -> DataRecDf (pDom d) (pIdent i) (pExp e)

-- Domain definition polish
pDomDefExp :: DomDefExp_ -> Dom
pDomDefExp = \case
  DefUnion_ (UnionDom_ ubs) -> UnionDom (map pUnionBranch ubs)
  DefNonUnion_ d -> pDom d

-- -- Union branch polish
pUnionBranch :: UnionBranch_ -> (String, Dom)
pUnionBranch = \case
  TagDom_ i d -> (pIdent i, pDom d)
  Tag_ i -> (pIdent i, EDom)

-- -- Non-union domain polish
pDom :: Dom_ -> Dom
pDom = \case
  StrDom_ -> StrDom
  BoolDom_ -> BoolDom
  SymDom_ -> SymDom
  IntDom_ -> IntDom
  VarDom_ i -> VarDom (pIdent i)
  FuncDom_ d1 d2 -> FuncDom (pDom d1) (pDom d2)
  ProdDom_ d1 d2 -> ProdDom (pDom d1) (pDom d2)

-- Syntax polish
pFormRule :: FormRule_ -> (String, Dom)
pFormRule = \case
  Term_ i d -> (pIdent i, pDom d)
  BareTerm_ i -> (pIdent i, EDom)

-- Transition system domain polish
pTDom :: TDom_ -> TDom
pTDom = \case
  BETDom_ d1 d2 d3 -> TDom (pDom d1) (pDom d2) (pDom d3)
  NBETDom_ d1 d2 -> TDom EDom (pDom d1) (pDom d2)

-- Transition system polish
-- -- Rule polish
pRule :: Rule_ -> Rule
pRule = \case
  BEAxiom_ i c1 c2 e -> Rule (pIdent i) (pConfig c1) (pConfig c2) (pExp e) []
  NBEAxiom_ i c e -> Rule (pIdent i) ECon (pConfig c) (pExp e) []
  BERule_ i c1 c2 e ps -> Rule (pIdent i) (pConfig c1) (pConfig c2) (pExp e) (map pPremise ps)
  NBERule_ i c e ps -> Rule (pIdent i) ECon (pConfig c) (pExp e) (map pPremise ps)

-- -- Premise polish
pPremise :: Premise_ -> Pr
pPremise = \case
  IfPremise_ e -> IfPr (pExp e)
  LetPremise_ i e -> LetPr (pIdent i) (pExp e)
  LetrecPremise_ d i e -> LetrPr (pDom d) (pIdent i) (pExp e)
  BETrInternalPremise_ e1 e2 c -> TrPr (pExp e1) (pExp e2) "/"  (pConfig c)
  BETrExternalPremise_ e1 e2 i c -> TrPr (pExp e1) (pExp e2) (pIdent i)  (pConfig c)
  NBETrInternalPremise_ e c -> TrPr EExp (pExp e) "/"  (pConfig c)
  NBETrExternalPremise_ e i c -> TrPr EExp (pExp e) (pIdent i)  (pConfig c)

-- -- Configuration polish
pConfig :: Config_ -> Con
pConfig = \case
  BareTagConfig_ i -> TagCon (pIdent i) ECon
  VarConfig_ i -> VarCon (pIdent i)
  ConstConfig_ c -> ConstCon (pConst c)
  TagConfig_ i c -> TagCon (pIdent i) (pConfig c)
  PairConfig_ c cs -> PairCon (pConfig c) (pConfigTreeify cs)
pConfigTreeify :: [Config_] -> Con
pConfigTreeify [c] = pConfig c
pConfigTreeify (c:cs) = PairCon (pConfig c) (pConfigTreeify cs)

-- Expression polish
pExp :: Exp_ -> Exp
pExp = \case
  Var_ i -> Var (pIdent i)
  ConstE_ c -> ConstE (pConst c)
-- -- Basic \-calculus expressions
  Lambda_ d i e -> Lambda (pDom d) (pIdent i) (pExp e)
  App_ e1 e2 -> App (pExp e1) (pExp e2)
-- -- Extended \-calculus expressions
  Let_ i e1 e2 -> Let (pIdent i) (pExp e1) (pExp e2)
  Letrec_ d i e1 e2 -> Letr (pDom d) (pIdent i) (pExp e1) (pExp e2)
  If_ e1 e2 e3 -> If (pExp e1) (pExp e2) (pExp e3)
  Update_ e1 e2 e3 -> Update (pExp e1) (pExp e2) (pExp e3)
-- -- String expressions
  Concat_ e1 e2 -> Concat (pExp e1) (pExp e2)
-- -- Arithmetic
  Inverse_ e -> Inverse (pExp e)
  Plus_ e1 e2 -> Plus (pExp e1) (pExp e2)
  Minus_ e1 e2 -> Minus (pExp e1) (pExp e2)
  Prod_ e1 e2 -> Prod (pExp e1) (pExp e2)
  Div_ e1 e2 -> Div (pExp e1) (pExp e2)
  Mod_ e1 e2 -> Mod (pExp e1) (pExp e2)
-- -- Relational
  Equal_ e1 e2 -> Equal (pExp e1) (pExp e2)
  NotEqual_ e1 e2 -> NotEqual (pExp e1) (pExp e2)
  LessThan_ e1 e2 -> LessThan (pExp e1) (pExp e2)
  LessEqThan_ e1 e2 -> LessEqThan (pExp e1) (pExp e2)
  GreaterThan_ e1 e2 -> GreaterThan (pExp e1) (pExp e2)
  GreaterEqThan_ e1 e2 -> GreaterEqThan (pExp e1) (pExp e2)
-- -- Boolean
  And_ e1 e2 -> And (pExp e1) (pExp e2)
  Or_ e1 e2 -> Or (pExp e1) (pExp e2)
  Neg_ e -> Neg (pExp e)
-- -- Pair and tag operations
  BareTag_ i -> Inject (pIdent i) EExp
  Inject_ i e -> Inject (pIdent i) (pExp e)
  IsTag_ e i -> IsTag (pExp e) (pIdent i)
  Project_ e i -> Project (pExp e) (pIdent i)
  Head_ e -> Head (pExp e)
  Tail_ e -> Tail (pExp e)
  Pair_ e es -> Pair (pExp e) (pExpTreeify es)
pExpTreeify :: [Exp_] -> Exp
pExpTreeify [e] = pExp e
pExpTreeify (e:es) = Pair (pExp e) (pExpTreeify es)

-- Constant polish
pConst :: Const_ -> Const
pConst = \case
  Bot_ d -> Bot (pDom d)
  Int_ i -> Int i
  Str_ s -> Str s
  Sym_ y -> pSymbol y
  BTrue_ -> BConst True
  BFalse_ -> BConst False

-- Evaluation polish
pEval :: Eval_ -> Ev
pEval = \case
  BEEval_ e1 e2 i -> Ev (pExp e1) (pExp e2) (pIdent i)
  NBEEval_ e i -> Ev EExp (pExp e) (pIdent i)
  ExpEval_ e -> ExpEv (pExp e)
