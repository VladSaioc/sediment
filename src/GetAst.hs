module GetAst (getAst) where

import Syntax.AbsSediment
import Syntax.ParSediment
import Syntax.ErrM
import AstSediment

pIdent :: Ident -> String
pIdent (Ident s) = s
pSymbol :: Symbol -> Const
pSymbol (Symbol s) = Sym (drop 1 (take (length s - 1) s))

getRawAst s =
  let
    tokens = myLexer s
    ast = pSpecification_ tokens
  in ast

polishAst (Ok (Spec_ dfs evs)) =
  Spec
    (map pDef dfs)
    (map pEval evs)
polishAst (Bad err) = SpecError err

getAst = polishAst . getRawAst

-- Definition polish
pDef :: Def_ -> Df
pDef (DomDef_ i de) = DomDf (pIdent i) (pDomDefExp de)
pDef (SyntaxDef_ i frs) = SynDf (pIdent i) (map pFormRule frs)
pDef (TSystem_ td i rs) = TSysDf (pTDom td) (pIdent i) (map pRule rs)
pDef (Data_ i e) = DataDf (pIdent i) (pExp e)
pDef (DataRec_ d i e) = DataRecDf (pDom d) (pIdent i) (pExp e)

-- Domain definition polish
pDomDefExp :: DomDefExp_ -> Dom
pDomDefExp (DefUnion_ (UnionDom_ ubs)) = UnionDom (map pUnionBranch ubs)
pDomDefExp (DefNonUnion_ d) = pDom d

-- -- Union branch polish
pUnionBranch :: UnionBranch_ -> (String, Dom)
pUnionBranch (TagDom_ i d) = (pIdent i, pDom d)
pUnionBranch (Tag_ i) = (pIdent i, EDom)

-- -- Non-union domain polish
pDom :: Dom_ -> Dom
pDom StrDom_ = StrDom
pDom BoolDom_ = BoolDom
pDom SymDom_ = SymDom
pDom IntDom_ = IntDom
pDom (VarDom_ i) = VarDom (pIdent i)
pDom (FuncDom_ d1 d2) = FuncDom (pDom d1) (pDom d2)
pDom (ProdDom_ d1 d2) = ProdDom (pDom d1) (pDom d2)

-- Syntax polish
pFormRule :: FormRule_ -> (String, Dom)
pFormRule (Term_ i d) = (pIdent i, pDom d)
pFormRule (BareTerm_ i) = (pIdent i, EDom)

-- Transition system domain polish
pTDom :: TDom_ -> TDom
pTDom (BETDom_ d1 d2 d3) = TDom (pDom d1) (pDom d2) (pDom d3)
pTDom (NBETDom_ d1 d2) = TDom EDom (pDom d1) (pDom d2)

-- Transition system polish
-- -- Rule polish
pRule :: Rule_ -> Rule
pRule (BEAxiom_ i c1 c2 e) = Rule (pIdent i) (pConfig c1) (pConfig c2) (pExp e) []
pRule (NBEAxiom_ i c e) = Rule (pIdent i) ECon (pConfig c) (pExp e) []
pRule (BERule_ i c1 c2 e ps) = Rule (pIdent i) (pConfig c1) (pConfig c2) (pExp e) (map pPremise ps)
pRule (NBERule_ i c e ps) = Rule (pIdent i) ECon (pConfig c) (pExp e) (map pPremise ps)

-- -- Premise polish
pPremise :: Premise_ -> Pr
pPremise (IfPremise_ e) = IfPr (pExp e)
pPremise (LetPremise_ i e) = LetPr (pIdent i) (pExp e)
pPremise (LetrecPremise_ d i e) = LetrPr (pDom d) (pIdent i) (pExp e)
pPremise (BETrInternalPremise_ e1 e2 c) = TrPr (pExp e1) (pExp e2) "/"  (pConfig c)
pPremise (BETrExternalPremise_ e1 e2 i c) = TrPr (pExp e1) (pExp e2) (pIdent i)  (pConfig c)
pPremise (NBETrInternalPremise_ e c) = TrPr EExp (pExp e) "/"  (pConfig c)
pPremise (NBETrExternalPremise_ e i c) = TrPr EExp (pExp e) (pIdent i)  (pConfig c)

-- -- Configuration polish
pConfig :: Config_ -> Con
pConfig (BareTagConfig_ i) = TagCon (pIdent i) ECon
pConfig (VarConfig_ i) = VarCon (pIdent i)
pConfig (ConstConfig_ c) = ConstCon (pConst c)
pConfig (TagConfig_ i c) = TagCon (pIdent i) (pConfig c)
pConfig (PairConfig_ c cs) = PairCon (pConfig c) (pConfigTreeify cs)
pConfigTreeify :: [Config_] -> Con
pConfigTreeify [c] = pConfig c
pConfigTreeify (c:cs) = PairCon (pConfig c) (pConfigTreeify cs)

-- Expression polish
pExp :: Exp_ -> Exp
pExp (Var_ i) = Var (pIdent i)
pExp (ConstE_ c) = ConstE (pConst c)
-- -- Basic \-calculus expressions
pExp (Lambda_ d i e) = Lambda (pDom d) (pIdent i) (pExp e)
pExp (App_ e1 e2) = App (pExp e1) (pExp e2)
-- -- Extended \-calculus expressions
pExp (Let_ i e1 e2) = Let (pIdent i) (pExp e1) (pExp e2)
pExp (Letrec_ d i e1 e2) = Letr (pDom d) (pIdent i) (pExp e1) (pExp e2)
pExp (If_ e1 e2 e3) = If (pExp e1) (pExp e2) (pExp e3)
pExp (Update_ e1 e2 e3) = Update (pExp e1) (pExp e2) (pExp e3)
-- -- String expressions
pExp (Concat_ e1 e2) = Concat (pExp e1) (pExp e2)
-- -- Arithmetic
pExp (Inverse_ e) = Inverse (pExp e)
pExp (Plus_ e1 e2) = Plus (pExp e1) (pExp e2)
pExp (Minus_ e1 e2) = Minus (pExp e1) (pExp e2)
pExp (Prod_ e1 e2) = Prod (pExp e1) (pExp e2)
pExp (Div_ e1 e2) = Div (pExp e1) (pExp e2)
pExp (Mod_ e1 e2) = Mod (pExp e1) (pExp e2)
-- -- Relational
pExp (Equal_ e1 e2) = Equal (pExp e1) (pExp e2)
pExp (NotEqual_ e1 e2) = NotEqual (pExp e1) (pExp e2)
pExp (LessThan_ e1 e2) = LessThan (pExp e1) (pExp e2)
pExp (LessEqThan_ e1 e2) = LessEqThan (pExp e1) (pExp e2)
pExp (GreaterThan_ e1 e2) = GreaterThan (pExp e1) (pExp e2)
pExp (GreaterEqThan_ e1 e2) = GreaterEqThan (pExp e1) (pExp e2)
-- -- Boolean
pExp (And_ e1 e2) = And (pExp e1) (pExp e2)
pExp (Or_ e1 e2) = Or (pExp e1) (pExp e2)
pExp (Neg_ e) = Neg (pExp e)
-- -- Pair and tag operations
pExp (BareTag_ i) = Inject (pIdent i) EExp
pExp (Inject_ i e) = Inject (pIdent i) (pExp e)
pExp (IsTag_ e i) = IsTag (pExp e) (pIdent i)
pExp (Project_ e i) = Project (pExp e) (pIdent i)
pExp (Head_ e) = Head (pExp e)
pExp (Tail_ e) = Tail (pExp e)
pExp (Pair_ e es) = Pair (pExp e) (pExpTreeify es)
pExpTreeify :: [Exp_] -> Exp
pExpTreeify [e] = pExp e
pExpTreeify (e:es) = Pair (pExp e) (pExpTreeify es)

-- Constant polish
pConst :: Const_ -> Const
pConst (Bot_ d) = Bot (pDom d)
pConst (Int_ i) = Int i
pConst (Str_ s) = Str s
pConst (Sym_ y) = pSymbol y
pConst BTrue_ = BConst True
pConst BFalse_ = BConst False

-- Evaluation polish
pEval :: Eval_ -> Ev
pEval (BEEval_ e1 e2 i) = Ev (pExp e1) (pExp e2) (pIdent i)
pEval (NBEEval_ e i) = Ev EExp (pExp e) (pIdent i)
