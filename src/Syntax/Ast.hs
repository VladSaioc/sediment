module Syntax.Ast where

data Spec = Spec [Df] [Ev] | SpecError String
  deriving (Eq, Ord, Show, Read)

data Df = DomDf String Dom
  | TSysDf TDom String [Rule]
  | DataDf String Exp
  | DataRecDf Dom String Exp
  deriving (Eq, Ord, Show, Read)

data Ev = Ev Exp Exp String
  deriving (Eq, Ord, Show, Read)

data Dom = IntDom | BoolDom | StrDom | SymDom | EDom
  | VarDom String | FuncDom Dom Dom | ProdDom Dom Dom
  | UnionDom [(String, Dom)]
  deriving (Eq, Ord, Show, Read)

data TDom = TDom Dom Dom Dom
  deriving (Eq, Ord, Show, Read)

data Exp = Var String -- Variables & constants
  | EExp
  | ConstE Const
  | Lambda Dom String Exp -- Basic \-calculus expressions
  | App Exp Exp
  | Let String Exp Exp -- Extended \-calculus expressions
  | Letr Dom String Exp Exp
  | If Exp Exp Exp
  | Update Exp Exp Exp
  | Concat Exp Exp -- String expressions
  | Inverse Exp -- Arithmetic expressions
  | Plus Exp Exp
  | Minus Exp Exp
  | Prod Exp Exp
  | Div Exp Exp
  | Mod Exp Exp
  | Neg Exp -- Boolean expressions
  | Or Exp Exp
  | And Exp Exp
  | Equal Exp Exp -- Relational expressions
  | NotEqual Exp Exp
  | LessThan Exp Exp
  | LessEqThan Exp Exp
  | GreaterThan Exp Exp
  | GreaterEqThan Exp Exp
  | Pair Exp Exp -- Pair and tag operations
  | Head Exp
  | Tail Exp
  | Project Exp String
  | Inject String Exp
  | IsTag Exp String
  deriving (Eq, Ord, Show, Read)

data Const
  = Bot Dom
  | Int Integer
  | Str String
  | Sym String
  | BConst Bool
  deriving (Eq, Ord, Show, Read)

data Rule = Rule String Con Con Exp [Pr]
  deriving (Eq, Ord, Show, Read)

data Pr = IfPr Exp
  | LetPr String Exp
  | LetrPr Dom String Exp
  | TrPr Exp Exp String Con
  deriving (Eq, Ord, Show, Read)

data Con = ECon
  | TagCon String Con
  | PairCon Con Con
  | VarCon String
  | ConstCon Const
  deriving (Eq, Ord, Show, Read)