module AstSediment where

data Spec = Spec [Df] [Ev]
  deriving (Eq, Ord, Show, Read)

data Df = DomDf String Dom
  | SynDf String Syn
  | TSysDf TDom String [Rule]
  | DataDf String Exp
  | DataRecDf Dom String Exp
  deriving (Eq, Ord, Show, Read)

data Ev = Ev Exp Exp String
  deriving (Eq, Ord, Show, Read)

data Dom = IntDom | BoolDom | StrDom | SymDom | EmptyDom
  | VarDom String | FuncDom Dom Dom | ProdDom Dom Dom
  | UnionDom [(String, Dom)]
  deriving (Eq, Ord, Show, Read)

newtype Syn = Syn [(String, Dom)]
  deriving (Eq, Ord, Show, Read)

data TDom = TDom Dom Dom Dom
  deriving (Eq, Ord, Show, Read)

data Exp = Var String -- Variables & constants
  | EmptyExp
  | ConstE Const
  | Lambda Dom String Exp -- Basic \-calculus expressions
  | App Exp Exp
  | Let String Exp Exp -- Extended \-calculus expressions
  | Letrec Dom String Exp Exp
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

data Rule = Rule String Config Config Exp [Premise]
  deriving (Eq, Ord, Show, Read)

data Premise = IfPremise Exp
    | LetPremiseL String Exp
    | LetrecPremise Dom String Exp
    | TrPremise Exp Exp String Config
  deriving (Eq, Ord, Show, Read)

data Config = EmptyConfig
  | TagConfig String Config
  | PairConfig Config Config
  | VarConfig String
  | ConstConfig Const
  deriving (Eq, Ord, Show, Read)