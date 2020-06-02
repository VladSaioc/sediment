module Syntax.Ast where

import Data.List
import Data.Map

data Spec = Spec [Df] [Ev] | SpecError String
  deriving (Eq, Ord, Show, Read)

data Df = DomDf String Dom
  | TSysDf TDom String [Rule]
  | DataDf String Exp
  | DataRecDf Dom String Exp
  deriving (Eq, Ord, Show, Read)

data Ev = Ev Exp Exp String | ExpEv Exp
  deriving (Eq, Ord, Show, Read)

data Dom = IntDom | BoolDom | StrDom | SymDom | EDom
  | VarDom String | FuncDom Dom Dom | ProdDom Dom Dom
  | UnionDom [(String, Dom)]
  deriving (Eq, Ord, Read)

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
  | Closure Env Exp
  deriving (Eq, Ord, Read)

data Const
  = Bot Dom
  | Int Integer
  | Str String
  | Sym String
  | BConst Bool
  deriving (Eq, Ord, Read)

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

instance Show Dom where
  show = \case
    EDom -> ""
    VarDom x -> x
    IntDom -> "Integer"
    BoolDom -> "Boolean"
    SymDom -> "Symbol"
    StrDom -> "String"
    FuncDom d1 d2 -> show d1 ++ " -> " ++ show d2
    ProdDom d1 d2 -> show d1 ++ " * " ++ show d2
    UnionDom us -> "[" ++ intercalate " + " (Data.List.map (\(t,d) -> t ++ "<" ++ show d ++ ">") us) ++ "]"
  
instance Show Const where
  show = \case
    Bot d -> "_|_"
    BConst b -> if b then "tt"
      else "ff"
    Int i -> show i
    Str s -> "\"" ++ s ++ "\""
    Sym y -> "`" ++ y ++ "`"

instance Show Exp where
  show = \case
    Var x -> x
    EExp -> "__"
    ConstE c -> show c
    Lambda _ x e -> "\\" ++ x ++ "." ++ show e
    App e1 e2 -> "(" ++ show e1 ++ ")" ++ "(" ++ show e2 ++ ")"
    Let x e1 e2 -> "let " ++ x ++ " := " ++ show e1 ++ " in " ++ show e2
    Letr _ x e1 e2 -> "letrec" ++ x ++ " := " ++ show e1 ++ " in " ++ show e2
    If e1 e2 e3 -> "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    Update e1 e2 e3 -> "[" ++ show e1 ++ " -> " ++ show e3 ++ "]" ++ "(" ++ show e3 ++ ")"
    Concat e1 e2 -> show e1 ++ " ++ " ++ show e2
    Inverse e -> "-(" ++ show e ++ ")"
    Plus e1 e2 -> show e1 ++ " + " ++ show e2
    Minus e1 e2 -> show e1 ++ " - " ++ show e2
    Prod e1 e2 -> show e1 ++ " * " ++ show e2
    Div e1 e2 -> show e1 ++ " / " ++ show e2
    Mod e1 e2 -> show e1 ++ " % " ++ show e2
    Neg e -> "!(" ++ show e ++ ")"
    Or e1 e2 -> show e1 ++ " || " ++ show e2
    And e1 e2 -> show e1 ++ " & " ++ show e2
    Equal e1 e2 -> show e1 ++ " == " ++ show e2
    NotEqual e1 e2 -> show e1 ++ " != " ++ show e2
    LessThan e1 e2 -> show e1 ++ " < " ++ show e2
    LessEqThan e1 e2 -> show e1 ++ " <= " ++ show e2
    GreaterThan e1 e2 -> show e1 ++ " > " ++ show e2
    GreaterEqThan e1 e2 -> show e1 ++ " >= " ++ show e2
    Pair e1 e2 -> "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
    Head e -> "(<) " ++ show e
    Tail e -> "(>) " ++ show e
    Project e t -> show e ++ " >> " ++ t
    Inject t e -> t ++ "[" ++ show e ++ "]"
    IsTag e t -> show e ++ " is " ++ t
    Closure env e -> "((" ++ show env ++ "," ++ show e ++ "))"
    
type Env = Map String Value

data Value = VEmpty | VInt Integer | VStr String | VBool Bool | VSym String
  | VPair Value Value
  | VTag String | VTagE String Value
  | Cloj Env String Exp
  | RCloj Env String String Exp
  deriving (Eq, Ord, Read)

instance Show Value where
  show = \case
    VEmpty -> "_"
    VInt i -> show i
    VBool b -> show b
    VStr s -> "\"" ++ s ++ "\""
    VSym y -> "`" ++ y ++ "`"
    VPair v1 v2 -> "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
    VTag t -> t ++ "[]"
    VTagE t v -> t ++ "[" ++ show v ++ "]"
    Cloj env x e -> "{" ++ show env ++ ", " ++ x ++ ", " ++ show e ++ "}"
    RCloj env x x' e -> "{" ++ show env ++ ", " ++ x ++ ", " ++ x' ++ ", " ++ show e ++ "}"

type Pos = (Int, Int)