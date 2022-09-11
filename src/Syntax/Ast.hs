module Syntax.Ast where

import Data.List
import Data.Map

type Pos = (Int, Int)

type PosVar = (String, Pos)

data Spec = Spec [Df] [Ev] | SpecError String
  deriving (Eq, Ord, Read)

data Df = Df Pos DfBase
  deriving (Eq, Ord, Read)

data DfBase = DomDf String Dom
  | TSysDf TDom String [Rule]
  | DataDf Con Exp
  | DataRecDf Dom String String Exp
  deriving (Eq, Ord, Read)

data Ev = EvP Pos EvBase
  deriving (Eq, Ord, Read)

data EvBase = Ev Exp Exp String | ExpEv Exp
  deriving (Eq, Ord, Read)

data Dom = Dom Pos DomBase
  deriving (Eq, Ord, Read)

data DomBase = IntDom | BoolDom | StrDom | SymDom | EDom
  | VarDom String | FuncDom Dom Dom | ProdDom Dom Dom
  | UnionDom [(String, Dom)] Bool
  deriving (Eq, Ord, Show, Read)

data TDom = TDom Dom Dom Dom
  deriving (Eq, Ord, Show, Read)

data Exp = Exp Pos ExpBase
  deriving (Eq, Ord, Read)

data ExpBase = Pair Exp Exp -- Pairs
  | Lambda Dom String Exp -- Basic \-calculus expressions
  | Let Con Exp Exp -- Extended \-calculus expressions
  | Letr Dom String String Exp Exp
  | If Exp Exp Exp
  | Concat Exp Exp -- String expressions
  | Plus Exp Exp -- Arithmetic expressions
  | Minus Exp Exp
  | Prod Exp Exp
  | Div Exp Exp
  | Mod Exp Exp
  | Or Exp Exp -- Boolean expressions
  | And Exp Exp
  | Equal Exp Exp -- Relational expressions
  | NotEqual Exp Exp
  | LessThan Exp Exp
  | LessEqThan Exp Exp
  | GreaterThan Exp Exp
  | GreaterEqThan Exp Exp
  | Inject String Exp
  | IsTag Exp String
  | Inverse Exp -- Unary operators
  | Neg Exp 
  | Project Exp String -- Tags
  | Head Exp
  | Tail Exp
  | App Exp Exp
  | Update Exp Exp Exp
  | Var String -- Variables & constants
  | ConstE Const
  | EExp
  | Closure Env Exp
  deriving (Eq, Ord, Show, Read)

data Const = Const Pos ConstBase
  deriving (Eq, Ord, Read)

data ConstBase
  = Bot Dom
  | Int Integer
  | Str String
  | Sym String
  | BConst Bool
  deriving (Eq, Ord, Show, Read)

data Rule = Rule PosVar Con Con Exp [Pr]
  deriving (Eq, Ord, Read)

data Pr = Pr Pos PrBase
  deriving (Eq, Ord, Read)

data PrBase = IfPr Exp
  | LetPr Con Exp
  | LetrPr Dom String String Exp
  | TrPr Exp Exp String Con
  deriving (Eq, Ord, Show, Read)

data Con = Con Pos ConBase
  deriving (Eq, Ord, Read)

data ConBase = ECon
  | TagCon String Con
  | PairCon Con Con
  | VarCon String
  | ConstCon Const
  deriving (Eq, Ord, Show, Read)

instance Show Con where
  show (Con _ c)= case c of
    ECon -> ""
    TagCon t c -> t ++ "[" ++ show c ++ "]"
    PairCon c1 c2 -> "(" ++ show c1 ++ ", " ++ show c2 ++ ")"
    VarCon x -> x
    ConstCon c -> show c

instance Show Dom where
  show (Dom _ d)= case d of
    EDom -> "()"
    VarDom x -> x
    IntDom -> "Integer"
    BoolDom -> "Boolean"
    SymDom -> "Symbol"
    StrDom -> "String"
    FuncDom d1 d2 -> show d1 ++ " -> " ++ show d2
    ProdDom d1 d2 -> show d1 ++ " * " ++ show d2
    UnionDom us False -> "[" ++ intercalate " + " (Data.List.map (\(t,d) -> t ++ "[" ++ show d ++ "]") us) ++ "]"
    UnionDom us True -> "<" ++ intercalate " | " (Data.List.map (\(t,d) -> t ++ " of " ++ show d) us) ++ ">"
  
instance Show Const where
  show (Const _ c)= case c of
    Bot _ -> "_|_"
    BConst b -> if b then "tt"
      else "ff"
    Int i -> show i
    Str s -> "\"" ++ s ++ "\""
    Sym y -> "`" ++ y ++ "`"

instance Show Exp where
  show (Exp _ e)= case e of
    Var x -> x
    EExp -> "__"
    ConstE c -> show c
    Lambda _ x e -> "\\" ++ x ++ "." ++ show e
    App e1 e2 -> "(" ++ show e1 ++ ")" ++ "(" ++ show e2 ++ ")"
    Let c e1 e2 -> "let " ++ show c ++ " := " ++ show e1 ++ " in " ++ show e2
    Letr _ x1 x2 e1 e2 -> "letrec" ++ x1 ++ " := " ++ x2 ++ "." ++ show e1 ++ " in " ++ show e2
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
    Or e1 e2 -> show e1 ++ " | " ++ show e2
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
    Inject t e -> t ++ "{ " ++ show e ++ " }"
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