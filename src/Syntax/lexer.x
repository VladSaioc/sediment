{
module Lexer (main) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
@var = $alpha [$alpha $digit \_ \']*

:-

  $white+				;
  "--".*				;

  "data"        { \s -> Data }
  "datarec"     { \s -> Datarec }
  "domain"      { \s -> Domain }
  "syntax"      { \s -> Syntax }
  "system"      { \s -> System }
  "evaluate"    { \s -> Evaluate }

  ":"           { \s -> Colon }
  ":="          { \s -> Bind }
  ";"           { \s -> Semicolon }

  "|-"          { \s -> BindModel }
  "=>"          { \s -> EvaluatesTo }
  "end"         { \s -> End }
  "\\\\"        { \s -> PremiseSplit }

  "*"           { \s -> Prod }
  "->"          { \s -> Map }
  "integer"     { \s -> IntDom }
  "bool"        { \s -> BoolDom }
  "string"      { \s -> StrDom }
  "symbol"      { \s -> SymDom }

  "["           { \s -> LBrack }
  "]"           { \s -> RBrack }
  "("           { \s -> LParen }
  ")"           { \s -> RParen }

  "."           { \s -> Period }
  "\\"          { \s -> Lambda }

  letrec        { \s -> Letrec }
  let				  	{ \s -> Let }
  in					  { \s -> In }
  "|->"         { \s -> MapsTo }
  "if"          { \s -> If }
  "then"        { \s -> Then }
  "else"        { \s -> Else }

  "is"          { \s -> Is }
  ","           { \s -> Comma }
  "(<)"         { \s -> Head }
  "(>)"         { \s -> Tail }
  ">>"          { \s -> Project }

  "+"           { \s -> Plus }
  "-"           { \s -> Minus }
  "*"           { \s -> Prod }
  "/"           { \s -> Div }
  "%"           { \s -> Mod }
  
  "="           { \s -> Equal }
  "!="          { \s -> NotEqual }
  "<"           { \s -> LessThan }
  ">"           { \s -> GreaterThan }
  "<="          { \s -> LEQThan }
  ">="          { \s -> GEQThan }

  "!"           { \s -> Neg }
  "&"           { \s -> And }
  "|"           { \s -> Or }

  "++"          { \s -> Concat }

  "true"        { \s -> BConst True }
  "false"       { \s -> BConst False }
  "_|_"         { \s -> Bottom }
  \" . # [\"] \"        { \s -> Str s }
  `@var`        { \s -> Sym s }
  $digit+				{ \s -> Num (read s) }
  @var	      	{ \s -> Var s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
-- Definitions
  Data    |
  Datarec |
  Domain  |
  Syntax  |
  System  |
-- Evaluation
  Evaluate  |
-- General operators & symbols
  Colon     |
  Bind      |
  Semicolon |
-- Transition operators
  BindModel     |
  EvaluatesTo   |
  End           |
  PremiseSplit  |
-- Domains and syntax
  Map     |
  Of      |
  IntDom  |
  BoolDom |
  StrDom  |
  SymDom  |
-- Brackets
  LBrack  |
  RBrack  |
  LParen  |
  RParen  |
-- Expression
-- -- Lambda
  Period  |
  App     |
  Lambda  |
-- -- Other functional
  Letrec  |
	Let     |
	In      |
  MapsTo  |
  If      |
  Then    |
  Else    |
-- -- Tuples and pairs
  Is      |
  Comma   |
  Head    |
  Tail    |
  Project |
-- -- Arithmetic operators
  Plus    |
  Minus   |
  Prod    |
  Div     |
  Mod     |
-- -- Relational operators
  Equal       |
  NotEqual    |
  LessThan    |
  GreaterThan |
  LEQThan     |
  GEQThan     |
-- -- Boolean operators
  Neg |
  And |
  Or  |
-- -- String operators
  Concat  |
-- -- Constants & variables
  BConst Bool |
  Bottom      |
  Str String  |
	Sym String	|
	Var String	|
	Num Int
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}