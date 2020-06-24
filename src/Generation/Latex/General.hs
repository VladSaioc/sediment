module Generation.Latex.General where

import Syntax.Ast

import Generation.Latex.Dom

import Data.Char (isDigit)

varTex x = let
    getTicks = \case
      [] -> []
      c:cs -> if c == '\'' then c : getTicks cs
        else []
    ticks = getTicks (reverse x)
    left = take (length x - length ticks) x
  in if isDigit (last left) then
    let
      getSubscript [] = ""
      getSubscript (c:cs) = if isDigit c then getSubscript cs ++ [c]
        else ""
      subscript = getSubscript (reverse left)
      x' = take (length left - length subscript) left
    in x' ++ "_{" ++ subscript ++ "}" ++ ticks
  else x

constTex :: Const -> String
constTex = \case
  Bot d -> " \\bot_{ " ++ domTex d ++ " } "
  Int i -> show i
  Str s -> " \\textrm{\" " ++ s ++ "\" } "
  Sym y -> " \\sv{ " ++ y ++ " } "
  BConst True -> "\\ltrue"
  BConst False -> "\\lfalse"