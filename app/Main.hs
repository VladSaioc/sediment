module Main where

import GetAst
import System.Environment

main :: IO ()
main = do
  s <- getLine
  let ast = getAst s
  print ast
