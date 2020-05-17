module Main where

import System.IO
import System.Environment
import Control.Monad
import Text.Pretty.Simple (pPrint)

import Syntax.GetAst
import Syntax.ErrM

import Semantics.Dom.Verify

import Semantics.StaticAnalysis
import Semantics.Execute

import Semantics.Playground

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  -- if not (null fileName)
  --   then do source <- readFile fileName
  --   else do  print "No file name provided. Type a text in Sediment to interpret ad-hoc:"
  --     source <- getContents
  source <- readFile fileName
  let ast = getAst source
  -- pPrint ast
  print testUpdate
  let result = verifyDomains ast >>=
        \(de, tt) -> staticAnalysis de tt ast >>
        execute ast >>= \results ->
        let evalLog log v = log ++ "\n" ++ show v  
        in Ok (foldl evalLog "Specification evaluation successful." results)
  case result of
    Bad msg -> putStr msg
    Ok msg -> putStr msg