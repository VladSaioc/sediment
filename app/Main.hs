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
  [fileName] <- getArgs
  -- if not (null fileName)
  --   then do source <- readFile fileName
  --   else do  print "No file name provided. Type a program in Sediment to interpret ad-hoc:"
  --     source <- getContents
  source <- readFile fileName
  let ast = getAst source
  -- pPrint ast
  -- print testUpdate
  let staticResults = verifyDomains ast >>= \(de, tt) -> staticAnalysis de tt ast
  case staticResults of
    Bad msg -> putStr msg
    Ok _ -> do
      putStr ("Static analysis successful for " ++ fileName ++ ".")
      let results =execute ast
      let
        evalLog log = \case
          Bad msg -> log ++ "\n" ++ msg
          Ok v -> log ++ "\n" ++ show v
      let resultLogs = foldl evalLog "" results
      putStr resultLogs