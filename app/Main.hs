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

main :: IO ()
main = do
  [fileName] <- getArgs
  source <- readFile fileName
  let ast = getAst source
  let staticResults = verifyDomains ast >>= \(de, tt) -> staticAnalysis de tt ast
  case staticResults of
    Bad msg -> putStr (fileName ++ ":" ++ msg)
    Ok _ -> do
      putStr (fileName ++ ": Static analysis successful.")
      let results =execute ast
      let
        evalLog log = \case
          Bad msg -> log ++ "\n" ++ msg
          Ok result -> log ++ "\n" ++ result
      let resultLogs = foldl evalLog "" results
      putStr resultLogs