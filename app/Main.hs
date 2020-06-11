module Main where

import System.IO
import System.Environment
import System.Directory

import Control.Monad
import Text.Pretty.Simple (pPrint)
import Data.List.Split (splitOn)

import Syntax.GetAst
import Syntax.ErrM

import Semantics.Dom.Verify

import Semantics.StaticAnalysis
import Semantics.Execute

import Generation.Latex

_DIST = "dist"

includes _ [] = False
includes v (x:xs) = (v == x) || includes v xs

main :: IO ()
main = do
  args <- getArgs
  let workflow (file : opts) source = let ast = getAst source
        in let staticResults = verifyDomains ast >>= \(de, tt) -> staticAnalysis de tt ast
        in case staticResults of
        Bad msg -> putStr (file ++ ": " ++ msg ++ "\n")
        Ok _ -> do
          putStr (file ++ ": Static analysis successful.\n")
          let results = execute ast
          let
            evalLog log = \case
              Bad msg -> log ++ "\n" ++ msg
              Ok result -> log ++ "\n" ++ result
          let resultLogs = foldl evalLog "" results
          putStr resultLogs
          distExists <- doesDirectoryExist _DIST
          unless distExists $ createDirectory _DIST 
          when (includes "latex" opts) $ do
            let _LATEX_FILE = last (splitOn "/" file) ++ ".tex"
            putStr "Generating Latex:\n"
            writeFile (_DIST ++ "/" ++ _LATEX_FILE) (generateLatex ast)
            putStr "Latex generated"
  case args of
    (fileName : opts) -> do
      source <- readFile fileName
      workflow (fileName : opts) source
    _ -> let
        takeInput = do
          source <- getLine
          when (source /= "exit") $ do
            workflow [">REPL"] source
            takeInput
      in takeInput
