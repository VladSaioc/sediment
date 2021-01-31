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
import Generation.Haskell

_DIST = "dist"

includes _ [] = False
includes v (x:xs) = (v == x) || includes v xs

main :: IO ()
main = do
  args <- getArgs
  let workflow (file : opts) source = let ast = getAst source
        in let staticResults = verifyDomains ast >>= \(de, tt) -> staticAnalysis de tt ast
        in case staticResults of
        Bad msg -> putStr (file ++ ":" ++ msg ++ "\n")
        Ok _ -> do
          putStr (file ++ ": Static analysis successful.\n")
          let results = execute ast
          let
            evalLog log = \case
              Bad msg -> log ++ "\n" ++ msg ++ "\n"
              Ok result -> log ++ "\n" ++ result
          let resultLogs = foldl evalLog "" results
          putStr resultLogs
          unless (null opts) $ do 
            distExists <- doesDirectoryExist _DIST
            unless distExists $ createDirectory _DIST
            let specName = last (splitOn "/" file)
            when (includes "latex" opts) $ do
              let _LATEX_FILE = specName ++ ".tex"
              putStr "\nGenerating Latex...\n"
              writeFile (_DIST ++ "/" ++ _LATEX_FILE) (generateLatex ast)
              putStr "Latex generated\n\n"
            when (includes "haskell" opts) $ do
              specDirExists <- doesDirectoryExist (_DIST ++ "/" ++ specName)
              unless specDirExists $ createDirectory (_DIST ++ "/" ++ specName)

              let _HASKELL_DIR = _DIST ++ "/" ++ specName ++ "/Haskell"
              haskellDirExists <- doesDirectoryExist _HASKELL_DIR
              unless haskellDirExists $ createDirectory _HASKELL_DIR
              
              let _TOOLING_DIR = _HASKELL_DIR ++ "/Tooling"
              toolingDirExists <- doesDirectoryExist _TOOLING_DIR
              unless toolingDirExists $ createDirectory _TOOLING_DIR
              
              let _INNER_SPEC_DIR = _HASKELL_DIR ++ "/Specification"
              innerSpecDirExists <- doesDirectoryExist _INNER_SPEC_DIR
              unless innerSpecDirExists $ createDirectory _INNER_SPEC_DIR

              putStr "Genetaring Haskell interpreter...\n"

              let (updatable, doms, tsys, dats) = generateHaskell ast
              writeFile (_TOOLING_DIR ++ "/Updatable.hs") updatable
              writeFile (_INNER_SPEC_DIR ++ "/Dom.hs") doms
              writeFile (_INNER_SPEC_DIR ++ "/Systems.hs") tsys
              writeFile (_INNER_SPEC_DIR ++ "/Data.hs") dats

              putStr "Haskell interpreter generated successfully"
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
