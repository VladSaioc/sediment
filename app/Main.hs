module Main where

import System.IO
import System.Environment
import Control.Monad
import Text.Pretty.Simple (pPrint)

import Syntax.GetAst

import Semantics.General

import Semantics.Domains.Verify

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
  case verifyDomains ast of
    Bad msg -> pPrint msg
    Ok de -> pPrint de
