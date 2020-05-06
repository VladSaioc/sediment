module Main where

import System.IO
import Control.Monad
import GetAst
import System.Environment
import Text.Pretty.Simple (pPrint)

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
  pPrint ast
