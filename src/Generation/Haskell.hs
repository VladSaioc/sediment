module Generation.Haskell where

import Syntax.Ast

import Generation.Haskell.Df

updatableFunctions = "module Tooling.Updatable where\n\n"
  ++ "import Data.Map\n"
  ++ "import Data.Maybe\n\n"
  ++ "data Updatable b c = Updatable (b -> c) (Map b c)\n\n"
  ++ "apply (Updatable f map) x = fromMaybe (f x) (Data.Map.lookup x map)\n"
  
generateHaskell :: Spec -> (String, String, String, String)
generateHaskell (Spec dfs _) = (
    updatableFunctions,
    printDomDfs dfs,
    printTSysDfs dfs,
    printDataDfs dfs
  )