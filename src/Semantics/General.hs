module Semantics.General where

import Syntax.ErrM

errMsg :: Eq a => (Int, Int) -> String -> Err a
errMsg (row, col) msg = Bad (show row ++ ":" ++ show col ++ ": " ++ msg)

results :: Eq a => [Err a] -> Err [a]
results [] = Ok []
results (r:rs) = r >>= \rv -> results rs >>= \rs -> Ok (rv : rs)