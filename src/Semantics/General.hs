module Semantics.General where

import Syntax.ErrM

results :: Eq a => [Err a] -> Err [a]
results [] = Ok []
results (r:rs) = r >>= \rv -> results rs >>= \rs -> Ok (rv : rs)