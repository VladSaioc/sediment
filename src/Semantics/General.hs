module Semantics.General where

import Syntax.ErrM

results :: Eq a => [Err a] -> Err [a]
results [] = Ok []
results (r:rs) = case r of
  Bad msg -> Bad msg
  Ok rv ->  let
      rest = results rs
    in case rest of
      Bad msg -> Bad msg
      Ok rs -> Ok (rv : rs)