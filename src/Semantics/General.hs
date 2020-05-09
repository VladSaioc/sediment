module Semantics.General where

data Result a = Ok a | Bad String
  deriving (Eq, Ord, Show, Read)