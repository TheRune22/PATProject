module Foo where

pow m n =
  if m == (0 :: Int) then
    (1 :: Int)
  else
    n * pow (m - (1 :: Int)) n


foo x y z = (pow x y + pow y x) + (pow x x + pow y z) + (pow x z + pow x y)
