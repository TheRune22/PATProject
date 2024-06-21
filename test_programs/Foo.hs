module Foo where


pow n m =
  if m == (0 :: Int) then
    (1 :: Int)
  else
    n * pow n (m - (1 :: Int))

foo x y z = (pow y x + pow x y) + (pow x x + pow z y) + (pow z x + pow y x)
