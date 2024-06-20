module Pow where


pow m n =
  if m == (0 :: Int) then
    (1 :: Int)
  else
    n * pow (m - (1 :: Int)) n
