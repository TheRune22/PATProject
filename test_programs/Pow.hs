module Pow where


pow n m =
  if m == (0 :: Int) then
    (1 :: Int)
  else
    n * pow n (m - (1 :: Int))
