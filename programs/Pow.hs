module Pow where

pow :: Int -> Int -> Int
pow m n =
  if m == 0 then
    1
  else
    n * pow (m - 1) n
