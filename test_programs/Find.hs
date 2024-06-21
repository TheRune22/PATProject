module Find where


find s x =
  if null s then
    (0 :: Int)
  else
    if head s == x then
      (0 :: Int)
    else
      (1 :: Int) + find (tail s) x
