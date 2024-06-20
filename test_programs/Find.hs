module Find where


find l x =
  if l == ([] :: [Int]) then
    (0 :: Int)
  else
    if head l == x then
      (0 :: Int)
    else
      (1 :: Int) + find (tail l) x
