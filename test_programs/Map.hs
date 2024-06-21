module Map where


mapL op l =
  if l == ([] :: [Int]) then
    ([] :: [Int])
  else
    op (head l) : mapL op (tail l)