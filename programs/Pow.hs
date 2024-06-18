pow :: Int -> Int -> Int
-- TODO: rewrite as lambda?
pow m n =
  if m == 0 then
    1
  else
    n * pow (m - 1) n


main () =
  pow 3 2
