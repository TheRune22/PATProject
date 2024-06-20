{-# LANGUAGE TemplateHaskell #-}
import qualified Find
import qualified Foo
import qualified Pow
import qualified Map
--import qualified Partial


test :: (Eq a, Show a) => a -> a -> IO ()
test expected actual =
  putStrLn $ (if expected == actual then "SUCCESS" else "FAILURE") <> " - Expected: " <> show expected <> ", Got: " <> show actual


$(Find.mainSpecializer "find123" [1, 2, 3])
$(Pow.mainSpecializer "square" 3)
$(Foo.mainSpecializer "foo2" 2)
$(Map.mainSpecializer "map123" [1, 2, 3])
-- $(Partial.mainSpecializer "squareList" 3)

main :: IO ()
main = do
  test (Find.find [1, 2, 3] 4) (find123 (4 :: Int))
  test (Pow.pow 3 2) (square 2)
  test (Foo.foo 2 3 4) (foo2 3 4)
  test (Map.mapL (+1) [1, 2, 3]) (map123 (+1))
--  test (Partial.powList 3 [1, 2, 3]) (squareList [1, 2, 3])
