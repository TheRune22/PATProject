{-# LANGUAGE TemplateHaskell #-}
import qualified Find
import qualified Foo
import qualified Pow
import qualified Map


test :: (Eq a, Show a) => a -> a -> IO ()
test expected actual =
  putStrLn $ (if expected == actual then "SUCCESS" else "FAILURE") <> " - Expected: " <> show expected <> ", Got: " <> show actual


$(Pow.mainSpecializer "square" 3)
$(Foo.mainSpecializer "foo2" 2)
$(Find.mainSpecializer "findabc" "abc")
$(Find.mainSpecializer "find123" ([1, 2, 3] :: [Int]))
$(Map.mainSpecializer "map123" ([1, 2, 3] :: [Int]))


main :: IO ()
main = do
  test (Pow.pow 2 3) (square 2)
  test (Foo.foo 2 3 4) (foo2 3 4)
  test (Find.find "abc" 'b') (findabc 'b')
  test (Find.find [1, 2, 3] 3) (find123 3)
  test (Map.mapL (+1) [1, 2, 3]) (map123 (+1))

