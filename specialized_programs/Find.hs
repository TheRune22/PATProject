{-# LANGUAGE TemplateHaskell #-}
module Find where
import Specializer
find l x
  = if l == ([] :: [Int]) then (0 :: Int) else
      if head l == x then (0 :: Int) else (1 :: Int) + find (tail l) x
find_BodyGen_StaticDynamic l
  = if l == ([] :: [Int]) then lift (0 :: Int) else
      [|
        if ($( [| ($( lift (head l) )) == ($( [| x |] )) |] )) then
          ($( lift (0 :: Int) )) else
          ($(
             [|
               ($( lift (1 :: Int) )) +
                 ($(
                    [|
                      ($(
                         specializer (find_BodyGen_StaticDynamic (tail l)) [[p| x |]]
                           [lift (tail l)]
                           "find_BodyGen_StaticDynamic"
                           Nothing
                         ))
                        ($( [| x |] ))
                      |]
                    ))
               |]
             ))
        |]
mainSpecializer name arg1
  = fmap snd
      (evalRWST
         (specializer (find_BodyGen_StaticDynamic arg1) [[p| x |]]
            [lift arg1]
            "find_BodyGen_StaticDynamic"
            (Just name))
         ()
         [])