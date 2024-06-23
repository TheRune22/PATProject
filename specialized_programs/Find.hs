{-# LANGUAGE TemplateHaskell #-}
module Find where
import Specializer
find s x
  = if null s then (0 :: Int) else
      if head s == x then (0 :: Int) else (1 :: Int) + find (tail s) x
find_BodyGen_StaticDynamic s
  = if null s then lift (0 :: Int) else
      [|
        if ($( lift (head s) )) == x then ($( lift (0 :: Int) )) else
          ($( lift (1 :: Int) )) +
            ($(
               specializer (find_BodyGen_StaticDynamic (tail s)) [[p| x |]]
                 [lift (tail s)]
                 "find_BodyGen_StaticDynamic"
                 Nothing
               ))
              x
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