{-# LANGUAGE TemplateHaskell #-}
module Pow where
import Specializer
pow m n
  = if m == (0 :: Int) then (1 :: Int) else
      n * pow (m - (1 :: Int)) n
pow_BodyGen_StaticDynamic m
  = if m == (0 :: Int) then lift (1 :: Int) else
      [|
        ($( [| n |] )) *
          ($( [| ($( pow_BodyGen_StaticDynamic (m - (1 :: Int)) )) |] ))
        |]
mainSpecializer name arg1
  = fmap snd
      (evalRWST
         (specializer (pow_BodyGen_StaticDynamic arg1) [[p| n |]]
            [lift arg1]
            "pow_BodyGen_StaticDynamic"
            (Just name))
         ()
         [])