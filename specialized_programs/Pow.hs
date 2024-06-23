{-# LANGUAGE TemplateHaskell #-}
module Pow where
import Specializer
pow n m
  = if m == (0 :: Int) then (1 :: Int) else
      n * pow n (m - (1 :: Int))
pow_BodyGen_DynamicStatic m
  = if m == (0 :: Int) then lift (1 :: Int) else
      [| n * ($( pow_BodyGen_DynamicStatic (m - (1 :: Int)) )) |]
mainSpecializer name arg1
  = fmap snd
      (evalRWST
         (specializer (pow_BodyGen_DynamicStatic arg1) [[p| n |]]
            [lift arg1]
            "pow_BodyGen_DynamicStatic"
            (Just name))
         ()
         [])