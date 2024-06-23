{-# LANGUAGE TemplateHaskell #-}
module Map where
import Specializer
mapL op l
  = if l == ([] :: [Int]) then ([] :: [Int]) else
      op (head l) : mapL op (tail l)
mapL_BodyGen_DynamicStatic l
  = if l == ([] :: [Int]) then lift ([] :: [Int]) else
      [|
        op ($( lift (head l) )) :
          ($( mapL_BodyGen_DynamicStatic (tail l) ))
        |]
mainSpecializer name arg1
  = fmap snd
      (evalRWST
         (specializer (mapL_BodyGen_DynamicStatic arg1) [[p| op |]]
            [lift arg1]
            "mapL_BodyGen_DynamicStatic"
            (Just name))
         ()
         [])