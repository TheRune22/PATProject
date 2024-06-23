{-# LANGUAGE TemplateHaskell #-}
module Foo where
import Specializer
import Language.Haskell.TH.Syntax (Q)
pow n m
  = if m == (0 :: Int) then (1 :: Int) else
      n * pow n (m - (1 :: Int))
foo x y z
  = (pow y x + pow x y) + (pow x x + pow z y) + (pow z x + pow y x)
pow_BodyGen_DynamicStatic m n
  = if m == (0 :: Int) then lift (1 :: Int) else
      [|
        ($( n )) *
          ($( [| ($( pow_BodyGen_DynamicStatic (m - (1 :: Int)) n )) |] ))
        |]
pow_BodyGen_StaticDynamic :: Int -> SpecializerMonad Exp -> SpecializerMonad Exp
pow_BodyGen_StaticDynamic n m
  = [|
      if ($( [| ($( m )) == ($( lift (0 :: Int) )) |] )) then
        ($( lift (1 :: Int) )) else
        ($(
           [|
             ($( lift n )) *
               ($(
                  [|
                    ($(
                       specializer (pow_BodyGen_StaticDynamic n [| m |]) [[p| m |]]
                         [lift n]
                         "pow_BodyGen_StaticDynamic"
                         Nothing
                       ))
                      ($( [| (($( [| ($( m )) - ($( lift (1 :: Int) )) |] ))) |] ))
                    |]
                  ))
             |]
           ))
      |]
foo_BodyGen_StaticDynamicDynamic x y z
  = [|
      ($(
         [|
           ($(
              [|
                (($(
                    [|
                      ($( [| ($( pow_BodyGen_DynamicStatic x y )) |] )) +
                        ($( [| ($( pow_BodyGen_StaticDynamic x y )) |] ))
                      |]
                    )))
                |]
              ))
             +
             ($(
                [|
                  (($(
                      [| ($( lift (pow x x) )) + ($( [| pow ($( z )) ($( y )) |] )) |]
                      )))
                  |]
                ))
           |]
         ))
        +
        ($(
           [|
             (($(
                 [|
                   ($( [| ($( pow_BodyGen_DynamicStatic x z )) |] )) +
                     ($( [| ($( pow_BodyGen_DynamicStatic x y )) |] ))
                   |]
                 )))
             |]
           ))
      |]
mainSpecializer name arg1
  = fmap snd
      (evalRWST (foo_BodyGen_StaticDynamicDynamic arg1 [| y |] [| z |])
         ()
         [])