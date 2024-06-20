{-# LANGUAGE TemplateHaskell #-}
module Foo where
import Specializer
pow m n
  = if m == (0 :: Int) then (1 :: Int) else
      n * pow (m - (1 :: Int)) n
foo x y z
  = (pow x y + pow y x) + (pow x x + pow y z) + (pow x z + pow x y)
pow_BodyGen_StaticDynamic m
  = if m == (0 :: Int) then lift (1 :: Int) else
      [|
        ($( [| n |] )) *
          ($( [| ($( pow_BodyGen_StaticDynamic (m - (1 :: Int)) )) |] ))
        |]
pow_BodyGen_DynamicStatic n
  = [|
      if ($( [| ($( [| m |] )) == ($( lift (0 :: Int) )) |] )) then
        ($( lift (1 :: Int) )) else
        ($(
           [|
             ($( lift n )) *
               ($(
                  [|
                    ($(
                       specializer (pow_BodyGen_DynamicStatic n) [[p| m |]] [lift n]
                         "pow_BodyGen_DynamicStatic"
                         Nothing
                       ))
                      ($( [| (($( [| ($( [| m |] )) - ($( lift (1 :: Int) )) |] ))) |] ))
                    |]
                  ))
             |]
           ))
      |]
foo_BodyGen_StaticDynamicDynamic x
  = [|
      ($(
         [|
           ($(
              [|
                (($(
                    [|
                      ($(
                         [|
                           ($(
                              specializer (pow_BodyGen_StaticDynamic x) [[p| n |]] [lift x]
                                "pow_BodyGen_StaticDynamic"
                                Nothing
                              ))
                             ($( [| y |] ))
                           |]
                         ))
                        +
                        ($(
                           [|
                             ($(
                                specializer (pow_BodyGen_DynamicStatic x) [[p| m |]] [lift x]
                                  "pow_BodyGen_DynamicStatic"
                                  Nothing
                                ))
                               ($( [| y |] ))
                             |]
                           ))
                      |]
                    )))
                |]
              ))
             +
             ($(
                [|
                  (($(
                      [|
                        ($( lift (pow x x) )) +
                          ($( [| pow ($( [| y |] )) ($( [| z |] )) |] ))
                        |]
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
                   ($(
                      [|
                        ($(
                           specializer (pow_BodyGen_StaticDynamic x) [[p| n |]] [lift x]
                             "pow_BodyGen_StaticDynamic"
                             Nothing
                           ))
                          ($( [| z |] ))
                        |]
                      ))
                     +
                     ($(
                        [|
                          ($(
                             specializer (pow_BodyGen_StaticDynamic x) [[p| n |]] [lift x]
                               "pow_BodyGen_StaticDynamic"
                               Nothing
                             ))
                            ($( [| y |] ))
                          |]
                        ))
                   |]
                 )))
             |]
           ))
      |]
mainSpecializer name arg1
  = fmap snd
      (evalRWST
         (specializer (foo_BodyGen_StaticDynamicDynamic arg1)
            [[p| y |], [p| z |]]
            [lift arg1]
            "foo_BodyGen_StaticDynamicDynamic"
            (Just name))
         ()
         [])