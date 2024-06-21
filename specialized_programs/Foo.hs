{-# LANGUAGE TemplateHaskell #-}
module Foo where
import Specializer
pow n m
  = if m == (0 :: Int) then (1 :: Int) else
      n * pow n (m - (1 :: Int))
foo x y z
  = (pow y x + pow x y) + (pow x x + pow z y) + (pow z x + pow y x)
pow_BodyGen_DynamicStatic m
  = if m == (0 :: Int) then lift (1 :: Int) else
      [|
        ($( [| n |] )) *
          ($( [| ($( pow_BodyGen_DynamicStatic (m - (1 :: Int)) )) |] ))
        |]
pow_BodyGen_StaticDynamic n
  = [|
      if ($( [| ($( [| m |] )) == ($( lift (0 :: Int) )) |] )) then
        ($( lift (1 :: Int) )) else
        ($(
           [|
             ($( lift n )) *
               ($(
                  [|
                    ($(
                       specializer (pow_BodyGen_StaticDynamic n) [[p| m |]] [lift n]
                         "pow_BodyGen_StaticDynamic"
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
                              specializer (pow_BodyGen_DynamicStatic x) [[p| n |]] [lift x]
                                "pow_BodyGen_DynamicStatic"
                                Nothing
                              ))
                             ($( [| y |] ))
                           |]
                         ))
                        +
                        ($(
                           [|
                             ($(
                                specializer (pow_BodyGen_StaticDynamic x) [[p| m |]] [lift x]
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
             +
             ($(
                [|
                  (($(
                      [|
                        ($( lift (pow x x) )) +
                          ($( [| pow ($( [| z |] )) ($( [| y |] )) |] ))
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
                           specializer (pow_BodyGen_DynamicStatic x) [[p| n |]] [lift x]
                             "pow_BodyGen_DynamicStatic"
                             Nothing
                           ))
                          ($( [| z |] ))
                        |]
                      ))
                     +
                     ($(
                        [|
                          ($(
                             specializer (pow_BodyGen_DynamicStatic x) [[p| n |]] [lift x]
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