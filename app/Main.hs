{-# LANGUAGE TemplateHaskell, QuasiQuotes, TupleSections #-}
module Main (main) where


-- TODO: add export lists
-- TODO: add tests
-- TODO: clean main, this is just for testing
-- TODO: separate BTA (maybe both TH and normal), specializer, and runner in separate executables

import BTA
import Utils

import qualified Language.Haskell.Exts as H
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Debug.Trace
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Arrow ((>>>), first)
import Data.Function ((&))


showBTAResult :: H.Exp H.SrcSpanInfo -> [String] -> (BindingTime, String, [String])
showBTAResult exp dynamicNames =
-- TODO: also take explicitly static names as input
  let dynamicNames' = fmap ((,Dynamic) . QNameLookup . H.UnQual H.noSrcSpan . H.Ident H.noSrcSpan) dynamicNames in
  analyzeExp exp
  & flip runReaderT (dynamicNames', ())
  & runWriter
  & fst
  & fmap H.prettyPrint
  & \(a, b) -> (a, b, dynamicNames)


main :: IO ()
main = do
--  print [hExp| 1 `a` 2 |]
--  print $ H.prettyPrint [hExp| a 1 2 |]
--  print $ showBTAResult [hExp| if a then b else c |] ["a", "b", "c"]
--  print $ showBTAResult [hExp| if a then b else c |] ["a", "b"]
--  print $ showBTAResult [hExp| if a then b else c |] ["a"]
--  print $ showBTAResult [hExp| if a then b else c |] ["b", "c"]
--  print $ showBTAResult [hExp| if a then b else c |] ["b"]
--  print $ showBTAResult [hExp| if a then b else c |] []
--  print $ showBTAResult [hExp| -a |] []
--  print $ showBTAResult [hExp| -a |] ["a"]
--  print $ showBTAResult [hExp| [1, 2, a] |] ["a"]
--  print $ simplifyApp [hExp| f (1 2) 3 |]
  print $ prepModule [hModule| module Test where f x = x |] & flip runReaderT (H.Ident H.noSrcSpan "f", [])
