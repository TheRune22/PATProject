{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main (main) where


-- TODO: add export lists
-- TODO: add tests
-- TODO: clean main, this is just for testing
-- TODO: separate BTA (maybe both TH and normal), specializer, and runner in separate executables

import Language.Haskell.Exts
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Debug.Trace

import BTA
import Utils
import Control.Monad.Reader



main :: IO ()
main = do
--  print [hExp| 1 |]
--  print $ fromParseResult $ parseExp "1"

--  parseRes <- parseFile "programs/Pow.hs"
--  print $ fromParseResult parseRes

--  print $ prettyPrint (Lit 0 (Int 1 1 "1"))

  print $ prettyPrint $ runReader (analyzeExp [hExp| a |]) [(QNameLookup $ UnQual noSrcSpan (Ident noSrcSpan "a"), Static)]


