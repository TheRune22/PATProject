{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main (main) where


-- TODO: add export lists
-- TODO: add tests
-- TODO: clean main, this is just for testing
-- TODO: separate BTA (maybe both TH and normal), specializer, and runner in separate executables

import BTA
import Utils

import Language.Haskell.Exts
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Debug.Trace
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Arrow ((>>>))
import Data.Function ((&))



main :: IO ()
main = do
  print $ [hExp| reader 1 2 |]
--  print $ fromParseResult $ parseExp "1"

--  parseRes <- parseFile "programs/Pow.hs"
--  print $ fromParseResult parseRes

--  print $ prettyPrint (Lit 0 (Int 1 1 "1"))

--  print $ prettyPrint $ fst $ runWriter $ runReaderT (analyzeExp [hExp| a |]) ([(QNameLookup $ UnQual noSrcSpan (Ident noSrcSpan "a"), Static)], ())


