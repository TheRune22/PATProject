{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Specializer where

import Language.Haskell.TH.Syntax
import qualified Control.Monad.RWS.Lazy as M
import Data.Function ((&))
import Debug.Trace (trace)

instance (M.MonadTrans t, Quote q) => Quote (t q) where
  newName s = M.lift $ newName s

debug :: (Monad m, Show a) => a -> m ()
debug x = do
  trace (show x) $ pure ()


type SpecializerSignature = (String, [Exp])


-- TODO: remove unused parts
-- TODO: add Q to Dec? writer
type SpecializerMonad = M.RWST () [Dec] [(SpecializerSignature, Name)] Q

freshName :: String -> SpecializerMonad Name
freshName pfx = mkName . show <$> newName pfx


lookupSpecialized :: SpecializerSignature -> SpecializerMonad (Maybe Name)
lookupSpecialized signature = M.gets $ lookup signature

addSpecialized :: (SpecializerSignature, Name) -> SpecializerMonad ()
addSpecialized entry = M.modify (<>[entry])


--specializer :: Maybe String -> SpecializerMonad Exp -> [Q Pat] -> [Q Exp] -> SpecializerMonad Exp
--specializer maybeName body pats args = do
----  TODO: only add decl if not already defined, need signature
--  name <- case maybeName of
--    Just s -> pure $ mkName s
--    Nothing -> freshName "f"
--  pats' <- M.lift $ sequence pats
--  bodyExp <- body
--  let dec = FunD name [Clause pats' (NormalB bodyExp) []]
--  M.tell [dec]
--  return $ VarE name


specializer :: SpecializerMonad Exp -> [Q Pat] -> [Q Exp] -> String -> Maybe String -> SpecializerMonad Exp
specializer body pats args bodyGenName maybeName = do
--  TODO: only add decl if not already defined, need signature
--  TODO: create name from signature instead, if not possivle do as above
  argExps <- M.lift $ sequence args
  let signature = (bodyGenName, argExps)
--  debug signature
  lookupRes <- lookupSpecialized signature
  name <- case lookupRes of
    Nothing -> do
      createdName <- case maybeName of
        Just s -> pure $ mkName s
        Nothing -> freshName "f"
      pats' <- M.lift $ sequence pats
      bodyExp <- body
      let dec = FunD createdName [Clause pats' (NormalB bodyExp) []]
      M.tell [dec]
      addSpecialized (signature, createdName)
      pure createdName
    Just existingName -> pure existingName
  pure $ VarE name
