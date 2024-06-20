{-# LANGUAGE FlexibleInstances #-}

module Specializer (
  specializer,
  evalRWST,
  lift
) where

import Language.Haskell.TH.Syntax
import Control.Monad.RWS.Lazy (evalRWST)
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


specializer :: SpecializerMonad Exp -> [Q Pat] -> [Q Exp] -> String -> Maybe String -> SpecializerMonad Exp
specializer body pats args bodyGenName maybeName = do
  argExps <- M.lift $ sequence args
  let signature = (bodyGenName, argExps)
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
