{-# LANGUAGE BlockArguments #-}
module BTA where

import Utils


import Language.Haskell.Exts
import Control.Monad.RWS.Lazy
import Debug.Trace (trace)
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.List (partition)
import Data.Bifunctor (bimap, first, second)
import System.Environment (getArgs)


-- Monads and utilities:
newtype NameLookup l = NameLookup (Name l)
  deriving (Show)

instance Eq (NameLookup l) where
--  Compare Names, ignoring SrcSpanInfo
  NameLookup n1 == NameLookup n2 = n1 =~= n2

data BindingTime = Static | Dynamic
  deriving (Eq, Show)


type Division l = [(NameLookup l, BindingTime)]

type BTASignature l = (NameLookup l, [BindingTime])

type BTAMonad l r = RWS ((Module l, Exp l, Bool, NameLookup l), r) [Decl l] ([(BTASignature l, (String, String))], [(BTASignature l, [Pat l])])
type BTAExpMonad l =  BTAMonad l (Division l)
type BTAFuncMonad l = BTAMonad l (BTASignature SrcSpanInfo, String)

getModule :: BTAMonad l r (Module l)
getModule = reader (fst >>> \(x, _, _, _) -> x)

getSpecializer :: BTAMonad l r (Exp l)
getSpecializer = reader (fst >>> \(_, x, _, _) -> x)

getUnfolding :: BTAMonad l r Bool
getUnfolding = reader (fst >>> \(_, _, x, _) -> x)

getAnalyzed :: BTAMonad l r (NameLookup l)
getAnalyzed = reader (fst >>> \(_, _, _, x) -> x)

withUnfolding :: Bool -> BTAMonad l r a -> BTAMonad l r a
withUnfolding b = withRWS (\r s -> (first (\(x, y, _, z) -> (x, y, b, z)) r, s))

withAnalyzed :: NameLookup l -> BTAMonad l r a -> BTAMonad l r a
withAnalyzed n = withRWS (\r s -> (first (\(x, y, z, _) -> (x, y, z, n)) r, s))


addNames :: BTASignature SrcSpanInfo -> BTAMonad SrcSpanInfo r (String, String)
addNames sig = do
  let bodyGenName = getBodyGenName sig
  let specializerName = getSpecializerName sig
  modify $ first (<>[(sig, (bodyGenName, specializerName))])
  pure (bodyGenName, specializerName)

addDynamicPats :: BTASignature SrcSpanInfo -> [Pat SrcSpanInfo] -> BTAMonad SrcSpanInfo r ()
addDynamicPats sig dynamicPats = do
  modify $ second (<>[(sig, dynamicPats)])
  pure mempty

getBodyGenName :: BTASignature SrcSpanInfo -> String
getBodyGenName (NameLookup (Ident info oldName), bts) = oldName <> "_BodyGen_" <> mconcat (fmap show bts)
getBodyGenName _ = undefined

getSpecializerName :: BTASignature SrcSpanInfo -> String
getSpecializerName (NameLookup (Ident info oldName), bts) = oldName <> "_Specializer_" <> mconcat (fmap show bts)
getSpecializerName _ = undefined


bindingTime :: QName SrcSpanInfo -> BTAExpMonad SrcSpanInfo BindingTime
bindingTime (UnQual info name) = do
  lookupRes <- reader $ snd >>> lookup (NameLookup name)
  case lookupRes of
    Just bt -> pure bt
-- If not found, assume static, since probably a static import
    Nothing -> pure Static
bindingTime _ = pure Static


liftIfStatic :: (BindingTime, Exp SrcSpanInfo) -> Exp SrcSpanInfo
liftIfStatic (Static, e) = liftTH e
liftIfStatic (Dynamic, e) = e

splitByBTs :: [BindingTime] -> [a] -> ([a], [a])
splitByBTs bts xs = bimap (fmap snd) (fmap snd) $ partition (fst >>> (==Static)) $ zip bts xs


-- Analysis API:
btaCmd :: IO ()
btaCmd = do
  args <- getArgs
  case args of
    (pathIn : pathOut : fName : btStrings) ->
      let bts = fmap (\s -> if s == "1" then Static else Dynamic) btStrings in
      btaFile pathIn pathOut fName bts
    _ -> putStrLn "Usage: BTA <pathIn> <pathOut> <fName> <1|0>..."

btaFile :: FilePath -> FilePath -> String -> [BindingTime] -> IO ()
btaFile pathIn pathOut fName bts = do
  parsed <- parseFile pathIn
  case parsed of
    ParseOk m -> do
      putStrLn $ "Performing BTAfor signature " <> show (fName, bts) <> " in file " <> pathIn
      case btaModule m (NameLookup $ Ident noSrcSpan fName, bts) of
        Nothing -> putStrLn "Function not found in module"
        Just newM -> do
          putStrLn $ "Saving result to " <> pathOut
          writeFile pathOut $ prettyPrint newM
    _ -> print parsed

btaModule :: Module SrcSpanInfo -> BTASignature SrcSpanInfo -> Maybe (Module SrcSpanInfo)
btaModule (Module info moduleHead pragmas imports decls) signature =
  case runRWS (getMainSpecializer signature) ((Module info moduleHead pragmas imports decls, unQualVarH "specializer", True, NameLookup $ Ident noSrcSpan "Test"), []) ([], []) of
    (Just mainDecl, _, generatedDecls) ->
       let specializerImport = ImportDecl {
        importAnn = noSrcSpan,
        importModule = ModuleName noSrcSpan "Specializer",
        importQualified = False,
        importSrc = False,
        importSafe = False,
        importPkg = Nothing,
        importAs = Nothing,
        importSpecs = Nothing
      } in
      Just $ Module info moduleHead (pragmas <> [LanguagePragma noSrcSpan [Ident noSrcSpan "TemplateHaskell"]]) (imports <> [specializerImport]) (decls <> generatedDecls <> [mainDecl])
    _ ->
      Nothing
btaModule _ _ = undefined

getMainSpecializer :: BTASignature SrcSpanInfo -> BTAMonad SrcSpanInfo r (Maybe (Decl SrcSpanInfo))
getMainSpecializer signature = do
  let numStaticArgs = filter (==Static) (snd signature) & length
  let specializedNameVarName = Ident noSrcSpan "name"
  let staticArgNames = [1..numStaticArgs] & fmap (Ident noSrcSpan . ("arg" <>) . show)
  let staticArgPats = fmap (PVar noSrcSpan) staticArgNames
  let staticArgExps = fmap (Var noSrcSpan . UnQual noSrcSpan) staticArgNames
  lookupRes <- getSpecializerApp signature staticArgExps $ justH $ Var noSrcSpan $ UnQual noSrcSpan specializedNameVarName
  case lookupRes of
    Nothing ->
      pure Nothing
    Just (specializerApp, _) ->
      let monadEval = applyToArgs (unQualVarH "evalRWST") [specializerApp, Tuple noSrcSpan Boxed [], List noSrcSpan []] in
      let body = applyToArgs (unQualVarH "fmap") [unQualVarH "snd", monadEval] in
      let mainSpecializerName = Ident noSrcSpan "mainSpecializer" in
      pure $ Just $ FunBind noSrcSpan [Match noSrcSpan mainSpecializerName ([PVar noSrcSpan specializedNameVarName] <> staticArgPats) (UnGuardedRhs noSrcSpan body) Nothing]

getSpecializerApp :: BTASignature SrcSpanInfo -> [Exp SrcSpanInfo] -> Exp SrcSpanInfo -> BTAMonad SrcSpanInfo r (Maybe (Exp SrcSpanInfo, Bool))
getSpecializerApp signature staticArgs maybeName = do
  lookupRes <- findOrDefine signature
  case lookupRes of
    Just ((bodyGenName, specializerName), dynamicPats) -> do
      unfoldingEnabled <- getUnfolding
      analyzedName <- getAnalyzed
      let recursiveCall = analyzedName == fst signature
      let shouldUnfold = unfoldingEnabled && recursiveCall
      if shouldUnfold then
--        TODO: should also give dynamic args to bodyGen
        pure $ Just (applyToArgs (unQualVarH bodyGenName) staticArgs, True)
      else do
        -- insert call to TH specializer function
        specializerFunc <- getSpecializer
        let dynamicPatsExps = fmap (BracketExp noSrcSpan . PatBracket noSrcSpan) dynamicPats
        let staticArgsTH = fmap liftTH staticArgs
--        TODO: should also give dynamic args to bodyGen
        pure $ Just (applyToArgs specializerFunc [applyToArgs (unQualVarH bodyGenName) staticArgs, listH dynamicPatsExps, listH staticArgsTH, stringH bodyGenName, maybeName], False)
    Nothing -> do
        -- Function not found in module
        pure Nothing

findOrDefine :: BTASignature SrcSpanInfo -> BTAMonad SrcSpanInfo r (Maybe ((String, String), [Pat SrcSpanInfo]))
findOrDefine signature = do
  (namesRes, dynamicPatsRes) <- gets $ bimap (lookup signature) (lookup signature)
  case (namesRes, dynamicPatsRes) of
    (Just names, Just dynamicPats) ->
--    Already analyzed, return result
      pure $ Just (names, dynamicPats)
    _ ->
--    New signature, analyze with unfolding enabled
      withAnalyzed (fst signature) $ withUnfolding True $ analyzeFunc signature

analyzeFunc :: BTASignature SrcSpanInfo -> BTAMonad SrcSpanInfo r (Maybe ((String, String), [Pat SrcSpanInfo]))
analyzeFunc signature = do
  -- Add to handled
  (bodyGenName, specializerName) <- addNames signature
  analyzedModule <- withRWS (\r s -> (fmap (const (signature, bodyGenName)) r, s)) analyzeModule
  case analyzedModule of
    Nothing ->
      -- Function not found in module
      pure Nothing
    Just (dynamicPats, bodyGenDecl) -> do
      -- Add body generating function to decls
      tell [bodyGenDecl]
      pure $ Just ((bodyGenName, specializerName), dynamicPats)

analyzeModule :: BTAFuncMonad SrcSpanInfo (Maybe ([Pat SrcSpanInfo], Decl SrcSpanInfo))
analyzeModule = do
  m <- getModule
  case m of
    (Module info moduleHead pragmas imports decls) -> do
      analyzedDecls <- catMaybes <$> mapM analyzeDecl decls
      case analyzedDecls of
        [] -> pure Nothing
        [x] -> pure $ Just x
--        Only one matching decl is assumed to exist
        _ -> undefined
    _ -> undefined

analyzeDecl :: Decl SrcSpanInfo -> BTAFuncMonad SrcSpanInfo (Maybe ([Pat SrcSpanInfo], Decl SrcSpanInfo))
analyzeDecl (FunBind info1 [Match info2 name pats rhs Nothing]) = do
  (_, ((oldName, bts), newName)) <- ask
  if NameLookup name == oldName then do
    -- default to dynamic if less bts than pats for partial application
    let division = zip (bts <> repeat Dynamic) pats >>= uncurry analyzePat
    let (staticPats, dynamicPats) = splitByBTs bts pats

    -- Add dynamic pats to state
    addDynamicPats (oldName, bts) dynamicPats

    analyzedRhs <- analyzeRhs division rhs
    pure $ Just (dynamicPats, FunBind info1 [Match info2 (Ident noSrcSpan newName) staticPats analyzedRhs Nothing])
  else
    pure Nothing
analyzeDecl _ = pure Nothing

analyzePat :: BindingTime -> Pat SrcSpanInfo -> Division SrcSpanInfo
analyzePat bt (PVar info name) = [(NameLookup name, bt)]
analyzePat _ _ = undefined

analyzeRhs :: Division SrcSpanInfo -> Rhs SrcSpanInfo -> BTAFuncMonad SrcSpanInfo (Rhs SrcSpanInfo)
analyzeRhs division (UnGuardedRhs info exp) = UnGuardedRhs info <$> analyzeRhsHelper division exp
analyzeRhs _ _ = undefined

analyzeRhsHelper :: Division SrcSpanInfo -> Exp SrcSpanInfo -> BTAFuncMonad SrcSpanInfo (Exp SrcSpanInfo)
analyzeRhsHelper division exp = do
  analyzedExp <- withRWS (\r s -> (fmap (const division) r, s)) $ analyzeExp exp
  pure $ liftIfStatic analyzedExp


-- Analysis of expressions:
analyzeExp :: Exp SrcSpanInfo -> BTAExpMonad SrcSpanInfo (BindingTime, Exp SrcSpanInfo)
-- Literals are always static
analyzeExp (Lit info lit) = pure (Static, Lit info lit)
-- Look up BT for vars
analyzeExp (Var info qName) = do
  bt <- bindingTime qName
  case bt of
    Static -> pure (Static, Var info qName)
    Dynamic -> pure (Dynamic, bracketTH $ Var info qName)
-- Simple cases
analyzeExp (NegApp info e) = analyzeSimpleExp (head >>> NegApp info) [e]
analyzeExp (List info exps) = analyzeSimpleExp (List info) exps
analyzeExp (Tuple info boxed exps) = analyzeSimpleExp (Tuple info boxed) exps
-- Control flow
analyzeExp (If info condExp thenExp elseExp) = do
  (condBT, condExpAnalyzed) <- analyzeExp condExp

  if condBT == Dynamic then
    -- condition is dynamic, must be evaluated at runtime
    withUnfolding False (do
      (thenBT, thenExpAnalyzed) <- analyzeExp thenExp
      (elseBT, elseExpAnalyzed) <- analyzeExp elseExp
      let thenExpMaybeLifted = liftIfStatic (thenBT, thenExpAnalyzed)
      let elseExpMaybeLifted = liftIfStatic (elseBT, elseExpAnalyzed)
      pure (Dynamic, bracketTH $ If info (spliceTH condExpAnalyzed) (spliceTH thenExpMaybeLifted) (spliceTH elseExpMaybeLifted))
      )
  else do
    (thenBT, thenExpAnalyzed) <- analyzeExp thenExp
    (elseBT, elseExpAnalyzed) <- analyzeExp elseExp
    let thenExpMaybeLifted = liftIfStatic (thenBT, thenExpAnalyzed)
    let elseExpMaybeLifted = liftIfStatic (elseBT, elseExpAnalyzed)
    if thenBT == Dynamic || elseBT == Dynamic then
      -- condition is static, can be evaluated at compile time, but branches are dynamic
      pure (Dynamic, If info condExpAnalyzed thenExpMaybeLifted elseExpMaybeLifted)
    else
      -- fully static if
      pure (Static, If info condExpAnalyzed thenExpAnalyzed elseExpAnalyzed)
analyzeExp (App info exp1 exp2) =
  let (funExp, exps) = simplifyApp (App info exp1 exp2) in
  case funExp of
  Var _ qName -> do
    analyzeRes <- mapM analyzeExp exps
    let (bts, analyzedExps) = unzip analyzeRes
    funBT <- bindingTime qName
    if funBT == Dynamic then
--      Don't specialize, just insert dynamic call
      dynamicApp funExp analyzeRes
    else if all (==Static) bts then
--      Fully static function application, do statically
      pure (Static, applyToArgs funExp analyzedExps)
    else
--      Specialize call by running BTA recursively and inserting call to specialized function
      case qName of
        UnQual info1 name -> do
          let signature = (NameLookup name, bts)
          let (staticArgs, dynamicArgs) = splitByBTs bts analyzedExps
          lookupRes <- getSpecializerApp signature staticArgs nothingH
          case lookupRes of
            Just (specializerApp, True) ->
--            Unfolding call
--            TODO: this is wrong if dynamic args are not the same as for the function call being handled currently
              pure (Dynamic, bracketTH $ spliceTH specializerApp)
            Just (specializerApp, False) ->
--            Not unfolding call
              pure (Dynamic, bracketTH $ applyToArgs (spliceTH specializerApp) (fmap spliceTH dynamicArgs))
            Nothing ->
              -- Function not found in module, probably an import, so dont specialize
              dynamicApp funExp analyzeRes
        _ ->
--        Don't specialize qualified names
          dynamicApp funExp analyzeRes
  _ -> undefined
analyzeExp (InfixApp info exp1 qOp exp2) = analyzeSimpleExp (\[e1, e2] -> InfixApp info e1 qOp e2) [exp1, exp2]
analyzeExp (Paren info e) = analyzeSimpleExp (head >>> Paren info) [e]
analyzeExp (ExpTypeSig info e t) = analyzeSimpleExp (head >>> \e' -> ExpTypeSig info e' t) [e]
analyzeExp e = trace (show e) undefined

dynamicApp :: Exp SrcSpanInfo -> [(BindingTime, Exp SrcSpanInfo)] -> BTAExpMonad SrcSpanInfo (BindingTime, Exp SrcSpanInfo)
dynamicApp funExp analyzeRes = pure (Dynamic, bracketTH $ applyToArgs funExp (fmap (liftIfStatic >>> spliceTH) analyzeRes))

analyzeSimpleExp :: ([Exp SrcSpanInfo] -> Exp SrcSpanInfo) -> [Exp SrcSpanInfo] -> BTAExpMonad SrcSpanInfo (BindingTime, Exp SrcSpanInfo)
analyzeSimpleExp constructor exps = do
  analyzeRes <- mapM analyzeExp exps
  let (bts, analyzedExps) = unzip analyzeRes
  if Dynamic `elem` bts then
    pure (Dynamic, bracketTH $ constructor $ fmap (liftIfStatic >>> spliceTH) analyzeRes)
  else
    pure (Static, constructor analyzedExps)

