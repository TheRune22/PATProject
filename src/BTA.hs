{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module BTA where

import Utils

--import Prelude hiding (unzip, head)
--import Relude
--import Control.Lens
import Language.Haskell.Exts
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Monad.RWS.Lazy
import Data.Set (Set, member)
import Debug.Trace (trace)
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor.Syntax ((<$$>))
import Control.Monad.Identity (Identity)
import Data.Maybe (catMaybes, isJust)
import Data.List (partition)
import Control.Lens (both)
import Data.Bifunctor (bimap, first, second)
import System.Environment (getArgs)
--import Data.List.NonEmpty



-- Do now
-- TODO: read about static vs dynamic function calls
-- TODO: specialized signature as QName and list of binding times (empty list for original function used when all have same BT, or still need to distinguish?)
-- TODO: implement handling of function calls in analyzeExp
-- TODO: function for specializing single function name given dynamic args, maybe returning name of specialized function?
-- TODO: add input mudule to reader, or just decs, or map of names to bodies as exps?
-- TODO: support partial application? specialize one application at a time? would need to be able to specialize a specialized function, must support Th and avoid nesting
-- TODO: how to implement dynamic recursion?
-- TODO: should also keep track of which vars are actually functions?, binding time of functions?
-- TODO: Reader or state for keeping track of implemented functions, maybe mapping function name and dynamic args to actual name of implementation?
-- TODO: How to determine binding time of function call?
-- TODO: lift full body if fully static, in order to still return Q Exp?
-- TODO: use explicit division of all args, should make other parts easier
-- TODO: copy original function definition
-- TODO: handle pattern matching? simply inherit BT? can different parts of a pattern have different BT?
-- TODO: how to figure out which specializations to do?
-- TODO: add (hardcoded) prelude to generated file? containing e.g. function resolver
-- TODO: could just insert lambdas to avoid function resolution, but what about recursion?
-- TODO: add specializations by side effects during specialization? generated definitions then have type m Q Exp?
-- TODO:    generated program should then have a function taking name and args, returning specialized function call along with any generated definitions
-- TODO:    could use Code monadic actions
-- TODO: avoid quotes in order to avoid Q monad?
-- TODO: functions for getting suffixed names allowing for ensuring unique in the future
-- TODO: should probably just copy input module to ensure necessary imports stay, and add to the end of it


-- Do later
-- TODO: check if infinite unfolding is actually a problem
-- TODO: just use bool for binding time, i.e. isDynamic?
-- TODO: don't use monad (or components of monad) before necessary
-- TODO: decide on and clean up monad stack
-- TODO: make clean wrapper without monads, maybe merge with analyzeFunc or similar?
-- TODO: drop or modify SrcSpanInfo? Store binding time here instead or as tuple, or just ()
-- TODO: implement missing pattern matches
-- TODO: take map of function name to dynamic args, enabling handling multiple decls at once? could just map over functions to handle instead?
-- TODO: use ExpBracket or TExpBracket?
-- TODO: must also know name of function being specialized, and the name of the specialized function for recursion, or can all functions be handled equally?
-- TODO: need to be able to map original function names to specialized names? could be done by a function created at specialization?
-- TODO: must keep track of values that must be specialized to? should be done at specialization, unless simply inserting lambdas instead of function calls
-- TODO: ensure that var names can simply be preserved, and are always resolved correctly
-- TODO: ensure TH wrapping is valid
-- TODO: if always wrapping in TH if and only if dynamic just check this in caller, rather than returning binding time explicitly
-- TODO: how to communicate binding time? return explicitly, use annotation, or just infer from TH bracket use?

-- TODO: do a first pass that simply annotates all expressions with their binding time, then a second pass that generates template haskell?
-- TODO:    should subexpressions be handled differently if within larger expression of same binding time? would probably be nicer, but maybe not needed
-- TODO:    if handled same way, maybe second pass could merge subexpressions of same binding time into one?
-- TODO:    alternatively wrap in TH in caller
-- TODO:    two passes or not? only needed if subexpressions must know context, can maybe be handled by simpler second pass to merge subexpressions
-- TODO:    try single pass first, then two passes if needed
-- TODO: merge subexpressions of same binding time into one?

-- TODO: add imports to generated file: TH, lift
-- TODO: try using typed brackets?
-- TODO: look into stuff like map fusions, see supercompiler article, static function rules
-- TODO: check for undefined
-- TODO: handle functions being used as vars
-- TODO: only add parens in splices where necessary, or do always?
-- TODO: monad for handling common BTA patterns, somewhat similar to either
-- TODO: just leave unchanged as default? could just leave portion as dynamic, supplying any static values as arguments statically in a lambda
-- TODO:    would need to know which vars are static, get from division if mapping, or could be determined at specialization? maybe not with lets
-- TODO: could specialize lambdas by binding to names and replacing, and then handling as normal function call, could be done as preprocessing step
-- TODO: cannot default to static args if some are not supplied at all in partial application, should handle explicitly?
-- TODO: use record for env?
-- TODO: clean up imports, dependencies
-- TODO: Formalize syntax of subset of Haskell handled
-- TODO: assume no mutual recursion? is this needed?
-- TODO: introduce functions for each dynamic if (p. 103), handle static ifs at specialization?
-- TODO: handle other parts of the module, e.g. imports?


-- Notes
-- Monad:
-- static/dynamic division in reader monad
-- Generated code emitted in writer monad
-- Misc:
-- create (un)specialized versions of functions, as needed
-- create new specializations when new calls are found
--    only specialize local function definitions initially
-- for each decl, need to generate function generating body and function generating decl that can be called to specialize
--    second part could be done more generally, perhaps by map over generated bodies
-- if unfolding function call, must call body generating function, if not, must call final function name, should maybe be variable given at specialization?
-- variable names are preserved
-- Wrap dynamic variables in TH brackets?: yes
--    wrapping variables in TH brackets if dynamic is only possible if not given as input, and then requires name not being an argument to body generating function
--    alternatively could not wrap, but would then need exp or name as argument placeholder
-- wrap in TH in caller or callee?: wrap in TH in callee better for handling ifs with static conditional and dynamic branches
-- copy original function definition?: yes, could be used as argument, outside application
-- each function definition can have multiple divisions, each division can have multiple specializations





newtype NameLookup l = NameLookup (Name l)
  deriving (Show)

instance Eq (NameLookup l) where
--  Compare Names, ignoring SrcSpanInfo
  NameLookup n1 == NameLookup n2 = n1 =~= n2

-- TODO: map each variable to its binding time, add special cases for functions, are these also seen as vars?
data BindingTime = Static | Dynamic
  deriving (Eq, Show)

-- TODO: could just be set of dynamic variables, since static is default?
-- TODO: instantiate l or drop it? probably won't need SrcSpanInfo, but could be used for Division?
-- TODO: use QName, Name, or String?, drop l?
type Division l = [(NameLookup l, BindingTime)]

type BTASignature l = (NameLookup l, [BindingTime])


-- TODO: rename? remove l
-- TODO: use record for env?
type BTAMonad l r = RWS ((Module l, Exp l, Bool, NameLookup l), r) [Decl l] ([(BTASignature l, (String, String))], [(BTASignature l, [Pat l])])

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


--checkFuncSeen :: BTASignature l -> BTAMonad l r Bool
--checkFuncSeen signature = gets $ fst >>> fmap (first fst) >>> lookup (fst signature) >>> isJust

---- TODO: remove specializerName?
--lookupSignature :: BTASignature SrcSpanInfo -> BTAMonad SrcSpanInfo r (Maybe ((Name SrcSpanInfo, Name SrcSpanInfo), [Pat SrcSpanInfo]))
--lookupSignature sig = do
--  (namesRes, dynamicPatsRes) <- gets $ bimap (lookup sig) (lookup sig)
--  case (namesRes, dynamicPatsRes) of
--    (Just names, Just dynamicPats) ->
--      pure $ Just (names, dynamicPats)
--    _ -> pure Nothing


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
-- TODO: ensure unique by using Q monad?
getBodyGenName (NameLookup (Ident info oldName), bts) = oldName <> "_BodyGen_" <> mconcat (fmap show bts)
getBodyGenName _ = undefined

getSpecializerName :: BTASignature SrcSpanInfo -> String
-- TODO: ensure unique by using Q monad?
getSpecializerName (NameLookup (Ident info oldName), bts) = oldName <> "_Specializer_" <> mconcat (fmap show bts)
getSpecializerName _ = undefined


type BTAExpMonad l =  BTAMonad l (Division l)

-- TODO: should probably avoid SrcSpanInfo here, rename to isDynamic and just return bool?
bindingTime :: QName SrcSpanInfo -> BTAExpMonad SrcSpanInfo BindingTime
bindingTime (UnQual info name) = do
  lookupRes <- reader $ snd >>> lookup (NameLookup name)
  case lookupRes of
    Just bt -> pure bt
-- If not found, assume static, since probably a static import
    Nothing -> pure Static
-- TODO: handle differently?
bindingTime _ = pure Static



-- TODO: maybe reader should not be used before this point, should be input instead?
-- TODO: start here

analyzeExp :: Exp SrcSpanInfo -> BTAExpMonad SrcSpanInfo (BindingTime, Exp SrcSpanInfo)
-- Literals are always static
analyzeExp (Lit info lit) = pure (Static, Lit info lit)
-- Look up BT for vars
analyzeExp (Var info qName) = do
  bt <- bindingTime qName
  case bt of
-- TODO: handle case where this is a (recursive) function call? don't put brackets?
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
    -- TODO: prevent unfolding in this case? must do before analyzing branches, could use reader
    withUnfolding False (do
      (thenBT, thenExpAnalyzed) <- analyzeExp thenExp
      (elseBT, elseExpAnalyzed) <- analyzeExp elseExp
      let thenExpMaybeLifted = liftIfStatic (thenBT, thenExpAnalyzed)
      let elseExpMaybeLifted = liftIfStatic (elseBT, elseExpAnalyzed)

  --    pure (bracketTH $ If info (spliceTH condExpAnalyzed) (spliceTH thenExpMaybeLifted) (spliceTH elseExpMaybeLifted), Dynamic)
      pure (Dynamic, bracketTH $ If info $|$ condExpAnalyzed $|$ thenExpMaybeLifted $|$ elseExpMaybeLifted)
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

-- TODO: need to check for nested applications to get all arguments, but could maybe handle one at a time?
analyzeExp (App info exp1 exp2) =
  let (funExp, exps) = simplifyApp (App info exp1 exp2) in
-- TODO: also analyze funExp?
  case funExp of
  Var _ qName -> do
    -- TODO: just use analyzeSimpleExp?
    analyzeRes <- mapM analyzeExp exps
    let (bts, analyzedExps) = unzip analyzeRes
    funBT <- bindingTime qName
    if funBT == Dynamic || all (==Dynamic) bts then
--      Don't specialize, just insert dynamic call
      -- TODO: handle `all (==Dynamic) bts` case as part of spezializing?
      dynamicApp funExp analyzeRes
    else if all (==Static) bts then
--      Fully static function application, do statically
--      TODO: Copy function here specifically, or always copy all decls?
      pure (Static, applyToArgs funExp analyzedExps)
--      TODO: could possibly merge with below by using bt = if Dynamic `elem` bts then Dynamic else Static
    else
--      Specialize call by running BTA recursively and inserting call to specialized function
    -- TODO: extract below to new function or just keep here?
      case qName of
        UnQual info1 name -> do
          let signature = (NameLookup name, bts)
          let (staticArgs, dynamicArgs) = splitByBTs bts analyzedExps
          -- TODO: exploit that this cannot be recursive call when not already analyzed?
          lookupRes <- getSpecializerApp signature staticArgs nothingH
          case lookupRes of
            Just (specializerApp, True) ->
--            Unfolding call
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
-- TODO: default to simple dynamic call here instead of above?
-- TODO: assume funExp is a var? in full Haskell could also be e.g. lambda, section, constructor?, or var defined in let
  _ -> undefined
-- TODO: reuse code from above for InfixApp or just don't specialize, maybe even assume static?
analyzeExp (InfixApp info exp1 qOp exp2) = analyzeSimpleExp (\[e1, e2] -> InfixApp info e1 qOp e2) [exp1, exp2]
--analyzeExp (Let info binds exp) = undefined
--analyzeExp (Lambda info pats exp) = undefined
-- TODO: case, multiif, paren, section, TH brackets/quotes, con, do, stmts (avoiding IO?)
-- TODO: add more from https://hackage.haskell.org/package/haskell-src-exts-1.23.1/docs/Language-Haskell-Exts-Syntax.html#t:Exp
-- TODO: Try self application to get missing cases
analyzeExp (Paren info e) = analyzeSimpleExp (head >>> Paren info) [e]
analyzeExp (ExpTypeSig info e t) = analyzeSimpleExp (head >>> \e' -> ExpTypeSig info e' t) [e]
analyzeExp e = trace (show e) undefined
-- TODO: bind static vars in dynamic let binding around this to use as default
--analyzeExp e = pure (Dynamic, bracketTH e)


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




-- TODO: One specializer func or shared? initially shared
-- TODO: are specializer functions needed for every function or just outer function?
-- TODO: add functions using body gen to create decls above, use this below, need Static pats for this? - probably not
-- TODO: should have function that when given Static args of "main" function creates necessary decls



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
    -- TODO: get name from monad or elsewhere?
    -- TODO: add initial reader and state?
      let monadEval = applyToArgs (unQualVarH "evalRWST") [specializerApp, Tuple noSrcSpan Boxed [], List noSrcSpan []] in
      let body = applyToArgs (unQualVarH "fmap") [unQualVarH "snd", monadEval] in
      let mainSpecializerName = Ident noSrcSpan "mainSpecializer" in
      pure $ Just $ FunBind noSrcSpan [Match noSrcSpan mainSpecializerName ([PVar noSrcSpan specializedNameVarName] <> staticArgPats) (UnGuardedRhs noSrcSpan body) Nothing]


-- TODO: add TemplateHaskell Language Extensions to generated file, import TH, define specializer and monad with instances
-- Prelude:
--{-# LANGUAGE TemplateHaskell #-}
--import Specializer (specializer)
--import Language.Haskell.TH.Syntax (lift)
--import Control.Monad.RWS.Lazy (evalRWST)
btaModule :: Module SrcSpanInfo -> BTASignature SrcSpanInfo -> Maybe (Module SrcSpanInfo)
btaModule (Module info moduleHead pragmas imports decls) signature =
-- TODO: set initial name
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


btaFile :: FilePath -> String -> [BindingTime] -> IO ()
btaFile path fName bts = do
  parsed <- parseFile path
  case parsed of
    ParseOk m -> do
      putStrLn $ "Performing BTAfor signature " <> show (fName, bts) <> " in file " <> path
      case btaModule m (NameLookup $ Ident noSrcSpan fName, bts) of
        Nothing -> putStrLn "Function not found in module"
        Just newM -> do
          let baseName = takeWhile (/='.') path
          let newFilePath = baseName <> "_specialized.hs"
          putStrLn $ "Saving result to " <> newFilePath
          writeFile newFilePath $ prettyPrint newM
    _ -> print parsed


btaCmd :: IO ()
btaCmd = do
  args <- getArgs
  case args of
    (path : fName : btStrings) ->
      let bts = fmap (\s -> if s == "1" then Static else Dynamic) btStrings in
      btaFile path fName bts
    _ -> putStrLn "Usage: BTA <file> <fName> <1|0>..."



analyzeFunc :: BTASignature SrcSpanInfo -> BTAMonad SrcSpanInfo r (Maybe ((String, String), [Pat SrcSpanInfo]))
analyzeFunc signature = do
  -- Add to handled
  (bodyGenName, specializerName) <- addNames signature
  -- TODO: postpone recursive call and do in loop instead as seen in article?
  analyzedModule <- withRWS (\r s -> (fmap (const (signature, bodyGenName)) r, s)) analyzeModule
  case analyzedModule of
    Nothing ->
      -- Function not found in module
      pure Nothing
    Just (dynamicPats, bodyGenDecl) -> do
      -- TODO: emit code in analyzeModule instead?
      -- Add body generating function to decls
      tell [bodyGenDecl]
      -- TODO: also create decl using body generating decl to create specialized function, or wait until specialization? could use DeclBracket
      -- let specializerDecl = FunBind noSrcSpan [Match noSrcSpan specializerName ]
      pure $ Just ((bodyGenName, specializerName), dynamicPats)


---- TODO: remove specializerName?
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


-- TODO: replace lookup with this? merge with above?
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
        pure $ Just (applyToArgs (unQualVarH bodyGenName) staticArgs, True)
      else do
        -- insert call to TH specializer function
        specializerFunc <- getSpecializer
        -- TODO: this is ideal but requires separate specializer for each signature
        -- pure $ Just $ applyToArgs (unQualVarH specializerName) [unQualVarH bodyGenName, listH dynamicPats, listH staticArgs]
        let dynamicPatsExps = fmap (BracketExp noSrcSpan . PatBracket noSrcSpan) dynamicPats
        let staticArgsTH = fmap liftTH staticArgs
--        TODO: also give specializer dynamic args and control unfolding from there?
        pure $ Just (applyToArgs specializerFunc [applyToArgs (unQualVarH bodyGenName) staticArgs, listH dynamicPatsExps, listH staticArgsTH, stringH bodyGenName, maybeName], False)
  -- TODO: decide on unfolding here, or handle in specializer function? initially no unfolding?
  -- TODO: only unfold recursive calls?
    Nothing -> do
        -- Function not found in module
        pure Nothing






--type BTAFuncMonad = ReaderT (Module SrcSpanInfo, (Name SrcSpanInfo, [BindingTime])) (Writer [Decl SrcSpanInfo])
type BTAFuncMonad l = BTAMonad l (BTASignature SrcSpanInfo, String)

-- TODO: separate lookup and analysis?
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

---- TODO: don't specialize if all args are static or dynamic, just copy code
analyzeDecl :: Decl SrcSpanInfo -> BTAFuncMonad SrcSpanInfo (Maybe ([Pat SrcSpanInfo], Decl SrcSpanInfo))
-- TODO: if Let is implemented, also handle binds here in same way?
analyzeDecl (FunBind info1 [Match info2 name pats rhs Nothing]) = do
  (_, ((oldName, bts), newName)) <- ask
  if NameLookup name == oldName then do
    -- default to dynamic if less bts than pats for partial application
    let division = zip (bts <> repeat Dynamic) pats >>= uncurry analyzePat

    let (staticPats, dynamicPats) = splitByBTs bts pats

    -- Add dynamic pats to state
    addDynamicPats (oldName, bts) dynamicPats

    analyzedRhs <- analyzeRhs division rhs
    -- TODO: just discard dynamic pats, replace by simple var, or give to specializer somehow? need to be able to recover original structure and var names
    -- TODO: could convert Pats to TH pats using PatBracket and pass as arguments here?
    -- TODO: change name, i.e. add suffix to show body generating?
--    TODO: fmap (BracketExp noSrcSpan . PatBracket noSrcSpan) dynamicPats here or later?
    pure $ Just (dynamicPats, FunBind info1 [Match info2 (Ident noSrcSpan newName) staticPats analyzedRhs Nothing])
  else
    pure Nothing
-- TODO: handle this? could reuse from prepMatch
--prepDecl (PatBind info pat rhs Nothing) = undefined
-- TODO: return Nothing as default instead?
analyzeDecl _ = pure Nothing

analyzePat :: BindingTime -> Pat SrcSpanInfo -> Division SrcSpanInfo
analyzePat bt (PVar info name) = [(NameLookup name, bt)]
analyzePat bt (PApp info qName pats) = pats >>= analyzePat bt
analyzePat bt (PTuple info boxed pats) = pats >>= analyzePat bt
analyzePat bt (PList info pats) = pats >>= analyzePat bt
---- TODO: handle more cases
analyzePat _ _ = undefined

analyzeRhs :: Division SrcSpanInfo -> Rhs SrcSpanInfo -> BTAFuncMonad SrcSpanInfo (Rhs SrcSpanInfo)
analyzeRhs division (UnGuardedRhs info exp) = UnGuardedRhs info <$> analyzeRhsHelper division exp
analyzeRhs _ _ = undefined
--analyzeRhs division (GuardedRhss info guardedRhss) = GuardedRhss info <$> mapM (analyzeGuardedRhs division) guardedRhss
--
--analyzeGuardedRhs :: Division SrcSpanInfo -> GuardedRhs SrcSpanInfo -> BTAFuncMonad (GuardedRhs SrcSpanInfo)
---- TODO: can just ignore guards? must check for both dynamic and static variables
--analyzeGuardedRhs division (GuardedRhs info stmts exp) = GuardedRhs info stmts <$> analyzeRhsHelper division exp

analyzeRhsHelper :: Division SrcSpanInfo -> Exp SrcSpanInfo -> BTAFuncMonad SrcSpanInfo (Exp SrcSpanInfo)
analyzeRhsHelper division exp = do
--  analyzedExp <- withReaderT (fmap (const division)) $ analyzeExp exp
  analyzedExp <- withRWS (\r s -> (fmap (const division) r, s)) $ analyzeExp exp
--  TODO: if static, lift? is this right?
  pure $ liftIfStatic analyzedExp




-- TODO: move some to Utils?
-- Helpers
liftIfStatic :: (BindingTime, Exp SrcSpanInfo) -> Exp SrcSpanInfo
liftIfStatic (Static, e) = liftTH e
liftIfStatic (Dynamic, e) = e

-- Gather expression being applied along with argument list from nested applications
simplifyApp :: Exp SrcSpanInfo -> (Exp SrcSpanInfo, [Exp SrcSpanInfo])
simplifyApp (App _ e1 e2) = (<>[e2]) <$> simplifyApp e1
simplifyApp e = (e, [])

-- Inverse of above
applyToArgs :: Exp SrcSpanInfo -> [Exp SrcSpanInfo] -> Exp SrcSpanInfo
applyToArgs = foldl (App noSrcSpan)

splitByBTs :: [BindingTime] -> [a] -> ([a], [a])
splitByBTs bts xs = bimap (fmap snd) (fmap snd) $ partition (fst >>> (==Static)) $ zip bts xs


-- TODO: check if actually needed elsewhere, could generalize to list?
-- Ensures that both expressions have same binding time, lifting Static to Dynamic if needed
--normalizeBT :: (Exp SrcSpanInfo, BindingTime) -> (Exp SrcSpanInfo, BindingTime) -> (Exp SrcSpanInfo, Exp SrcSpanInfo, BindingTime)
--normalizeBT (exp1, bt1) (exp2, bt2) =
--  case (bt1, bt2) of
--    (Static, Static) -> (exp1, exp2, Static)
--    (Static, Dynamic) -> (liftTH exp1, exp2, Dynamic)
--    (Dynamic, Static) -> (exp1, liftTH exp2, Dynamic)
---- TODO: Could potentially remove brackets here?
--    (Dynamic, Dynamic) -> (exp1, exp2, Dynamic)


-- TODO: rename to splice, wrap, insert?
-- TODO: what information is needed to determine whether to do nothing, lift, or splice?
-- Helper for composing TH expressions
--spliceExp :: BindingTime -> BindingTime -> Exp SrcSpanInfo -> Exp SrcSpanInfo
--spliceExp contextBT expBT exp =
--  case (contextBT, expBT) of
--    (Static, Static) -> spliceTH exp
----    (Static, Dynamic) -> exp
----    (Dynamic, Static) -> exp
----    (Dynamic, Dynamic) -> exp
---- TODO: check this
--    _ -> exp
