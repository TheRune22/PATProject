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

-- TODO: merge Division and functionEnv if we need to look up functions same way as variables?
--type FunctionEnv l = ()






-- TODO: rename? remove l
--type BTAMonad l = Reader (Division l)
-- TODO: use record for env?
-- TODO: function env in state instead? flip reader and writer?
type BTAMonad l = ReaderT (Division l, Module l) (Writer [Decl l])
-- TODO: use this instead
--type BTAMonad l = ReaderT (Division l, Module l) (Writer [Decl l])
--type BTAMonad l = WriterT [Decl l] (Reader (Division l, FunctionEnv l))
--type BTAMonad l = RWS (Division l) [Decl l] (FunctionEnv l)


getModule :: BTAMonad l (Module l)
getModule = reader snd


-- TODO: should probably avoid SrcSpanInfo here, rename to isDynamic and just return bool?
bindingTime :: QName SrcSpanInfo -> BTAMonad SrcSpanInfo BindingTime
bindingTime (UnQual info name) = do
  lookupRes <- reader $ fst >>> lookup (NameLookup name)
  case lookupRes of
    Just bt -> pure bt
-- If not found, assume static, since probably a static import
    Nothing -> pure Static
-- TODO: handle differently?
bindingTime _ = pure Static





-- TODO: maybe reader should not be used before this point, should be input instead?
-- TODO: start here

analyzeExp :: Exp SrcSpanInfo -> BTAMonad SrcSpanInfo (BindingTime, Exp SrcSpanInfo)
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
  (thenBT, thenExpAnalyzed) <- analyzeExp thenExp
  (elseBT, elseExpAnalyzed) <- analyzeExp elseExp
  let thenExpMaybeLifted = liftIfStatic (thenBT, thenExpAnalyzed)
  let elseExpMaybeLifted = liftIfStatic (elseBT, elseExpAnalyzed)

  if condBT == Dynamic then
    -- condition is dynamic, must be evaluated at runtime
    -- TODO: prevent unfolding in this case? must do before analyzing branches, could use reader
--    pure (bracketTH $ If info (spliceTH condExpAnalyzed) (spliceTH thenExpMaybeLifted) (spliceTH elseExpMaybeLifted), Dynamic)
    pure (Dynamic, bracketTH $ If info $|$ condExpAnalyzed $|$ thenExpMaybeLifted $|$ elseExpMaybeLifted)
  else if thenBT == Dynamic || elseBT == Dynamic then
    -- condition is static, can be evaluated at compile time, but branches are dynamic
    pure (Dynamic, If info condExpAnalyzed thenExpMaybeLifted elseExpMaybeLifted)
  else
    -- fully static if
    pure (Static, If info condExpAnalyzed thenExpAnalyzed elseExpAnalyzed)

-- TODO: need to check for nested applications to get all arguments, but could maybe handle one at a time?
analyzeExp (App info exp1 exp2) =
  let (funExp, exps) = simplifyApp (App info exp1 exp2) in
-- TODO: assume funExp is a var? in full Haskell could also be e.g. lambda, section, constructor?, or var defined in let
-- TODO: decide on unfolding here, or handle in specializer function?
-- TODO: ensure correct order of static and dynamic args
-- Notes:
-- initially no unfolding?
  case funExp of
-- TODO: handle Qualified names?
  Var _ (UnQual info1 name) -> do
    -- TODO: just use analyzeSimpleExp?
    analyzeRes <- mapM analyzeExp exps
    let (bts, analyzedExps) = unzip analyzeRes
    -- TODO: call BTA recursively
    btaFunc name bts
    let bt = if Dynamic `elem` bts then Dynamic else Static
    -- TODO: insert call to specializer function in TH instead of just funExp, unless fully static (or fully dynamic?)
    --  Maybe supply arguments properly by undoing listing? probably needs bts as well
    pure (bt, App info funExp (List noSrcSpan analyzedExps))
  _ -> undefined
-- TODO: reuse code from above for InfixApp
--analyzeExp (InfixApp info exp1 qOp exp2) = undefined
--analyzeExp (Let info binds exp) = undefined
--analyzeExp (Lambda info pats exp) = undefined
-- TODO: case, multiif, paren, section, TH brackets/quotes, con, do, stmts (avoiding IO?)
-- TODO: add more from https://hackage.haskell.org/package/haskell-src-exts-1.23.1/docs/Language-Haskell-Exts-Syntax.html#t:Exp
-- TODO: Try self application to get missing cases
analyzeExp _ = undefined
-- TODO: bind static vars in dynamic let binding around this to use as default
--analyzeExp e = pure (Dynamic, bracketTH e)





--TODO: rename to analyzeFunc?
btaFunc :: Name SrcSpanInfo -> [BindingTime] -> BTAMonad SrcSpanInfo ()
btaFunc name bts =
-- TODO: don't specialize if all args are static or dynamic, just copy code?
-- Check if already analyzed
-- Lookup qName in module
-- Translate bts to mapping of qNames for env
-- Mark as handled
-- Analyze RHS
-- Emit code
-- TODO: return name of specialized function?
  undefined



-- TODO: reconstruct decl into body generating decl
-- TODO: also create decl using body generating decl to create specialized function, or wait until specialization?

-- TODO: Handle like exp in order to reconstruct decl?
-- TODO: use this below? or can avoid if reconstructing?
--lookupFunc :: Name SrcSpanInfo -> Module SrcSpanInfo -> [Decl SrcSpanInfo]

-- TODO: should be able to reconstruct Decl, maybe already do here?
-- TODO: handle empty, i.e. definition not found?
prepFunc :: Module SrcSpanInfo -> Name SrcSpanInfo -> [BindingTime] -> [(Division SrcSpanInfo, Exp SrcSpanInfo)]
prepFunc m n bts =
  prepModule m & flip runReaderT (n, bts)


type PrepMonad = ReaderT (Name SrcSpanInfo, [BindingTime]) [] (Division SrcSpanInfo, Exp SrcSpanInfo)

-- TODO: rename?
-- TODO: do as preprocessing or on demand?
-- TODO: get QName and Module from reader instead? create wrapper using monad (reader, writer)? do above?
prepModule :: Module SrcSpanInfo -> PrepMonad
-- TODO: handle other parts of the module?
prepModule (Module info moduleHead pragmas imports decls) = lift decls >>= prepDecl
prepModule _ = undefined

---- TODO: don't specialize if all args are static or dynamic, just copy code
prepDecl :: Decl SrcSpanInfo -> PrepMonad
-- TODO: check FunBind vs PatBind
-- TODO: only handle single match if too hard?
prepDecl (FunBind info matches) = lift matches >>= prepMatch
-- TODO: handle this? could reuse from prepMatch
--prepDecl (PatBind info pat rhs Nothing) = undefined
prepDecl _ = undefined

prepMatch :: Match SrcSpanInfo -> PrepMonad
-- TODO: how to handle pats, only do simple cases?
-- TODO: if Let is implemented, also handle binds here in same way?
-- TODO: handle guards? could probably just ignore
prepMatch (Match info1 name pats rhs Nothing) = do
  (qName, bts) <- ask
  if name =~= qName then
    let division = liftA2 prepPat bts pats & join in
    prepRhs division rhs
  else
    lift []
prepMatch _ = undefined

prepPat :: BindingTime -> Pat SrcSpanInfo -> Division SrcSpanInfo
-- TODO: use Name instead of QName in division?
prepPat bt (PVar info name) = [(NameLookup name, bt)]
prepPat bt (PApp info qName pats) = pats >>= prepPat bt
prepPat bt (PTuple info boxed pats) = pats >>= prepPat bt
prepPat bt (PList info pats) = pats >>= prepPat bt
-- TODO: handle more cases
prepPat _ _ = undefined

prepRhs :: Division SrcSpanInfo -> Rhs SrcSpanInfo -> PrepMonad
prepRhs division (UnGuardedRhs info exp) = pure (division, exp)
prepRhs division (GuardedRhss info guardedRhss) = lift guardedRhss >>= prepGuardedRhs division

prepGuardedRhs :: Division SrcSpanInfo -> GuardedRhs SrcSpanInfo -> PrepMonad
prepGuardedRhs division (GuardedRhs info stmts exp) = pure (division, exp)









-- Helpers
analyzeSimpleExp :: ([Exp SrcSpanInfo] -> Exp SrcSpanInfo) -> [Exp SrcSpanInfo] -> BTAMonad SrcSpanInfo (BindingTime, Exp SrcSpanInfo)
analyzeSimpleExp constructor exps = do
  analyzeRes <- mapM analyzeExp exps
  let (bts, analyzedExps) = unzip analyzeRes
  if Dynamic `elem` bts then
    pure (Dynamic, bracketTH $ constructor $ fmap (liftIfStatic >>> spliceTH) analyzeRes)
  else
    pure (Static, constructor analyzedExps)


liftIfStatic :: (BindingTime, Exp SrcSpanInfo) -> Exp SrcSpanInfo
liftIfStatic (Static, e) = liftTH e
liftIfStatic (Dynamic, e) = e


-- TODO: implement inverse of this?
-- Gather expression being applied along with argument list from nested applications
simplifyApp :: Exp SrcSpanInfo -> (Exp SrcSpanInfo, [Exp SrcSpanInfo])
simplifyApp (App _ e1 e2) = (<>[e2]) <$> simplifyApp e1
simplifyApp e = (e, [])


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
