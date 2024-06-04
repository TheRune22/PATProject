module BTA where

import Utils

import Language.Haskell.Exts
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Monad.RWS.Lazy
import Data.Set (Set, member)
import Debug.Trace (trace)
import Control.Arrow ((>>>))
import Data.Function ((&))



-- Do now
-- TODO: implement handling of function calls in analyzeExp
-- TODO: function for specializing single function name given dynamic args, maybe returning name of specialized function?
-- TODO: add input mudule to reader, or just decs, or map of names to bodies as exps?
-- TODO: support partial application? specialize one application at a time? would need to be able to specialize a specialized function, must support Th and avoid nesting
-- TODO: how to implement dynamic recursion?
-- TODO: should also keep track of which vars are actually functions?, binding time of functions?
-- TODO: Reader or state for keeping track of implemented functions, maybe mapping function name and dynamic args to actual name of implementation?
-- TODO: How to determine binding time of function call?

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
-- TODO: copy original function definition? just import or duplicate?
-- TODO: must also know name of function being specialized, and the name of the specialized function for recursion, or can all functions be handled equally?
-- TODO: need to be able to map original function names to specialized names? could be done by a function created at specialization?
-- TODO: must keep track of values that must be specialized to? should be done at specialization, unless simply inserting lambdas instead of function calls
-- TODO: ensure that var names can simply be preserved, and are always resolved correctly
-- TODO: ensure TH wrapping is valid
-- TODO: if always wrapping in TH if and only if dynamic just check this in caller, rather than returning binding time explicitly

-- TODO: do a first pass that simply annotates all expressions with their binding time, then a second pass that generates template haskell?
-- TODO:    should subexpressions be handled differently if within larger expression of same binding time? would probably be nicer, but maybe not needed
-- TODO:    if handled sam way, maybe second pass could merge subexpressions of same binding time into one?
-- TODO:    alternatively wrap in TH in caller
-- TODO:    two passes or not? only needed if subexpressions must know context, can maybe be handled by simpler second pass to merge subexpressions
-- TODO:    try single pass first, then two passes if needed


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
-- wrap in TH in caller or callee?: wrap in TH in caller, requires above solution^
-- how to communicate binding time? return explicitly, use annotation, or just infer from TH bracket use?





newtype QNameLookup l = QNameLookup (QName l)

instance Eq (QNameLookup l) where
--  TODO: how to handle all QName info? different representations could refer to same variable
--  Compare QNames, ignoring SrcSpanInfo
  QNameLookup n1 == QNameLookup n2 = n1 =~= n2

-- TODO: just use bool instead?
-- TODO: map each variable to its binding time, add special cases for functions, are these also seen as vars?
data BindingTime = Static | Dynamic

-- TODO: could just be set of dynamic variables, since static is default?
-- TODO: instantiate l or drop it? probably won't need SrcSpanInfo, but could be used for Division?
-- TODO: use QName, Name, or String?, drop l?
type Division l = [QNameLookup l]
--type Division l = Set (QNameLookup l)

-- TODO: merge Division and functionEnv if we need to look up functions same way as variables?
type FunctionEnv l = ()






-- TODO: rename? remove l
--type BTAMonad l = Reader (Division l)
-- TODO: function env in state instead? flip reader and writer?
type BTAMonad l = ReaderT (Division l, FunctionEnv l) (Writer [Decl l])
--type BTAMonad l = RWS (Division l) [Decl l] (FunctionEnv l)

-- TODO: should probably avoid SrcSpanInfo here, rename to isDynamic and just return bool?
bindingTime :: QName SrcSpanInfo -> BTAMonad SrcSpanInfo BindingTime
bindingTime qName = do
  isDynamic <- reader $ fst >>> elem (QNameLookup qName)
  if isDynamic then
    pure Dynamic
  else
    pure Static
--  lookupRes <- reader $ fst >>> lookup (QNameLookup qName)
--  case lookupRes of
--    Just bt -> pure bt
----    If not found, assume static, since probably a static import
--    Nothing -> pure Static





--analyzeModule :: Module SrcSpanInfo -> BTAMonad SrcSpanInfo (Module SrcSpanInfo)
---- TODO: handle other parts of the module
--analyzeModule (Module info moduleHead pragmas imports [decl]) = Module info moduleHead pragmas imports <$> mapM analyzeDecl [decl]
--analyzeModule _ = notImplementedError
--
---- TODO: don't specialize if all args are static or dynamic, just copy code
--analyzeDecl :: Decl SrcSpanInfo -> BTAMonad SrcSpanInfo (Decl SrcSpanInfo)
--analyzeDecl (FunBind info [match]) = FunBind info <$> mapM analyzeMatch [match]
--analyzeDecl _ = notImplementedError
--
--analyzeMatch :: Match SrcSpanInfo -> BTAMonad SrcSpanInfo (Match SrcSpanInfo)
---- TODO: how to handle pats, only do simple cases?
--analyzeMatch (Match info name pats rhs Nothing) = do
----  TODO: could look up division for name here
--  analyzedRhs <- analyzeRhs rhs
--  pure $ Match info name pats analyzedRhs Nothing
---- TODO: if Let is implemented, also handle binds here in same way?
--analyzeMatch _ = notImplementedError
--
--analyzeRhs :: Rhs SrcSpanInfo -> BTAMonad SrcSpanInfo (Rhs SrcSpanInfo)
--analyzeRhs (UnGuardedRhs info exp) = UnGuardedRhs info <$> analyzeExp exp
--analyzeRhs _ = notImplementedError

-- TODO: this should maybe use or replace above
--analyzeFunc funcName dynamicArgs =
-- where to get existing module from, and how to find right function?
-- add signature to set of specialized functions, but first check if already there?
-- initialize environment with dynamic args
-- emit implementation in writer monad
-- return name of specialized function?
-- TODO: take QName or string for function name?
-- TODO: take QName, String or argument numbers for dynamic args?
-- TODO: don't specialize if all args are static or dynamic, just copy code



-- TODO: maybe reader should not be used before this point, should be input instead?
-- TODO: start here

analyzeExp :: Exp SrcSpanInfo -> BTAMonad SrcSpanInfo (Exp SrcSpanInfo, BindingTime)
--analyzeExp :: Exp l -> Exp l
analyzeExp (Var info qName) = do
  bt <- bindingTime qName
  case bt of
-- TODO: handle case where this is a (recursive) function call?
    Static -> pure (Var info qName, Static)
    Dynamic -> pure (wrapTH $ Var info qName, Dynamic)
analyzeExp (Lit info lit) =
-- Literals are always static
  pure (Lit info lit, Static)
analyzeExp (InfixApp info exp1 qOp exp2) = undefined
-- TODO: need to check for nested applications to get all arguments, but could maybe handle one at a time?
analyzeExp (App info exp1 exp2) = undefined
analyzeExp (NegApp info exp) = undefined
-- TODO:
--analyzeExp (Lambda info pats exp) = _
--analyzeExp (Let info binds exp) = _
analyzeExp (If info exp1 exp2 exp3) = do
  (analyzedExp1, bt1) <- analyzeExp exp1
  (analyzedExp2, bt2) <- analyzeExp exp2
  (analyzedExp3, bt3) <- analyzeExp exp3
--  TODO: what to do if branches have different binding time?
  undefined
--  TODO: add more from https://hackage.haskell.org/package/haskell-src-exts-1.23.1/docs/Language-Haskell-Exts-Syntax.html#t:Exp
-- List, tuple, case, multiif, paren, section
-- Try self application to get missing cases
analyzeExp _ = notImplementedError
