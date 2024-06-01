module BTA where


import Language.Haskell.Exts
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Monad.RWS.Lazy
import Data.Set (Set, member)
import Debug.Trace (trace)

import Utils

-- Done?
-- TODO: add static/dynamic division as input/(reader) monad7

-- Do now
-- TODO: implement handling of function calls in analyzeExp
-- TODO: function for specializing single function name given dynamic args, maybe returning name of specialized function?
-- TODO: also keep original module in reader?
-- Reader or state for keeping track of implemented functions, maybe mapping function name and dynamic args to actual name of implementation?
-- Writer for emitting specialized functions
-- How to determine binding time of function call?

-- Do later
-- TODO: should also keep track of which vars are actually functions?
-- TODO: just use bool for binding time, i.e. isDynamic?
-- TODO: decide on and clean up monad stack
-- TODO: make clean wrapper without monads, maybe merge with analyzeFunc or similar?
-- TODO: drop or modify SrcSpanInfo?
-- TODO: implement missing pattern matches
-- TODO: take map of function name to dynamic args, enabling handling multiple decls at once? could just map over functions to handle instead?
-- TODO: use ExpBracket or TExpBracket?
-- TODO: copy original function definition? just import or duplicate?
-- TODO: must also know name of function being specialized, and the name of the specialized function for recursion, or can all functions be handled equally?

-- Notes
-- create (un)specialized versions of functions, as needed
-- Generated code emitted in writer monad
-- create new specializations when new calls are found
--  only specialize local function definitions initially



-- TODO: just use bool instead?
-- TODO: map each variable to its binding time, add special cases for functions, are these also seen as vars?
data BindingTime = Static | Dynamic

-- TODO: could just be set of dynamic variables, since static is default?
-- TODO: instantiate l or drop it? probably won't need SrcSpanInfo, but could be used for Division?
-- TODO: use QName, Name, or String?, drop l?
type Division l = [(QNameLookup l, BindingTime)]
--type Division l = Set (QNameLookup l)

type FunctionEnv l = ()

newtype QNameLookup l = QNameLookup (QName l)

instance Eq (QNameLookup l) where
--  TODO: how to handle all QName info? different representations could refer to same variable
--  Compare QNames, ignoring SrcSpanInfo
  QNameLookup n1 == QNameLookup n2 = n1 =~= n2


-- TODO: rename? remove l
type BTAMonad l = Reader (Division l)
-- TODO: function env in state instead?
--type BTAMonad l = ReaderT (Division l, FunctionEnv l) (Writer [Decl l])
--type BTAMonad l = RWS (Division l) [Decl l] (FunctionEnv l)

-- TODO: should probably avoid SrcSpanInfo here, rename to isDynamic and just return bool?
bindingTime :: QName SrcSpanInfo -> BTAMonad SrcSpanInfo BindingTime
bindingTime qName = do
  division <- ask
--  if member (QNameLookup qName) division then
--    pure Dynamic
--  else
--    pure Static
  case lookup (QNameLookup qName) division of
    Just bt -> pure bt
--    If not found, assume static, since probably a static import
    Nothing -> pure Static





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
-- TODO: must know division

-- TODO: should check if all subexpressions are static
-- TODO: do a first pass that simply annotates all expressions with their binding time, then a second pass that generates template haskell?
-- TODO: should subexpressions be handled differently if within larger expression of same binding time? would probably be nicer, but maybe not needed
-- TODO: if handled sam way, maybe second pass could merge subexpressions of same binding time into one?
-- TODO: just add binding time to output, and check binding time of all subexpressions?
-- TODO: use info to store binding time, also preserve original info?

-- TODO: two passes or not? only needed if subexpressions must know context, can maybe be handled by simpler second pass to merge subexpressions
-- TODO: try single pass first, then two passes if needed

-- TODO: function that takes Exp and returns Exp constructing Exp using TH, or just do as part of analyzeExp?
-- TODO: could just use Brackets and Splice?

analyzeExp :: Exp SrcSpanInfo -> BTAMonad SrcSpanInfo (Exp SrcSpanInfo)
--analyzeExp :: Exp l -> Exp l
analyzeExp (Var info qName) = do
  bt <- bindingTime qName
  case bt of
-- TODO: handle case where this is a (recursive) function call?
    Static -> pure $ Var info qName
--    TODO: should name be handled in special way, or can we assume it to be unique and known?
--    TODO: possible to avoid wrapping here and just let variable be bound to a wrapping?
--          outer expression probably needs to know if variable is static or dynamic, so maybe not useful?
    Dynamic -> pure $ Var info qName
--    Dynamic -> pure $ wrapTH $ Var info qName
analyzeExp (Lit info lit) =
-- Literals are always static
  pure $ Lit info lit
analyzeExp (InfixApp info exp1 qOp exp2) = undefined
analyzeExp (App info exp1 exp2) = undefined
analyzeExp (NegApp info exp) = undefined
-- TODO:
--analyzeExp (Lambda info pats exp) = _
--analyzeExp (Let info binds exp) = _
analyzeExp (If info exp1 exp2 exp3) = undefined
--  TODO: add more from https://hackage.haskell.org/package/haskell-src-exts-1.23.1/docs/Language-Haskell-Exts-Syntax.html#t:Exp
-- List, tuple, case, multiif, paren, section
-- Try self application to get missing cases
analyzeExp _ = notImplementedError
