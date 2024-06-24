{-# LANGUAGE RankNTypes #-}

module Utils where


import qualified Language.Haskell.Exts as H
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Control.Arrow ((>>>))
import Debug.Trace (trace)


-- QuasiQuoters for parsing Haskell code to AST, useful for testing and debugging
hModule :: QuasiQuoter
hModule = QuasiQuoter { quoteExp = H.parseModule >>> H.fromParseResult >>> liftData}

hExp :: QuasiQuoter
hExp = QuasiQuoter { quoteExp = H.parseExp >>> H.fromParseResult >>> liftData}

hStmt :: QuasiQuoter
hStmt = QuasiQuoter { quoteExp = H.parseStmt >>> H.fromParseResult >>> liftData}

hPat :: QuasiQuoter
hPat = QuasiQuoter { quoteExp = H.parsePat >>> H.fromParseResult >>> liftData}

hDecl :: QuasiQuoter
hDecl = QuasiQuoter { quoteExp = H.parseDecl >>> H.fromParseResult >>> liftData}

hType :: QuasiQuoter
hType = QuasiQuoter { quoteExp = H.parseType >>> H.fromParseResult >>> liftData}

hImports :: QuasiQuoter
hImports = QuasiQuoter { quoteExp = H.parseImportDecl >>> H.fromParseResult >>> liftData}


-- From an expression generate a corresponding expression which generates the original expression using TH
bracketTH :: H.Exp H.SrcSpanInfo -> H.Exp H.SrcSpanInfo
bracketTH e = H.BracketExp H.noSrcSpan $ H.ExpBracket H.noSrcSpan e

spliceTH :: H.Exp H.SrcSpanInfo -> H.Exp H.SrcSpanInfo
-- Splice cancels out brackets, giving simpler output
spliceTH (H.BracketExp info1 (H.ExpBracket info2 e)) = e
-- Add extra parenthesis just in case, since can be problematic in e.g. NegApp
spliceTH e = H.Paren H.noSrcSpan $ H.SpliceExp H.noSrcSpan $ H.ParenSplice H.noSrcSpan e

liftTH :: H.Exp H.SrcSpanInfo -> H.Exp H.SrcSpanInfo
liftTH = H.App H.noSrcSpan (H.Var H.noSrcSpan $ H.UnQual H.noSrcSpan $ H.Ident H.noSrcSpan "lift")

listH :: [H.Exp H.SrcSpanInfo] -> H.Exp H.SrcSpanInfo
listH = H.List H.noSrcSpan

stringH :: String -> H.Exp H.SrcSpanInfo
stringH s = H.Lit H.noSrcSpan $ H.String H.noSrcSpan s s

nothingH :: H.Exp H.SrcSpanInfo
nothingH = H.Con H.noSrcSpan $ H.UnQual H.noSrcSpan $ H.Ident H.noSrcSpan "Nothing"

justH :: H.Exp H.SrcSpanInfo -> H.Exp H.SrcSpanInfo
justH = H.App H.noSrcSpan (H.Con H.noSrcSpan $ H.UnQual H.noSrcSpan $ H.Ident H.noSrcSpan "Just")

unQualVarH :: String -> H.Exp H.SrcSpanInfo
unQualVarH = H.Var H.noSrcSpan . H.UnQual H.noSrcSpan . H.Ident H.noSrcSpan

boolH :: Bool -> H.Exp H.SrcSpanInfo
boolH True = H.Con H.noSrcSpan $ H.UnQual H.noSrcSpan $ H.Ident H.noSrcSpan "True"
boolH False = H.Con H.noSrcSpan $ H.UnQual H.noSrcSpan $ H.Ident H.noSrcSpan "False"


-- Gather expression being applied along with argument list from nested applications
simplifyApp :: H.Exp H.SrcSpanInfo -> (H.Exp H.SrcSpanInfo, [H.Exp H.SrcSpanInfo])
simplifyApp (H.App _ e1 e2) = (<>[e2]) <$> simplifyApp e1
simplifyApp e = (e, [])

-- Inverse of above
applyToArgs :: H.Exp H.SrcSpanInfo -> [H.Exp H.SrcSpanInfo] -> H.Exp H.SrcSpanInfo
applyToArgs = foldl (H.App H.noSrcSpan)


debug :: (Monad m, Show a) => a -> m ()
debug x = do
  trace (show x) $ pure ()
