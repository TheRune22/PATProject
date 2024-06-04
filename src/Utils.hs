{-# LANGUAGE RankNTypes #-}

module Utils where


import qualified Language.Haskell.Exts as H
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Arrow ((>>>))
import Data.Data (Data)


-- QuasiQuoters for parsing Haskell code to AST
-- TODO: extract inner functions if needed elsewhere, use parseWithMode to enable TH in quasiquotes?
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


-- Inspired by dataToExpQ in Language.Haskell.Syntax
--nameTHToH :: Name -> H.QName ()
--nameTHToH (Name (OccName n) NameS) = H.UnQual () (H.Ident () n)
--nameTHToH (Name (OccName n) (NameQ (ModName m))) = H.Qual () (H.ModuleName () m) (H.Ident () n)
---- TODO: handle other cases
--nameTHToH _ = notImplementedError
--
--
--litTHToH :: Lit -> H.Literal ()
--litTHToH (CharL c) = H.Char () c $ show c
--litTHToH (StringL s) = H.String () s $ show s
--litTHToH (IntegerL i) = H.Int () i $ show i
--litTHToH (RationalL r) = H.Frac () r $ show r
--litTHToH (IntPrimL i) = H.PrimInt () i $ show i
--litTHToH (WordPrimL w) = H.PrimWord () w $ show w
--litTHToH (FloatPrimL f) = H.PrimFloat () f $ show f
--litTHToH (DoublePrimL d) = H.PrimDouble () d $ show d
--litTHToH (CharPrimL c) = H.PrimChar () c $ show c
---- TODO: handle other cases
--litTHToH _ = notImplementedError
--
--
--nameToHExp :: Name -> m (H.Exp ())
--nameToHExp name =
--  case nameSpace name of
--                   Just VarName  -> return $ H.Var () $ nameTHToH name
--                   Just DataName -> return $ H.Con () $ nameTHToH name
--                   _ -> error $ "Can't construct an expression from name "
--                             ++ showName name
--
--litToHExp :: Lit -> m (H.Exp ())
--litToHExp lit = return $ H.Lit () $ litTHToH lit

-- TODO: need expTHToH, too much work?
--appToHExp :: m Exp -> m Exp -> m (H.Exp ())
--appToHExp e1 e2 = do
--  e1' <- e1
--  e2' <- e2
--  return $ H.App () e1' e2'

--dataToHExpQ  ::  (Quote m, Data a)
--            =>  (forall b . Data b => b -> Maybe (m (H.Exp ())))
--            ->  a
--            ->  m (H.Exp ())
--dataToHExpQ = dataToQa nameToHExp litToHExp (foldl appToHExp)


--showToHExp :: Show a => a -> m (H.Exp ())
--showToHExp :: Show a => a -> H.ParseResult (H.Exp H.SrcSpanInfo)
--showToHExp = show >>> H.parseExp


-- From an expression generate a corresponding expression which generates the original expression using TH
bracketTH :: H.Exp H.SrcSpanInfo -> H.Exp H.SrcSpanInfo
bracketTH e = H.BracketExp H.noSrcSpan $ H.ExpBracket H.noSrcSpan e
-- TODO: avoid use of brackets to allow nesting using something like this?
--bracketTH e = H.fromParseResult $ H.parseExp $ show $ liftData e

spliceTH :: H.Exp H.SrcSpanInfo -> H.Exp H.SrcSpanInfo
-- TODO: only add parens where necessary?
-- Add extra parenthesis just in case, since can be problematic in e.g. NegApp
spliceTH e = H.Paren H.noSrcSpan $ H.SpliceExp H.noSrcSpan $ H.ParenSplice H.noSrcSpan e
--spliceTH e = H.SpliceExp H.noSrcSpan $ H.ParenSplice H.noSrcSpan e

-- TODO: check if this is actually useful frequently
($|$) :: (H.Exp H.SrcSpanInfo -> t) -> H.Exp H.SrcSpanInfo -> t
f $|$ e = f (spliceTH e)

liftTH :: H.Exp H.SrcSpanInfo -> H.Exp H.SrcSpanInfo
-- TODO: could be qualified? ever need parens here?
liftTH = H.App H.noSrcSpan (H.Var H.noSrcSpan $ H.UnQual H.noSrcSpan $ H.Ident H.noSrcSpan "lift")


-- Error used for not implemented functionality
notImplementedError :: a
-- TODO: use undefined instead?
notImplementedError = error "Not implemented"
