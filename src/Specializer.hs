module Specializer where

import Language.Haskell.TH
import Control.Monad.State.Lazy
import Control.Monad.RWS.Lazy


-- TODO: use something like this in specializerFunc and state
type Signature = (Name, [Exp])


-- TODO: remove unused parts
-- TODO: add Q to Dec? writer
type SpecializerMonad = RWST () [Dec] () Q




-- TODO: rename
--specializerFunc :: Q Exp -> [Q Pat] -> [Q Exp] -> SpecializerMonad Exp
