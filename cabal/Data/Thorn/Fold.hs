{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

-- |
-- The module Data.Thorn.Fold
module Data.Thorn.Fold (
    unfixdata
  , autofold, autoMutualFold
  , autounfold, autoMutualUnfold
    ) where

import Data.Thorn.Internal
import Language.Haskell.TH
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Data.Monoid

-- |
-- @unfixdata t@
unfixdata :: TypeQ -> DecsQ
unfixdata t = do fail "oh"

-- |
-- @autofold t@
autofold :: TypeQ -> ExpQ
autofold t = do fail "oh"

-- |
-- @autoMutualFold t@
autoMutualFold :: [TypeQ] -> ExpQ
autoMutualFold ts = do fail "oh"


-- |
-- @autounfold t@
autounfold :: TypeQ -> ExpQ
autounfold t = do fail "oh"

-- |
-- @autoMutualUnfold t@
autoMutualUnfold :: [TypeQ] -> ExpQ
autoMutualUnfold ts = do fail "oh"

