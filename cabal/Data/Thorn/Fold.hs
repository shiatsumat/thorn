{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

-- |
-- The module Data.Thorn.Fold
module Data.Thorn.Fold (
    unfixpoint
  , autofold
  , autounfold
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
-- @unfixpoint t@
unfixpoint :: TypeQ -> DecsQ
unfixpoint t = do fail "oh"

-- |
-- @autofold t@
autofold :: TypeQ -> ExpQ
autofold t = do fail "oh"

-- |
-- @autounfold t@
autounfold :: TypeQ -> ExpQ
autounfold t = do fail "oh"

