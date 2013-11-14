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
-- @unfixdata t@ provides a declaration of a data whose fixpoint is the recursive type @t@.
unfixdata :: TypeQ -> DecsQ
unfixdata t = do fail "oh"

-- |
-- @autofold t@ provides a folding function for the recursive type @t@.
autofold :: TypeQ -> ExpQ
autofold t = do fail "oh"

-- |
-- @autoMutualFold ts@ provides a folding function for the mutually recursive types @ts@.
autoMutualFold :: [TypeQ] -> ExpQ
autoMutualFold ts = do fail "oh"

-- |
-- @autounfold t@ provides an unfolding function for the recursive type @t@.
autounfold :: TypeQ -> ExpQ
autounfold t = do fail "oh"

-- |
-- @autoMutualUnfold ts@ provides an unfolding function for the mutually recursive types @ts@.
autoMutualUnfold :: [TypeQ] -> ExpQ
autoMutualUnfold ts = do fail "oh"

