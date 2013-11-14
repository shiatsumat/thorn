{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

-- |
-- Thorn, a template haskell library.
module Data.Thorn (
    autofmap
  , Variance(..)
  , autovariance, autofunctorize

  , unfixdata
  , autofold, autoMutualFold
  , autounfold, autoMutualUnfold

  , autozipper
  ) where

import Data.Thorn.Fmap
import Data.Thorn.Fold
import Data.Thorn.Zipper

