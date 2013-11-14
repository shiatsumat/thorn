{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

-- |
-- Thorn, a template haskell library.
module Data.Thorn (
    -- * Data.Thorn.Fmap
    autofmap
  , Variance(..)
  , autovariance, autofunctorize
  ) where

import Data.Thorn.Fmap
import Data.Thorn.Fold
import Data.Thorn.Zipper

