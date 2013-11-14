{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

-- |
-- Thorn, a template haskell library.
-- Re-exports of modules.
module Data.Thorn (
    module Data.Thorn.Fmap
  , module Data.Thorn.Fold
  ) where

import Data.Thorn.Fmap
import Data.Thorn.Fold

