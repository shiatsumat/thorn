{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

-- |
-- The module Data.Thorn.Zipper
module Data.Thorn.Zipper (
    autozipper
  ) where

import Data.Thorn.Internal
import Data.Thorn.Fmap
import Language.Haskell.TH

autozipper :: TypeQ -> DecsQ
autozipper t = fail "oh"

