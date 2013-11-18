{-# LANGUAGE TemplateHaskell #-}

-- |
-- The module Data.Thorn.Zipper.
module Data.Thorn.Zipper (
    autozipper
  ) where

import Data.Thorn.Type
import Data.Thorn.Functor
import Data.Thorn.Fold
import Language.Haskell.TH

autozipper :: TypeQ -> DecsQ
autozipper t = fail "oh"

