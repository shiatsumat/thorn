{-# LANGUAGE TemplateHaskell #-}

-- |
-- The module Data.Thorn.Zipper.
module Data.Thorn.Zipper (
    Hop(..)
  ) where

import Data.Thorn.Internal
import Data.Thorn.Functor
import Data.Thorn.Fold
import Language.Haskell.TH

data Hop =
    MemberH Name Int
  | LabelH Name
  | ListH Int

