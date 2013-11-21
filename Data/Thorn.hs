-- |
-- Thorn, Datatype Manipulation with Template Haskell.
-- 
-- It generates
-- 
-- * functors from various kinds of datatypes, regardless of its arity or variances.
-- 
-- * folds and unfolds from various kinds of recursive datatypes, including mutually recursive ones.

module Data.Thorn (
    module Data.Thorn.Functor
  , module Data.Thorn.Fold
  , module Data.Thorn.Type
  ) where

import Data.Thorn.Functor
import Data.Thorn.Fold
import Data.Thorn.Type

