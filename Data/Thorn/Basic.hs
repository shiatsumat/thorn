-- |
-- The module Data.Thorn.Basic.
module Data.Thorn.Basic (
    -- * Type Variants
    -- $typevariants
    T0, T1, T2, T3, T4, T5, T6, T7, T8, T9

    -- * Names
  , modifyname
  ) where

import Data.Thorn.Internal

{- $typevariants
    These types @'T0', ..., 'T9'@ are used for representing type variants.

> testtypevariant :: (String,Int,Int)
> testtypevariant = $(autofmap $[t|(,,) T0|]) (+10) (+20) ("hello",1,1) -- ("hello",11,21)
-}

