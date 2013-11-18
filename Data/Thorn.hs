-- |
-- Thorn, Datatype Manipulation with Template Haskell.
-- 
-- It supports
-- * generating functors from various kinds of datatypes
-- * generating folding and unfolding functions from various kinds of recursive datatypes, including mutually recursive datatypes.

module Data.Thorn (
    -- * Functor
    -- $functor
    autofmap, autofmaptype, autofmapdec, autofunctorize
    -- ** Variance
  , Variance(..)
  , autovariance
    
    -- * Folding and Unfolding
    -- $fold
  , unfixdata, unfixdataEx, autofold, autofoldtype, autofolddec, autounfold, autounfoldtype, autounfolddec
    -- ** Mutual Recursion
  , unfixdataMutual, unfixdataMutualEx, autofoldMutual, autofoldtypeMutual, autofolddecMutual, autounfoldMutual, autounfoldtypeMutual, autounfolddecMutual
    -- ** Primitive Functions
  , autoin, autoout, autohylo
  , autoinMutual, autooutMutual, autohyloMutual

    -- * Type Variants
    -- $typevariants
  , T0, T1, T2, T3, T4, T5, T6, T7, T8, T9    

    -- * Example
    -- ** Functor Example
    -- $functorexample
    -- ** Folding and Unfolding Example
    -- $foldexample
  ) where

import Data.Thorn.Type
import Data.Thorn.Functor
import Data.Thorn.Fold

{- $functor
    Thorn generates functors from various kinds of datatypes.

    Quite surprisingly, it still works for any arities, co\/contra\/free\/fixed-variances, partially applied types, type synonyms, and mutual recursions.

    For more information, see <Data-Thorn.html#functorexample Functor Example>.
-}

{- $fold
    Thorn generates folding and unfolding functions from various kinds of recursive datatypes, including mutually recursive datatypes.

    For more information, see <Data-Thorn.html#foldexample Folding and Unfolding Example>.
-}

{- $typevariants
    These types are used for representing type variants. For more information, see <Data-Thorn.html#functorexample Functor Example>.
-}

{- $functorexample

   #functorexample#

   "Data.Thorn.FunctorExample"

-}

{- $foldexample

   #foldexample#

   "Data.Thorn.FoldExample"

-}

