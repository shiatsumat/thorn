-- | Thorn, a template haskell library.

module Data.Thorn (
    -- * Functor
    -- $functor
    autofmap
  , Variance(..)
  , autovariance, autofunctorize
    
    -- * Folding and Unfolding
    -- $fold

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

{- $functor
    For more information, see <Data-Thorn.html#FunctorExample Functor Example>.
-}

{- $fold
    For more information, see <Data-Thorn.html#FoldExample Folding and Unfolding Example>.
-}

{- $typevariants
    These types are used for representing type variants. For more information, see <Data-Thorn.html#FunctorExample Functor Example>.
-}

{- $functorexample
   #FunctorExample#

> type a :<- b = b -> a
> nuf :: Char
> nuf = $(autofmap [t|(:<-)|]) chr ord (+1) 'a' -- 'b'
> varnuf :: [Variance]
> varnuf = $(autovariance [t|(:<-)|]) -- [Co,Contra]
> 
> data Cntr a = Cntr (a -> Int)
> autofunctorize [t|Cntr|] -- instance Contravariant Cntr where ...
> 
> tuple :: (Int,Int,Int)
> tuple = $(autofmap $[t|(,,) Int|]) (+1) (+2) (0,0,0) -- (0,1,2)
> vartuple :: [Variance]
> vartuple = $(autovariance [t|(,,) Int|]) -- [Co,Co]
> 
> data FunFun a b = FunFun ((b -> a) -> b)
> varfunfun :: [Variance]
> varfunfun = $(autovariance [t|FunFun|]) -- [Contra,Co]
> autofunctorize [t|FunFun|] -- instance Profunctor FunFun where ...
> 
> data What a b c = What1 c (a -> c) | What2 a
> varwhat :: [Variance]
> varwhat = $(autovariance [t|What|]) -- [Fixed,Free,Co]
> autofunctorize [t|What T0|]
> -- instance Bifunctor (What a) where ... and
> -- instance Profunctor (What a) where ...
> 
> data List a = Nil | a :+ (List a) deriving Show
> fromNormalList :: [a] -> List a
> fromNormalList [] = Nil
> fromNormalList (a : as) = a :+ fromNormalList as
> toNormalList :: List a -> [a]
> toNormalList Nil = []
> toNormalList (a :+ as) = a : toNormalList as
> list :: [Int]
> list = toNormalList $ $(autofmap [t|List|]) (+10) (fromNormalList [1..5]) -- [11..15]
> varlist :: [Variance]
> varlist = $(autovariance [t|List|]) -- [Co]
> autofunctorize [t|List|] -- instance Functor List where ...
> 
> data Rose a = Rose a (Forest a) deriving Show
> data Forest a = Forest [Rose a] deriving Show
> gorose n = Rose n (Forest (replicate n (gorose (n-1))))
> rose = $(autofmap [t|Rose|]) (+1) (gorose 2)
> varrose, varforest :: [Variance]
> varrose = $(autovariance [t|Rose|]) -- [Co]
> varforest = $(autovariance [t|Forest|]) -- [Co]
> autofunctorize [t|Rose|] -- instance Functor Rose where ...
> autofunctorize [t|Forest|] -- instance Functor Forest where ...

-}

{- $foldexample
   #FunctorExample#
    yeah
-}

