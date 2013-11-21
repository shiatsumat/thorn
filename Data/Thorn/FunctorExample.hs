{-# LANGUAGE TemplateHaskell, TypeOperators #-}

-- |
-- Example of generating functors.
module Data.Thorn.FunctorExample (module Data.Thorn.FunctorExample) where

import Data.Thorn
import Data.Functor.Contravariant
import Data.Bifunctor
import Data.Profunctor

type a :<- b = b -> a
varnuf :: [Variance]
varnuf = $(autovariance [t|(:<-)|]) -- [Co,Contra]
$(autofmapdec "fmapnuf" [t|(:<-)|])

data Cntr a = Cntr (a -> Int)
autofunctorize [t|Cntr|] -- instance Contravariant Cntr where ...

vartuple :: [Variance]
vartuple = $(autovariance [t|(,,) Int|]) -- [Co,Co]
$(autofmapdec "fmaptuple" $[t|(,,) Int|])

data FunFun a b = FunFun ((b -> a) -> b)
varfunfun :: [Variance]
varfunfun = $(autovariance [t|FunFun|]) -- [Contra,Co]
autofunctorize [t|FunFun|] -- instance Profunctor FunFun where ...

data What a b c = What1 c (a -> c) | What2 a
varwhat :: [Variance]
varwhat = $(autovariance [t|What|]) -- [Fixed,Free,Co]
autofunctorize [t|What T0|]
-- instance Bifunctor (What a) where ... and
-- instance Profunctor (What a) where ...

data List a = Nil | a :* (List a) deriving Show
autofunctorize [t|List|] -- instance Functor List where ...
fromNormalList :: [a] -> List a
fromNormalList [] = Nil
fromNormalList (a : as) = a :* fromNormalList as
toNormalList :: List a -> [a]
toNormalList Nil = []
toNormalList (a :* as) = a : toNormalList as
list :: [Int]
list = toNormalList $ fmap (+10) (fromNormalList [1..5]) -- [11..15]

data Rose a = Rose a (Forest a) deriving Show
data Forest a = Forest [Rose a] deriving Show
autofunctorize [t|Rose|] -- instance Functor Rose where ...
autofunctorize [t|Forest|] -- instance Functor Forest where ...
gorose :: Int -> Rose Int
gorose n = Rose n (Forest (replicate n (gorose (n-1))))
getrose :: Rose Int
getrose = fmap (+1) (gorose 2)
