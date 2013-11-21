{-# LANGUAGE TemplateHaskell, TypeOperators, StandaloneDeriving #-}

-- |
-- Example of generating folding and unfolding.
module Data.Thorn.FoldExample (module Data.Thorn.FoldExample) where

{-
> import Data.Thorn
> 
> data x :$ y = Nil | (x,y) :* (x :$ y) deriving Show
> 
> unfixdata [t|(:$)|]
> 
> deriving instance (Show a, Show b, Show c) => Show ((:&$) a b c)
> 
> autofolddec "foldsth" [t|(:&$)|] [t|(:$)|]
> autounfolddec "unfoldsth" [t|(:&$)|] [t|(:$)|]
> 
> f :: Int -> (:&$) Int Int Int
> f 0 = UfNil
> f n = (n,n) :&* (n-1)
> ff :: Int :$ Int
> ff = unfoldsth f (5 :: Int)
> 
> g :: (:&$) Int Int Int -> Int
> g UfNil = 0
> g ((m,n) :&* k) = m+n+k
> gg :: Int
> gg = foldsth g ff
> 
> data Rose x = Rose x (Forest x)
> data Forest x = Forest [Rose x]
> 
> unfixdataMutual [[t|Rose|], [t|Forest|]]
> autofolddecMutual "foldrose" [([t|UfRose|],[t|Rose|]),([t|UfForest|],[t|Forest|])] 0
> autounfolddecMutual "unfoldrose" [([t|UfRose|],[t|Rose|]),([t|UfForest|],[t|Forest|])] 0
> 
> data CrazyA = CrazyA Int [CrazyB]
> data CrazyB = CrazyB Int [CrazyC]
> data CrazyC = CrazyC Int [CrazyA]
> unfixdataMutual [[t|CrazyA|], [t|CrazyB|], [t|CrazyC|]]
> autofolddecMutual "foldcrc" [([t|UfCrazyA|],[t|CrazyA|]),([t|UfCrazyB|],[t|CrazyB|]),([t|UfCrazyC|],[t|CrazyC|])] 2
> autounfolddecMutual "unfoldcrc" [([t|UfCrazyA|],[t|CrazyA|]),([t|UfCrazyB|],[t|CrazyB|]),([t|UfCrazyC|],[t|CrazyC|])] 2
-}

import Data.Thorn

data x :$ y = Nil | (x,y) :* (x :$ y) deriving Show

unfixdata [t|(:$)|]

deriving instance (Show a, Show b, Show c) => Show ((:&$) a b c)

autofolddec "foldsth" [t|(:&$)|] [t|(:$)|]
autounfolddec "unfoldsth" [t|(:&$)|] [t|(:$)|]

f :: Int -> (:&$) Int Int Int
f 0 = UfNil
f n = (n,n) :&* (n-1)
ff :: Int :$ Int
ff = unfoldsth f (5 :: Int)

g :: (:&$) Int Int Int -> Int
g UfNil = 0
g ((m,n) :&* k) = m+n+k
gg :: Int
gg = foldsth g ff

data Rose x = Rose x (Forest x)
data Forest x = Forest [Rose x]

unfixdataMutual [[t|Rose|], [t|Forest|]]
autofolddecMutual "foldrose" [([t|UfRose|],[t|Rose|]),([t|UfForest|],[t|Forest|])] 0
autounfolddecMutual "unfoldrose" [([t|UfRose|],[t|Rose|]),([t|UfForest|],[t|Forest|])] 0

data CrazyA = CrazyA Int [CrazyB]
data CrazyB = CrazyB Int [CrazyC]
data CrazyC = CrazyC Int [CrazyA]
unfixdataMutual [[t|CrazyA|], [t|CrazyB|], [t|CrazyC|]]
autofolddecMutual "foldcrc" [([t|UfCrazyA|],[t|CrazyA|]),([t|UfCrazyB|],[t|CrazyB|]),([t|UfCrazyC|],[t|CrazyC|])] 2
autounfolddecMutual "unfoldcrc" [([t|UfCrazyA|],[t|CrazyA|]),([t|UfCrazyB|],[t|CrazyB|]),([t|UfCrazyC|],[t|CrazyC|])] 2

