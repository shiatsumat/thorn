{-# LANGUAGE TemplateHaskell, TypeOperators, StandaloneDeriving #-}

module Main (module Main) where

import Data.Thorn

data x :$ y = Nil | (x,y) :* (x :$ y) deriving Show

unfixdata [t|(:$)|]

deriving instance (Show a, Show b, Show c) => Show ((:&$) a b c)

$(autofolddec "foldsth" [t|(:&$)|] [t|(:$)|])
$(autounfolddec "unfoldsth" [t|(:&$)|] [t|(:$)|])

f 0 = UfNil
f n = (n,n) :&* (n-1)
ff = unfoldsth f (5 :: Int)

g UfNil = 0
g ((m,n) :&* k) = m+n+k
gg = foldsth g ff

data Rose x = Rose x (Forest x)
data Forest x = Forest [Rose x]

unfixdataMutual [[t|Rose|], [t|Forest|]]
$(autofolddecMutual ["foldrose","foldforest"] [([t|(UfRose)|],[t|(Rose)|]),([t|(UfForest)|],[t|(Forest)|])])
$(autounfolddecMutual ["unfoldrose","unfoldforest"] [([t|(UfRose)|],[t|(Rose)|]),([t|(UfForest)|],[t|(Forest)|])])

main = print gg

