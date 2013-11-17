{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Main (module Main) where

import Data.Thorn

data x :$ y = Nil | (x,y) :* (x :$ y)

unfixdata [t|(:$)|]

insth = $(autoin [t|(:&$)|] [t|(:$)|])
outsth = $(autoout [t|(:&$)|] [t|(:$)|])
hylosth = $(autohylo [t|(:&$)|])
foldsth = $(autofold [t|(:&$)|] [t|(:$)|])
unfoldsth = $(autounfold [t|(:&$)|] [t|(:$)|])

data Rose x = Rose x [Rose x]
--data Forest x = Forest [Rose x]

unfixdata [t|Rose|]

main :: IO ()
main = print (0 :: Int)

