{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Data.Thorn.FoldExample (module Data.Thorn.FoldExample) where

import Data.Thorn

data x :$ y = Nil | (x,y) :* (x :$ y)

unfixdata [t|(:$)|]

insth = $(autoin [t|(:&$)|] [t|(:$)|])
outsth = $(autoout [t|(:&$)|] [t|(:$)|])
hylosth = $(autohylo [t|(:&$)|])
foldsth = $(autofold [t|(:&$)|] [t|(:$)|])
unfoldsth = $(autounfold [t|(:&$)|] [t|(:$)|])

