{-# LANGUAGE TemplateHaskell, TypeOperators #-}

import Data.Thorn
import Data.Char

type Nuf x y = y -> x
type a :<- b = Nuf a b
nuf = $(autofmap [t|(:<-)|]) chr ord (+1) 'c'

data List a = Nil | Cons a (List a) deriving Show
golist 0 = Nil
golist n = Cons n (golist (n-1))
list = $(autofmap $[t|List|]) (+1) (golist 10)

data Rose a = Rose a (Forest a) deriving Show
data Forest a = Forest [Rose a] deriving Show
gorose n = Rose n (Forest (replicate n (gorose (n-1))))
rose = $(autofmap $[t|Rose|]) (+1) (gorose 3)

main = print rose

