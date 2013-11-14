{-# LANGUAGE TemplateHaskell, TypeOperators #-}

import Language.Haskell.TH
import Data.Thorn
import Data.Char
import Data.Functor
import Data.Functor.Contravariant
import Data.Bifunctor
import Data.Profunctor

type Nuf x y = y -> x
type a :<- b = Nuf a b
nuf = $(autofmap [t|(:<-)|]) chr ord (+1) 'c'
vnuf = $(autovariance [t|(:<-)|])

data List a = Nil | a :+ (List a) deriving Show
golist 0 = Nil
golist n = n :+ (golist (n-1))
list = $(autofmap $[t|List|]) (+1) (golist 10)
vlist = $(autovariance [t|List|])
autofunctorize [t|List|]

data Rose a = Rose a (Forest a) deriving Show
data Forest a = Forest [Rose a] deriving Show
gorose n = Rose n (Forest (replicate n (gorose (n-1))))
rose = $(autofmap $[t|Rose|]) (+1) (gorose 3)
autofunctorize [t|Rose|]
autofunctorize [t|Forest|]

tuple = $(autofmap $[t|(,,) Int|]) (+1) (+2) (0,0,0)

main = print vlist

