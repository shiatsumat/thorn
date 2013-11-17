{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FunctionalDependencies #-}

-- |
-- The module Data.Thorn.Recursive.
module Data.Thorn.Recursive (
    Recursive(..)
  , hylo, cata, ana
  , L(..)
  , RecursiveMutual(..)
  , hyloMutual, cataMutual, anaMutual
  ) where

import Data.Bifunctor

class Functor u => Recursive u t where
    into :: u t -> t
    out :: t -> u t

hylo :: Functor u => (a -> u a) -> (u b -> b) -> a -> b
hylo f g = res where res = g. fmap res . f

cata :: Recursive u t => (u a -> a) -> t -> a
cata f x = hylo out f x

ana :: Recursive u t => (a -> u a) -> a -> t
ana f x = hylo f into x

data L a self = L (Maybe (a,self))

instance Functor (L a) where
    fmap f (L (Just (a,b))) = L (Just (a,f b))
    fmap _ (L Nothing) = L Nothing

instance Recursive (L a) [a] where
    into (L (Just (a,as))) = a:as
    into (L Nothing) = []
    out (a:as) = L (Just (a,as))
    out [] = L Nothing

class (Bifunctor u1, Bifunctor u2) => RecursiveMutual u1 u2 t1 t2 | t1->u2, t2->u1 where
    in1 :: u1 t1 t2 -> t1
    in2 :: u2 t1 t2 -> t2
    out1 :: t1 -> u1 t1 t2
    out2 :: t2 -> u2 t1 t2

hyloMutual :: (Bifunctor u1, Bifunctor u2) => (a -> u1 a b) -> (b -> u2 a b) -> (u1 c d -> c) -> (u2 c d -> d) -> (a -> c, b -> d)
hyloMutual f1 f2 g1 g2 = (h1, h2)
    where h1 = g1 . bimap h1 h2 . f1
          h2 = g2 . bimap h1 h2 . f2

cataMutual :: RecursiveMutual u1 u2 t1 t2 => (u1 a b -> a) -> (u2 a b -> b) -> (t1 -> a, t2 -> b)
cataMutual f1 f2 = hyloMutual out1 out2 f1 f2

anaMutual :: RecursiveMutual u1 u2 t1 t2 => (a -> u1 a b) -> (b -> u2 a b) -> (a -> t1, b -> t2)
anaMutual f1 f2 = hyloMutual f1 f2 in1 in2

data Rose x = Rose x (Forest x)
data Forest x = Forest [Rose x]


