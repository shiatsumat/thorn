{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators, MultiParamTypeClasses, ConstraintKinds, FlexibleInstances #-}

module Data.GeneralFunctor.Functor(
{-
    Variance(..)
  , Functor(..)
  , Bifunctor, Trifunctor, Profunctor
  ,-} module Data.GeneralFunctor.Functor) where

import Prelude hiding (Functor(..))
import GHC.Exts

data Variance = Co | Contra | Free | Fixed

type family   Apply (f :: k) (as :: [*]) :: *
type instance Apply (f :: * -> l) (a ': as) = Apply (f a) as
type instance Apply (f :: *) as = f

type family   Fmap' (vs :: [Variance]) (f :: k) (as :: [*]) (bs :: [*]) (cs :: [*]) (x :: *)
type instance Fmap' (Co ': vs) (f :: * -> k) (a ': as) (b ': bs) (c ': cs) x = (a -> b) -> Fmap' vs (f Int) as bs cs x
type instance Fmap' (Contra ': vs) (f :: * -> k) (a ': as) (b ': bs) (c ': cs) x = (b -> a) -> Fmap' vs (f Int) as bs cs x
type instance Fmap' (Free ': vs) (f :: * -> k) (a ': as) (b ': bs) (c ': cs) x = c -> Fmap' vs (f Int) as bs cs x
type instance Fmap' (Fixed ': vs) (f :: * -> k) (a ': as) (b ': bs) (c ': cs) x = (a -> a) -> Fmap' vs (f Int) as bs cs x
type instance Fmap' '[] f as bs cs x = x

type family   FmapConst (vs :: [Variance]) (as :: [*]) (bs :: [*]) :: Constraint
type instance FmapConst (Co ': vs) (a ': as) (b ': bs) = FmapConst vs as bs
type instance FmapConst (Contra ': vs) (a ': as) (b ': bs) = FmapConst vs as bs
type instance FmapConst (Free ': vs) (a ': as) (b ': bs) = FmapConst vs as bs
type instance FmapConst (Fixed ': vs) (a ': as) (b ': bs) = (a~b, FmapConst vs as bs)
type instance FmapConst '[] as bs = ()

type Fmap vs f as bs cs = Fmap' vs f as bs cs (Apply f as -> Apply f bs)

type family   Same (vs :: [Variance]) (f :: k) :: Bool
type instance Same (v ': vs) (f :: * -> k) = Same vs (f Int)
type instance Same (v ': vs) (f :: *) = False
type instance Same '[] (f :: * -> k) = False
type instance Same '[] (f :: *) = True

class (Same vs f ~ True) => Functor (vs :: [Variance]) (f :: k) where
    fmap :: FmapConst vs [a1,a2,a3,a4,a5,a6,a7] [b1,b2,b3,b4,b5,b6,b7] =>
                Fmap vs f [a1,a2,a3,a4,a5,a6,a7] [b1,b2,b3,b4,b5,b6,b7] [c1,c2,c3,c4,c5,c6,c7]

type Bifunctor f = Functor [Co,Co] f
type Trifunctor f = Functor [Co,Co,Contra] f
type Profunctor f = Functor [Contra,Co] f

instance Functor [Co,Co] (,) where
    fmap f1 f2 (a1,a2) = (f1 a1,f2 a2)

instance Functor [Co,Co,Co] (,,) where
    fmap f1 f2 f3 (a1,a2,a3) = (f1 a1,f2 a2,f3 a3)

instance Functor [Co,Co,Co,Co] (,,,) where
    fmap f1 f2 f3 f4 (a1,a2,a3,a4) = (f1 a1,f2 a2,f3 a3,f4 a4)

instance Functor [Co,Co,Co,Co,Co] (,,,,) where
    fmap f1 f2 f3 f4 f5 (a1,a2,a3,a4,a5) = (f1 a1,f2 a2,f3 a3,f4 a4,f5 a5)

instance Functor [Co,Co,Co,Co,Co,Co] (,,,,,) where
    fmap f1 f2 f3 f4 f5 f6 (a1,a2,a3,a4,a5,a6) = (f1 a1,f2 a2,f3 a3,f4 a4,f5 a5,f6 a6)

instance Functor [Co,Co,Co,Co,Co,Co,Co] (,,,,,,) where
    fmap f1 f2 f3 f4 f5 f6 f7 (a1,a2,a3,a4,a5,a6,a7) = (f1 a1,f2 a2,f3 a3,f4 a4,f5 a5,f6 a6,f7 a7)

instance Functor [Co,Co] Either where
    fmap f _ (Left a) = Left (f a)
    fmap _ g (Right b) = Right (g b)

instance Functor '[Co] (Either a) where
    fmap _ (Left a) = Left a
    fmap f (Right b) = Right (f b)

instance Functor [Contra,Co] (->) where
    fmap f g h = g . h . f

instance Functor '[Co] [] where
    fmap = m
        where m f (a:as) = f a : m f as
              m _ [] = []

