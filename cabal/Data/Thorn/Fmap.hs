{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

-- |
-- The module Data.Thorn.Fmap
module Data.Thorn.Fmap (
    autofmap
  , Variance(..)
  , autovariance, autovarianceRaw, autofunctorize
  ) where

import Data.Thorn.Internal
import Language.Haskell.TH
import Data.List
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Data.Functor
import Data.Functor.Contravariant
import Data.Bifunctor
import Data.Profunctor

-- |
-- @autofmap t@ generates the @fmap@ of the type @t@.
-- 
-- Quite surprisingly, it still works for any arities, co\/contra\/free\/fixed-variances, partially applied types, type synonyms, and mutual recursions.
--
-- @
--type Nuf x y = y -> x
--type a :<- b = Nuf a b
--nuf = $(autofmap [t|(:<-)|]) chr ord (+1) 'c'
--
--data List a = Nil | Cons a (List a) deriving Show
--golist 0 = Nil
--golist n = Cons n (golist (n-1))
--list = $(autofmap $[t|List|]) (+1) (golist 10)
--
--data Rose a = Rose a (Forest a) deriving Show
--data Forest a = Forest [Rose a] deriving Show
--gorose n = Rose n (Forest (replicate n (gorose (n-1))))
--rose = $(autofmap $[t|Rose|]) (+1) (gorose 3)
-- @
autofmap :: TypeQ -> ExpQ
autofmap t = do
    (n,tx) <- t >>= normalizeType [] [] >>= apply 0
    (e,txnmes) <- runStateT (autofmap' tx) []
    return $ LamE (map newFuncP [0..n-1]) (LetE (fmap (\(tx,nm,Just e) -> ValD (VarP nm) (NormalB e) []) txnmes) e)

apply :: Int -> Typex -> Q (Int,Typex)
apply n (FuncTx f) = f (SpecialTx n) >>= apply (n+1)
apply n tx@(VarTx _) = return (n,tx)
apply n tx@(DataTx _ _ _) = return (n,tx)
apply n tx@(SeenDataTx _ _) = return (n,tx)
apply n tx@(TupleTx _) = return (n,tx)
apply n tx@(ArrowTx _ _) = return (n,tx)
apply n tx@(ListTx _) = return (n,tx)

autofmap',autofmap'' :: Typex -> StateT [(Typex,Name,Maybe Exp)] Q Exp
autofmap' tx = do
    txnmes <- get
    case find (\(tx',_,_)->tx==tx') txnmes of
         Just (_,nm,_) -> return (VarE nm)
         Nothing -> autofmap'' tx
autofmap'' (VarTx _) = return $ mkNameE "id"
autofmap'' (BasicTx _) = return $ mkNameE "id"
autofmap'' (FuncTx _) = fail "Automap doesn't accept such a type with a kind * -> k."
autofmap'' (DataTx nm vmp cxs) = do
    txnmes <- get
    put ((tx0, newFmap (length txnmes), Nothing) : txnmes)
    e <- LamE [newVarP 0] <$> (CaseE (newVarE 0) <$> (mapM go cxs))
    txnmes' <- get
    put $ map (\(tx,nm,e') -> if tx==tx0 then (tx,nm,Just e) else (tx,nm,e')) txnmes'
    return e
    where go (NormalCx nm txs) = do
              es <- autofmapmap txs
              return $ Match (ConP nm (map newVarP [0..length txs-1])) (NormalB (apps (ConE nm) es)) []
          go (InfixCx nm txa txb) = do
              [ea,eb] <- autofmapmap [txa,txb]
              return $ Match (InfixP (newVarP 0) nm (newVarP 1)) (NormalB (InfixE (Just ea) (ConE nm) (Just eb))) []
          tx0 = SeenDataTx nm vmp
autofmap'' (SeenDataTx nm vmp) = fail "Autofmap doesn't work well, sorry."
autofmap'' (TupleTx txs) = do
    es <- autofmapmap txs
    return $ LamE [TupP (map newVarP [0..length txs-1])] (TupE es)
    where go i tx = autofmap' tx >>= \e -> return $ AppE e (newVarE i)
autofmap'' (ArrowTx txa txb) = do
    fa <- autofmap' txa
    fb <- autofmap' txb
    return $ LamE [newVarP 0, newVarP 1] (AppE fb (AppE (newVarE 0) (AppE fa (newVarE 1))))
autofmap'' (ListTx tx) = autofmap' tx >>= \f -> return $ AppE (mkNameE "map") f
autofmap'' (SpecialTx n) = return $ newFuncE n

autofmapmap txs = mapM (\(i,tx) -> autofmap' tx >>= \e -> return $ AppE e (newVarE i)) (zip [0 .. length txs - 1] txs)

-- |
-- @Variance@ is a variance of a parameter of a functor.
data Variance =
    -- | Covariance, one of a normal functor.
    Co
    -- | Contravariance, a dual of covariance.
  | Contra
    -- | Free-variance, or novariance, being supposed to satisfy either covariance or contravariance.
  | Free
    -- | Fixed-variance, or invariance, being suppoesed to satisfy both covariance and contravariance.
  | Fixed deriving (Show, Read)

-- | @v1 `mappend` v2@ means to be supposed to satisfy both @v1@ and @v2@.
instance Monoid Variance where
    Free `mappend` v = v
    v `mappend` Free = v
    Fixed `mappend` _ = Fixed
    _ `mappend` Fixed = Fixed
    Co `mappend` Contra = Fixed
    Contra `mappend` Co = Fixed
    mempty = Free

neg :: Variance -> Variance
neg Co = Contra
neg Contra = Co
neg Free = Free
neg Fixed = Fixed

-- |
-- @autovariance t@ provides the variances of the type @t@.
autovariance :: TypeQ -> ExpQ
autovariance t = do
    vs <- autovarianceRaw t
    return $ ListE (map go vs)
    where go Co = mkNameCE "Co"
          go Contra = mkNameCE "Contra"
          go Free = mkNameCE "Free"
          go Fixed = mkNameCE "Fixed"

autovarianceRaw :: TypeQ -> Q [Variance]
autovarianceRaw t = do
    (n,tx) <- t >>= normalizeType [] [] >>= apply 0
    (_,seq) <- runStateT (autovariance' Co [] tx) (S.replicate n Free)
    return $ (F.toList seq)

autovariance' :: Variance -> [(Name,[Conx])] -> Typex -> StateT (S.Seq Variance) Q ()
autovariance' v dts (SpecialTx n) = do
    seq <- get
    put $ S.adjust (<>v) n seq
autovariance' v dts (VarTx _) = return ()
autovariance' v dts (FuncTx _) = fail "Automap doesn't accept such a type with a kind * -> k."
autovariance' v dts (DataTx nm _ cxs) = mapM_ (mapM_ (autovariance' v ((nm,cxs):dts)) . cxtxs) cxs
autovariance' v dts (SeenDataTx nm _) = return ()
autovariance' v dts (TupleTx txs) = mapM_ (autovariance' v dts) txs
autovariance' v dts (ArrowTx txa txb) = autovariance' (neg v) dts txa >> autovariance' v dts txb
autovariance' v dts (ListTx tx) = autovariance' v dts tx

-- |
-- @autofunctorize t@ provides an instance delcaration of the type @t@ for the suitable functor class : Funtor, Contravariant, Bifunctor, or Profunctor
autofunctorize :: TypeQ -> DecsQ
autofunctorize t = do
    vs <- autovarianceRaw t
    case vs of
         [Co] -> go (mkName "Functor") (mkName "fmap")
         [Contra] -> go (mkName "Contravariant") (mkName "contramap")
         [Co,Co] -> go (mkName "Bifunctor") (mkName "bimap")
         [Contra,Co] -> go (mkName "Profunctor") (mkName "dimap")
         _ -> fail "autofunctorize doesn't know the suitable functor class for this variance"
    where go cls member = do
              e <- autofmap t
              t' <- t
              return [InstanceD [] (AppT (ConT cls) t') [ValD (VarP member) (NormalB e) []]]

