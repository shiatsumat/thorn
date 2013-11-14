{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

-- |
-- The module Data.Thorn.Fmap
module Data.Thorn.Fmap (
    autofmap
  , Variance(..)
  , autovariance
  ) where

import Data.Thorn.Internal
import Language.Haskell.TH
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Data.Monoid

-- |
-- @autofmap t@ generates the @fmap@ of the type @t@.
-- Quite surprisingly, it still works for any arity, co\/contra\/free\/fixed-variances, type synonyms, and mutual recursions.
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
autofmap'' (FuncTx _) = fail "* -> k"
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
          apps e es = foldl (\e es -> AppE e es) e es
          tx0 = SeenDataTx nm vmp
autofmap'' (SeenDataTx nm vmp) = fail "Autofmap doesn't work well, sorry"
autofmap'' (TupleTx txs) = do
    es <- autofmapmap txs
    return $ LamE [TupP (map newVarP [0..length txs-1])] (TupE es)
    where go i tx = autofmap' tx >>= \e -> return $ AppE e (newVarE i)
autofmap'' (ArrowTx txa txb) = do
    fa <- autofmap' txa
    fb <- autofmap' txb
    return $ LamE [newVarP 0, newVarP 1] (AppE fb (AppE (newVarE 0) (AppE fa (newVarE 1))))
autofmap'' (ListTx tx) = autofmap' tx >>= \f -> return $ AppE (mkNameE "map") f
autofmap'' (SpecialTx n) = return $ VarE (newFunc n)

autofmapmap txs = mapM (\(i,tx) -> autofmap' tx >>= \e -> return $ AppE e (newVarE i)) (zip [0 .. length txs - 1] txs)

-- |
-- @Variance@ is a variance of a parameter of a functor.
-- @Co@ means covariance, one of a normal functor, 
-- @Contra@ means contravariance, a dual of covariance, 
-- @Free@ means to be able to satisfy either covariance or contravariance, 
-- @Fixed@ means to have to satisfy both covariance and contravariance.
-- And @v1 `mappend` v2@ means to have to satisfy both @v1@ and @v2@.
data Variance = Co | Contra | Free | Fixed deriving (Show, Read)

instance Monoid Variance where
    Free `mappend` v = v
    v `mappend` Free = v
    Fixed `mappend` _ = Fixed
    _ `mappend` Fixed = Fixed
    Co `mappend` Contra = Fixed
    Contra `mappend` Co = Fixed
    mempty = Free

-- |
-- @autovariance t@ provides you the variances.
autovariance :: TypeQ -> Q [Variance]
autovariance t = do
    (n,tx) <- t >>= normalizeType [] [] >>= apply 0
    fail "oh"

