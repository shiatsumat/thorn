{-# LANGUAGE TemplateHaskell #-}

-- |
-- The module Data.Thorn.Functor.
module Data.Thorn.Functor (
    autofmap
  , Variance(..)
  , autovariance, autofunctorize
  ) where

import Data.Thorn.Type
import Language.Haskell.TH
import Data.List
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad.State
import Data.Monoid

-- |
-- @autofmap t@ generates the @fmap@ of the type @t@.
autofmap :: TypeQ -> ExpQ
autofmap t = do
    (n,tx) <- t >>= type2typex [] [] >>= applySpecial 0
    u <- unique
    (e,txnmes) <- runStateT (autofmap' u tx) []
    return $ LamE (map newFuncP [u..u+n-1]) (LetE (fmap (\(_,nm,Just e') -> ValD (VarP nm) (NormalB e') []) txnmes) e)

autofmap',autofmap'' :: Unique -> Typex -> StateT [(Typex,Name,Maybe Exp)] Q Exp
autofmap' u tx = do
    txnmes <- get
    case find (\(tx',_,_)->tx==tx') txnmes of
         Just (_,nm,_) -> return (VarE nm)
         Nothing -> autofmap'' u tx
autofmap'' _ (VarTx _) = return $ mkNameE "id"
autofmap'' _ (BasicTx _) = return $ mkNameE "id"
autofmap'' _ (FixedTx _) = return $ mkNameE "id"
autofmap'' _ NotTx = fail "Thorn doesn't work well, sorry."
autofmap'' _ (FuncTx _) = fail "Thorn doesn't accept such a type with a kind * -> k, sorry."
autofmap'' u (DataTx nm vmp cxs) = do
    txnmes <- get
    put ((tx0, newFmap (length txnmes), Nothing) : txnmes)
    u2 <- unique
    e <- LamE [newVarP u2] <$> (CaseE (newVarE u2) <$> (mapM go cxs))
    txnmes' <- get
    put $ map (\(tx,nm',e') -> if tx==tx0 then (tx,nm',Just e) else (tx,nm',e')) txnmes'
    return e
    where go (nm',txs) = do
              (u2,es) <- autofmapmap u txs
              return $ Match (ConP nm' (map newVarP [u2..u2+length txs-1])) (NormalB (applistE (ConE nm') es)) []
          tx0 = SeenDataTx nm vmp
autofmap'' _ (SeenDataTx _ _) = fail "Thorn doesn't work well, sorry."
autofmap'' u (TupleTx txs) = do
    (u2,es) <- autofmapmap u txs
    return $ LamE [TupP (map newVarP [u2..u2+length txs-1])] (TupE es)
autofmap'' u (ArrowTx txa txb) = do
    fa <- autofmap' u txa
    fb <- autofmap' u txb
    u2 <- unique
    return $ LamE [newVarP u2, newVarP (u2+1)] (AppE fb (AppE (newVarE u2) (AppE fa (newVarE (u2+1)))))
autofmap'' u (ListTx tx) = autofmap' u tx >>= \f -> return $ AppE (mkNameE "map") f
autofmap'' u (SpecialTx n) = return $ newFuncE (u+n)

autofmapmap :: Unique -> [Typex] -> StateT [(Typex,Name,Maybe Exp)] Q (Unique,[Exp])
autofmapmap u txs = do
    u2 <- unique
    es <- mapM (\(i,tx) -> autofmap' u tx >>= \e -> return $ AppE e (newVarE i)) (zip [u2..u2+length txs-1] txs)
    return (u2,es)

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
    Co `mappend` Co = Co
    Contra `mappend` Contra = Contra
    _ `mappend` _ = Fixed
    mempty = Free

neg :: Variance -> Variance
neg Co = Contra
neg Contra = Co
neg Free = Free
neg Fixed = Fixed

includes :: Variance -> Variance -> Bool
includes _ Free = True
includes Free _ = False
includes Fixed _ = True
includes _ Fixed = False
includes Co Co = True
includes Contra Contra = True
includes _ _ = False

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
    (n,tx) <- t >>= type2typex [] [] >>= applySpecial 0
    (_,sq) <- runStateT (autovariance' Co [] tx) (S.replicate n Free)
    return $ (F.toList sq)

autovariance' :: Variance -> [(Name,[Conx],Variance)] -> Typex -> StateT (S.Seq Variance) Q ()
autovariance' _ _ (VarTx _) = return ()
autovariance' _ _ (BasicTx _) = return ()
autovariance' v _ (SpecialTx n) = do
    sq <- get
    put $ S.adjust (<>v) n sq
autovariance' _ _ (FixedTx _) = return ()
autovariance' _ _ NotTx = fail "Thorn doesn't work well, sorry."
autovariance' _ _ (FuncTx _) = fail "Thorn doesn't accept such a type with a kind * -> k, sorry."
autovariance' v dts (DataTx nm _ cxs) = mapM_ (mapM_ (autovariance' v ((nm,cxs,v):dts)) . cxtxs) cxs
autovariance' v dts (SeenDataTx nm _)
    | v' `includes` v = return ()
    | otherwise = mapM_ (mapM_ (autovariance' v dts') . cxtxs) cxs
    where Just (_,cxs,v') = find (\(nm',_,_) -> nm==nm') dts
          dts' = map (\tpl@(nm',_,_) -> if nm==nm' then (nm',cxs,v<>v') else tpl) dts
autovariance' v dts (TupleTx txs) = mapM_ (autovariance' v dts) txs
autovariance' v dts (ArrowTx txa txb) = autovariance' (neg v) dts txa >> autovariance' v dts txb
autovariance' v dts (ListTx tx) = autovariance' v dts tx

-- |
-- @autofunctorize t@ provides instance delcarations of the type @t@, for the suitable functor classes : Funtor, Contravariant, Bifunctor, or Profunctor.
autofunctorize :: TypeQ -> DecsQ
autofunctorize t = do
    vs <- autovarianceRaw t
    case vs of
         [Co] -> functor
         [Contra] -> contravariant
         [Free] -> (++) <$> functor <*> contravariant
         [Co,Co] -> bifunctor
         [Contra,Co] -> profunctor
         [Free,Co] -> (++) <$> bifunctor <*> profunctor
         _ -> fail "Thorn doesn't know any suitable functor class for this variance, sorry."
    where go cls member = do
              e <- autofmap t
              t' <- normalizetype =<< t
              return [InstanceD [] (AppT (ConT cls) t') [ValD (VarP member) (NormalB e) []]]
          functor = go (mkName "Prelude.Functor") (mkName "fmap")
          contravariant = go (mkName "Data.Functor.Contravariant.Contravariant") (mkName "contramap")
          bifunctor = go (mkName "Data.Bifunctor.Bifunctor") (mkName "bimap")
          profunctor = go (mkName "Data.Profunctor.Profunctor") (mkName "dimap")

