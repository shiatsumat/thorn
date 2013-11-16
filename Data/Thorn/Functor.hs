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
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Data.Monoid

newVar,newFunc,newFmap :: Int -> Name
newVar n = mkName $ "thornvariant" ++ show n
newVarP = VarP . newVar
newVarE = VarE . newVar
newFunc n = mkName $ "thornfunction" ++ show n
newFuncP = VarP . newFunc
newFuncE = VarE . newFunc
newFmap n = mkName $ "thornfmap" ++ show n
newFmapP = VarP . newFmap
newFmapE = VarE . newFmap

-- |
-- @autofmap t@ generates the @fmap@ of the type @t@.
-- 
-- Quite surprisingly, it still works for any arities, co\/contra\/free\/fixed-variances, partially applied types, type synonyms, and mutual recursions.
autofmap :: TypeQ -> ExpQ
autofmap t = do
    (n,tx) <- t >>= type2typex [] [] >>= apply 0
    (e,txnmes) <- runStateT (autofmap' tx) []
    return $ LamE (map newFuncP [0..n-1]) (LetE (fmap (\(_,nm,Just e) -> ValD (VarP nm) (NormalB e) []) txnmes) e)

apply :: Int -> Typex -> Q (Int,Typex)
apply n (FuncTx f) = f (SpecialTx n) >>= apply (n+1)
apply n tx@(VarTx _) = return (n,tx)
apply n tx@(BasicTx _) = return (n,tx)
apply n tx@(SpecialTx _) = return (n,tx)
apply n tx@(FixedTx _) = return (n,tx)
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
autofmap'' (FixedTx _) = return $ mkNameE "id"
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
              return $ Match (ConP nm (map newVarP [0..length txs-1])) (NormalB (applistE (ConE nm) es)) []
          go (InfixCx nm txa txb) = do
              [ea,eb] <- autofmapmap [txa,txb]
              return $ Match (InfixP (newVarP 0) nm (newVarP 1)) (NormalB (InfixE (Just ea) (ConE nm) (Just eb))) []
          tx0 = SeenDataTx nm vmp
autofmap'' (SeenDataTx _ _) = fail "Autofmap doesn't work well, sorry."
autofmap'' (TupleTx txs) = do
    es <- autofmapmap txs
    return $ LamE [TupP (map newVarP [0..length txs-1])] (TupE es)
autofmap'' (ArrowTx txa txb) = do
    fa <- autofmap' txa
    fb <- autofmap' txb
    return $ LamE [newVarP 0, newVarP 1] (AppE fb (AppE (newVarE 0) (AppE fa (newVarE 1))))
autofmap'' (ListTx tx) = autofmap' tx >>= \f -> return $ AppE (mkNameE "map") f
autofmap'' (SpecialTx n) = return $ newFuncE n

autofmapmap :: [Typex] -> StateT [(Typex,Name,Maybe Exp)] Q [Exp]
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
    (n,tx) <- t >>= type2typex [] [] >>= apply 0
    (_,sq) <- runStateT (autovariance' Co [] tx) (S.replicate n Free)
    return $ (F.toList sq)

autovariance' :: Variance -> [(Name,[Conx],Variance)] -> Typex -> StateT (S.Seq Variance) Q ()
autovariance' v _ (SpecialTx n) = do
    sq <- get
    put $ S.adjust (<>v) n sq
autovariance' _ _ (VarTx _) = return ()
autovariance' _ _ (BasicTx _) = return ()
autovariance' _ _ (FixedTx _) = return ()
autovariance' _ _ (FuncTx _) = fail "Automap doesn't accept such a type with a kind * -> k."
autovariance' v dts (DataTx nm _ cxs) = mapM_ (mapM_ (autovariance' v ((nm,cxs,v):dts)) . cxtxs) cxs
autovariance' v dts (SeenDataTx nm _)
    | v' `includes` v = return ()
    | otherwise = mapM_ (mapM_ (autovariance' v ((nm,cxs,v):dts)) . cxtxs) cxs
    where Just (_,cxs,v') = find (\(nm',_,_) -> nm==nm') dts
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
         _ -> fail "autofunctorize doesn't know the suitable functor class for this variance"
    where go cls member = do
              e <- autofmap t
              t' <- normalizetype =<< t
              return [InstanceD [] (AppT (ConT cls) t') [ValD (VarP member) (NormalB e) []]]
          functor = go (mkName "Prelude.Functor") (mkName "fmap")
          contravariant = go (mkName "Data.Functor.Contravariant.Contravariant") (mkName "contramap")
          bifunctor = go (mkName "Data.Bifunctor.Bifunctor") (mkName "bimap")
          profunctor = go (mkName "Data.Profunctor.Profunctor") (mkName "dimap")

