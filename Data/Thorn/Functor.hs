{-# LANGUAGE TemplateHaskell #-}

-- |
-- The module Data.Thorn.Functor.
module Data.Thorn.Functor (
    -- * Functors
    -- $functor
    autofmap, autofmaptype, autofmapdec, autofunctorize
    
    -- ** Variance
  , Variance(..)
  , autovariance
    
    -- * Examples
     
    -- ** Basic
    -- $basic
    
    -- ** Functions
    -- $function
    
    -- ** Partial Application
    -- $partial
    
    -- ** Type Synonyms
    -- $synonym
    
    -- ** Variances
    -- $variance
    
    -- ** Recursive Types
    -- $recursive
    ) where

import Data.Thorn.Internal
import Language.Haskell.TH
import Data.Maybe
import Data.List
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Monoid
import Control.Applicative
import Control.Monad.State

{- $functor
    You can generate functors from various kinds of datatypes.

    Quite surprisingly, it still works for any arities, co\/contra\/free\/fixed-variances, partially applied types, type synonyms, and mutual recursions.
-}

{- $basic

    It's a piece of cake.

> testtuple :: (Int,String)
> testtuple = $(autofmap [t|(,)|]) (+1) ('h':) (100,"ello") -- (101,"hello")
> 
> testeither :: Either Int String
> testeither = $(autofmap [t|Either|]) (+1) ('a':) (Left 100) -- Left 101
> 
> testfunction :: String
> testfunction = $(autofmap [t|(->)|]) ('h':) (++"!") (++", world") "ello" -- "hello, world!"
> 
> testlist :: [Int]
> testlist = $(autofmap [t|[]|]) (+10) [1..5] -- [11..15]

-}

{- $function

    You can nest functions.

> data FunFun a b = FunFun ((b -> a) -> b)
> 
> varfunfun :: [Variance]
> varfunfun = $(autovariance [t|FunFun|]) -- [Contra,Co]
> 
> autofunctorize [t|FunFun|]
> -- instance Profunctor FunFun where
> --     dimap = ...

-}

{- $partial

    It works for partially applied types.

> testpartial :: (Int,Int,Int)
> testpartial = $(autofmap $[t|(,,) Int|]) (+10) (+20) (1,1,1) -- (1,11,21)

    You can use type variants @'T0', 'T1', ..., 'T9'@ to represent any type.

> testpartial' :: (String,Int,Int)
> testpartial' = $(autofmap $[t|(,,) T0|]) (+10) (+20) ("hello",1,1) -- ("hello",11,21)

-}

{- $synonym

    Interestingly, it works for type synonyms.

> type a :<- b = b -> a
> varnuf :: [Variance]
> varnuf = $(autovariance [t|(:<-)|]) -- [Co,Contra]
> $(autofmapdec "fmapnuf" [t|(:<-)|])

-}

{- $variance

    It works for free-variance and fixed-variance. See how @autofunctorize@ works for free-variance.

> data What a b c = What1 c (a -> c) | What2 a
> 
> varwhat :: [Variance]
> varwhat = $(autovariance [t|What|]) -- [Fixed,Free,Co]
> 
> autofunctorize [t|What T0|]
> -- instance Bifunctor (What a) where
> --     bimap = ...
> -- instance Profunctor (What a) where
> --     dimap = ...

-}

{- $recursive

    It works for recursive datatypes.

> data List a = Nil | a :* (List a) deriving Show
> 
> autofunctorize [t|List|]
> -- instance Functor List where
> --     fmap = ...
> 
> fromNormalList :: [a] -> List a
> fromNormalList [] = Nil
> fromNormalList (a : as) = a :* fromNormalList as
> toNormalList :: List a -> [a]
> toNormalList Nil = []
> toNormalList (a :* as) = a : toNormalList as
> 
> testlist :: [Int]
> testlist = toNormalList $ fmap (+10) (fromNormalList [1..5]) -- [11..15]

    It also works for mutually recursive datatypes.

> data Rose a = Rose a (Forest a) deriving Show
> data Forest a = Forest [Rose a] deriving Show
> 
> autofunctorize [t|Rose|]
> -- instance Functor Rose where
> --     fmap = ...
> 
> gorose :: Int -> Rose Int
> gorose 0 = Rose 0 (Forest [])
> gorose n = Rose n (Forest (replicate 2 (gorose (n-1))))
> testrose :: Rose Int
> testrose = fmap (+10) (gorose 2)
> -- Rose 12 (Forest [Rose 11 (Forest [Rose 10 (Forest []),Rose 10 (Forest [])]),Rose 11 (Forest [Rose 10 (Forest []),Rose 10 (Forest [])])])

-}

-- |
-- @autofmap t@ generates an fmap of the type @t@.
autofmap :: TypeQ -> ExpQ
autofmap t = do
    (n,tx) <- t >>= type2typex [] [] >>= applySpecial 0
    u <- unique
    (e,(txnmes,bs)) <- runStateT (autofmap' u tx) ([],S.replicate n False)
    let txnmes' = filter (\(_,nm,_) -> isJust nm) txnmes
    return $ LamE (map (\i -> if S.index bs i then newFuncP (i+u) else WildP) [0..n-1]) (LetE (fmap (\(_,Just nm,Just e') -> ValD (VarP nm) (NormalB e') []) txnmes') e)

autofmap',autofmap'' :: Unique -> Typex -> StateT ([(Typex,Maybe Name,Maybe Exp)],S.Seq Bool) Q Exp
autofmap' u tx = do
    (txnmes,bs) <- get
    case find (\(tx',_,_)->tx==tx') txnmes of
         Just (_,Just nm,_) -> return (VarE nm)
         Just (_,Nothing,_) -> do
             u2 <- unique
             let nm = newFmap u2
             put (map (\(tx',nm',e) -> if tx==tx' then (tx,Just nm,e) else (tx',nm',e)) txnmes, bs)
             return (VarE nm)
         Nothing -> autofmap'' u tx
autofmap'' _ (VarTx _) = return idE
autofmap'' _ (BasicTx _) = return idE
autofmap'' _ (FixedTx _) = return idE
autofmap'' _ NotTx = fail "Thorn doesn't work well, sorry."
autofmap'' _ (FuncTx _) = fail "Thorn doesn't accept such a type with a kind * -> k, sorry."
autofmap'' u (DataTx nm vmp cxs) = do
    (txnmes,bs) <- get
    put ((tx0,Nothing,Nothing) : txnmes, bs)
    u2 <- unique
    e <- LamE [newVarP u2] <$> (CaseE (newVarE u2) <$> (mapM go cxs))
    (txnmes',bs') <- get
    put (map (\(tx,nm',e') -> if tx==tx0 then (tx,nm',Just e) else (tx,nm',e')) txnmes', bs')
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
autofmap'' u (SpecialTx n) = do
    (txnmes,bs) <- get
    put (txnmes,S.update n True bs)
    return $ newFuncE (u+n)

autofmapmap :: Unique -> [Typex] -> StateT ([(Typex,Maybe Name,Maybe Exp)],S.Seq Bool) Q (Unique,[Exp])
autofmapmap u txs = do
    u2 <- unique
    es <- mapM (\(i,tx) -> autofmap' u tx >>= \e -> return $ AppE e (newVarE i)) (zip [u2..u2+length txs-1] txs)
    return (u2,es)

-- |
-- @Variance@ is a variance of a parameter of a functor.
data Variance =
    -- | Covariance, one of a normal functor.
    Co
    -- | Contravariance, the dual of covariance.
  | Contra
    -- | Free-variance, being supposed to satisfy either covariance or contravariance.
  | Free
    -- | Fixed-variance, being supposed to satisfy both covariance and contravariance.
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
-- @autofmaptype t@ provides the type of @$('autofmap' t)@.
autofmaptype :: TypeQ -> TypeQ
autofmaptype t = do
    tx <- type2typex [] [] =<< t
    vs <- autovarianceRaw t
    let ivs = zip [0..length vs-1] vs
        a i = mkNameTx ("a"++show i)
        b i = mkNameTx ("b"++show i)
        c i = mkNameTx ("c"++show i)
        a' i = mkName ("a"++show i)
        b' i = mkName ("b"++show i)
        c' i = mkName ("c"++show i)
        gofunc (i,Co) = ArrowTx (a i) (b i)
        gofunc (i,Contra) = ArrowTx (b i) (a i)
        gofunc (i,Free) = a i
        gofunc (i,Fixed) = ArrowTx (a i) (a i)
        gosrc (i,Co) = a i
        gosrc (i,Contra) = a i
        gosrc (i,Free) = b i
        gosrc (i,Fixed) = a i
        godst (i,Co) = b i
        godst (i,Contra) = b i
        godst (i,Free) = c i
        godst (i,Fixed) = a i
        gonm (i,Co) = [a' i,b' i]
        gonm (i,Contra) = [a' i,b' i]
        gonm (i,Free) = [a' i,b' i,c' i]
        gonm (i,Fixed) = [a' i]
        tvs = map PlainTV $ concatMap gonm ivs
    funcs <- mapM (typex2type . gofunc) ivs
    src <- typex2type =<< applistTx tx (map gosrc ivs)
    dst <- typex2type =<< applistTx tx (map godst ivs)
    return $ ForallT tvs [] (foldr1 (\ta tb -> applistT ArrowT [ta,tb]) (funcs++[src]++[dst]))

-- |
-- @autofmapdec s t@ provides a declaration of an fmap for the type @t@ with the name @s@, with a type signature.
autofmapdec :: String -> TypeQ -> DecsQ
autofmapdec = gendec1 autofmap autofmaptype

-- |
-- @autofunctorize t@ provides instance delcarations of the type @t@, for the suitable functor classes : 'Functor', 'Data.Functor.Contravariant.Contravariant', 'Data.Bifunctor.Bifunctor', or 'Data.Profunctor.Profunctor'. Multiple classes can be suitable for @t@, when one of the variances of @t@ is 'Free'.
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

