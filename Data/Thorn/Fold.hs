{-# LANGUAGE TemplateHaskell #-}

-- |
-- The module Data.Thorn.Fold.
module Data.Thorn.Fold (
    -- * Folding and Unfolding
    -- $fold
    unfixdata, unfixdataEx, autofold, autofoldtype, autofolddec, autounfold, autounfoldtype, autounfolddec
    -- ** Mutual Recursion
  , unfixdataMutual, unfixdataMutualEx, autofoldMutual, autofoldtypeMutual, autofolddecMutual, autounfoldMutual, autounfoldtypeMutual, autounfolddecMutual
    -- ** Primitive Functions
  , autoin, autoout, autohylo
  , autoinMutual, autooutMutual, autohyloMutual

    -- * Example
    -- $foldexample
    ) where

import Data.Thorn.Internal
import Data.Thorn.Functor
import Language.Haskell.TH
import Data.List
import Control.Applicative

{- $fold
    Thorn generates folds and unfolds from various kinds of recursive datatypes, including mutually recursive ones.
-}

{- $foldexample
> import Data.Thorn
> import Data.Functor.Contravariant
> import Data.Bifunctor
> import Data.Profunctor
> 
> type a :<- b = b -> a
> varnuf :: [Variance]
> varnuf = $(autovariance [t|(:<-)|]) -- [Co,Contra]
> $(autofmapdec "fmapnuf" [t|(:<-)|])
> 
> data Cntr a = Cntr (a -> Int)
> autofunctorize [t|Cntr|] -- instance Contravariant Cntr where ...
> 
> vartuple :: [Variance]
> vartuple = $(autovariance [t|(,,) Int|]) -- [Co,Co]
> $(autofmapdec "fmaptuple" $[t|(,,) Int|])
> 
> data FunFun a b = FunFun ((b -> a) -> b)
> varfunfun :: [Variance]
> varfunfun = $(autovariance [t|FunFun|]) -- [Contra,Co]
> autofunctorize [t|FunFun|] -- instance Profunctor FunFun where ...
> 
> data What a b c = What1 c (a -> c) | What2 a
> varwhat :: [Variance]
> varwhat = $(autovariance [t|What|]) -- [Fixed,Free,Co]
> autofunctorize [t|What T0|]
> -- instance Bifunctor (What a) where ... and
> -- instance Profunctor (What a) where ...
> 
> data List a = Nil | a :* (List a) deriving Show
> autofunctorize [t|List|] -- instance Functor List where ...
> fromNormalList :: [a] -> List a
> fromNormalList [] = Nil
> fromNormalList (a : as) = a :* fromNormalList as
> toNormalList :: List a -> [a]
> toNormalList Nil = []
> toNormalList (a :* as) = a : toNormalList as
> list :: [Int]
> list = toNormalList $ fmap (+10) (fromNormalList [1..5]) -- [11..15]
> 
> data Rose a = Rose a (Forest a) deriving Show
> data Forest a = Forest [Rose a] deriving Show
> autofunctorize [t|Rose|] -- instance Functor Rose where ...
> autofunctorize [t|Forest|] -- instance Functor Forest where ...
> gorose :: Int -> Rose Int
> gorose n = Rose n (Forest (replicate n (gorose (n-1))))
> getrose :: Rose Int
> getrose = fmap (+1) (gorose 2)
-}

-- |
-- @unfixdata t@ provides a declaration of a nonrecursive datatype whose fixpoint is the recursive type @t@.
unfixdata :: TypeQ -> DecsQ
unfixdata = unfixdataEx ("Uf","") ("Uf","") ("&","") ("&","")

-- |
-- Special version of @unfixdata@. Note that
--
-- > unfixdata = unfixdataEx ("Uf","") ("Uf","") ("&","") ("&","")
unfixdataEx ::
    (String,String) -- ^ prefix and suffix of type constructor
 -> (String,String) -- ^ prefix and suffix of data constructor
 -> (String,String) -- ^ prefix and suffix of infix type constructor
 -> (String,String) -- ^ prefix and suffix of infix data constructor
 -> TypeQ -- ^ data type
 -> DecsQ -- ^ declaration of a datatype
unfixdataEx (pretype,suftype) (predata,sufdata) (pretypeinfix,suftypeinfix) (predatainfix,sufdatainfix) t =
    unfixdataMutualEx (pretype,suftype) (predata,sufdata) (pretypeinfix,suftypeinfix) (predatainfix,sufdatainfix) [t]

autoin ::
    TypeQ -- ^ @u@, nonrecursive datatype
 -> TypeQ -- ^ @t@, fixpoint of @u@
 -> ExpQ -- ^ function with a type @u a0 .. an t -> t a0 .. an@
autoin u t = autoinMutual [(u,t)] 0

autoout ::
    TypeQ -- ^ @u@, nonrecursive datatype
 -> TypeQ -- ^ @t@, fixpoint of @u@
 -> ExpQ -- ^ function with a type @t x0 .. xn -> u x0 .. xn t@
autoout u t = autooutMutual [(u,t)] 0

autohylo ::
    TypeQ -- ^ @u@, nonrecursive datatype
 -> ExpQ -- ^ function with a type @(a -> u x0 .. xn a) -> (u x0 .. xn b -> b) -> (a -> b)@
autohylo u = autohyloMutual [u] 0

-- |
-- @autofold u t@ provides a folding function for the recursive type @t@.
autofold ::
    TypeQ -- ^ @u@, nonrecursive datatype
 -> TypeQ -- ^ @t@, fixpoint of @u@
 -> ExpQ -- ^ function with a type @(u x0 .. xn a -> a) -> (t -> a)@
autofold u t = autofoldMutual [(u,t)] 0

-- |
-- @autofoldtype u t@ provides the type of @$(autofoldtype u t)@.
autofoldtype :: TypeQ -> TypeQ -> TypeQ
autofoldtype u t = autofoldtypeMutual [(u,t)] 0

-- |
-- @autofolddec s u t@ provides a declaration of a folding function for the recursive type @t@ with the name @s@, with a type signature.
autofolddec :: String -> TypeQ -> TypeQ -> DecsQ
autofolddec = gendec2 autofold autofoldtype

-- |
-- @autounfold t@ provides an unfolding function for the recursive type @t@.
autounfold ::
    TypeQ -- ^ @u@, nonrecursive datatype
 -> TypeQ -- ^ @t@, fixpoint of @u@
 -> ExpQ -- ^ function with a type @(a -> u x0 .. xn a) -> (a -> t)@
autounfold u t = do
    e <- autounfoldMutual [(u,t)] 0
    return e

-- |
-- @autounfoldtype u t@ provides the type of @$(autounfoldtype u t)@.
autounfoldtype :: TypeQ -> TypeQ -> TypeQ
autounfoldtype u t = autounfoldtypeMutual [(u,t)] 0

-- |
-- @autounfolddec s u t@ provides a declaration of an unfolding function for the recursive type @t@ with the name @s@, with a type signature.
autounfolddec :: String -> TypeQ -> TypeQ -> DecsQ
autounfolddec = gendec2 autounfold autounfoldtype

-- |
-- Mutually recursive version of @unfixdata@. Note that
--
-- > unfixdata t = unfixdataMutual [t]
unfixdataMutual :: [TypeQ] -> DecsQ
unfixdataMutual = unfixdataMutualEx ("Uf","") ("Uf","") ("&","") ("&","")

-- |
-- Special version of @unfixdataMutual@. Note that
--
-- > unfixdataMutual = unfixdataMutualEx ("Uf","") ("Uf","") ("&","") ("&","")
unfixdataMutualEx ::
    (String,String) -- ^ prefix and suffix of type constructor
 -> (String,String) -- ^ prefix and suffix of data constructor
 -> (String,String) -- ^ prefix and suffix of infix type constructor
 -> (String,String) -- ^ prefix and suffix of infix data constructor
 -> [TypeQ] -- ^ recursive datatypes
 -> DecsQ -- ^ declarations of data
unfixdataMutualEx pstype psdata pstypeinfix psdatainfix ts = do
    tpls <- mapM (\t -> type2typex [] [] t >>= applyFixed 0) =<< sequence ts
    let nms = map (\(_, DataTx nm _ _) -> nm) tpls
        cxss = map (\(_, DataTx _ _ cxs) -> cxs) tpls
        l = length tpls
        (n,_) = head tpls
        modifytx (DataTx nm vmp cxs) = case elemIndex nm nms of
            Just k -> VarTx $ mkName ("self" ++ show k)
            Nothing -> DataTx nm (map (\(nm',tx) -> (nm',modifytx tx)) vmp) (map modifycx cxs)
        modifytx tx@(SeenDataTx nm _) = case elemIndex nm nms of
            Just k -> VarTx $ mkName ("self" ++ show k)
            Nothing -> tx
        modifytx (TupleTx txs) = TupleTx (map modifytx txs)
        modifytx (ArrowTx txa txb) = ArrowTx (modifytx txa) (modifytx txb)
        modifytx (ListTx tx) = ListTx (modifytx tx)
        modifytx tx = tx
        modifycx (nm,txs) = (nm,map modifytx txs)
        go (nm,txs) = do
              ts' <- map ((,) NotStrict) <$> mapM (typex2type . modifytx) txs
              return $ NormalC (datanm nm) ts'
        ho (nm,cns) = DataD [] (typenm nm) (map var [0..n-1] ++ map self [0..l-1]) cns []
    cnss <- mapM (mapM go) cxss
    return $ map ho (zip nms cnss)
    where typenm = modifyname pstype pstypeinfix
          datanm = modifyname psdata psdatainfix
          var i = PlainTV $ mkName ("t" ++ show i)
          self i = PlainTV $ mkName ("self" ++ show i)

-- |
-- Mutually recursive version of @autoin@.
autoinMutual ::
    [(TypeQ,TypeQ)] -- ^ @[(u0,t0), .., (un,tn)]@; @ui@ is an nonrecursive datatype and @ti@ is a fixpoint of @ui@
 -> Int -- ^ @k@; an index
 -> ExpQ -- ^ @in@ for @uk@
autoinMutual uts k = do
    cxsus <- mapM (\(u,_) -> u >>= type2typex [] [] >>= applyFixed 0 >>= return . getcxs . snd) uts
    cxsts <- mapM (\(_,t) -> t >>= type2typex [] [] >>= applyFixed 0 >>= return . getcxs . snd) uts
    let cxsu = cxsus !! k
        cxst = cxsts !! k
    u1 <- unique
    u2 <- unique
    let go ((nmu,txsu),(nmt,_)) = Match (ConP nmu (map newVarP [u2..u2+l-1])) (NormalB (applistE (ConE nmt) (map newVarE [u2..u2+l-1]))) []
            where l = length txsu
    return $ LamE [newVarP u1] (CaseE (newVarE u1) (map go (zip cxsu cxst)))
    where getcxs (DataTx _ _ cxs) = cxs
          getcxs _ = error "Thorn doesn't work well, sorry."

-- |
-- Mutually recursive version of @autoout@.
autooutMutual ::
    [(TypeQ,TypeQ)] -- ^ @[(u0,t0), .., (un,tn)]@; @ui@ is an nonrecursive datatype and @ti@ is a fixpoint of @ui@
 -> Int -- ^ @k@; an index
 -> ExpQ -- ^ @out@ for @uk@
autooutMutual uts k = do
    cxsus <- mapM (\(u,_) -> u >>= type2typex [] [] >>= applyFixed 0 >>= return . getcxs . snd) uts
    cxsts <- mapM (\(_,t) -> t >>= type2typex [] [] >>= applyFixed 0 >>= return . getcxs . snd) uts
    let cxsu = cxsus !! k
        cxst = cxsts !! k
    u1 <- unique
    u2 <- unique
    let go ((nmu,txsu),(nmt,_)) = Match (ConP nmt (map newVarP [u2..u2+l-1])) (NormalB (applistE (ConE nmu) (map newVarE [u2..u2+l-1]))) []
            where l = length txsu
    return $ LamE [newVarP u1] (CaseE (newVarE u1) (map go (zip cxsu cxst)))
    where getcxs (DataTx _ _ cxs) = cxs
          getcxs _ = error "Thorn doesn't work well, sorry."

-- |
-- Mutually recursive version of @autohylo@.
autohyloMutual ::
    [TypeQ] -- ^ @[u0, .., un]@; @ui@ is an nonrecursive datatype
 -> Int -- ^ @k@; an index
 -> ExpQ -- ^ @hylo@ for @uk@
autohyloMutual us k = do
    ms <- mapM (\u -> u >>= type2typex [] [] >>= applyFixed 0 >>= \(m,DataTx _ _ _) -> return m) us
    fms <- mapM autofmap us
    u1 <- unique
    let n = length us
        m i = (ms !! i) - n
        f i = mkName ("f"++show (u1+i))
        g i = mkName ("g"++show (u1+i))
        x = newVar u1
        fm i = fms !! i
        hylo i = mkName ("hylo"++show i)
        hylodef i = ValD (VarP $ hylo i) (NormalB (LamE [VarP x] (
            AppE (VarE $ g i) (applistE (fm i) (replicate (m i) idE ++ map (VarE . hylo) [0..n-1] ++ [AppE (VarE $ f i) (VarE x)]))
            ))) []
    return $ LamE (map (VarP . f) [0..n-1] ++ map (VarP . g) [0..n-1]) (LetE (map hylodef [0..n-1]) (VarE $ hylo k))
    {-
        \f0 .. fn-1 g0 .. gn-1 ->
            let hylo0 = \x -> g0 (fm0 hylo0 .. hylon-1 (f1 x))
                ..
            in hylok
    -}

-- |
-- @autofoldMutual ts@ provides a folding function for the mutually recursive types @ts@
autofoldMutual ::
    [(TypeQ,TypeQ)] -- ^ @[(u0,t0), .., (un,tn)]@; @ui@ is an nonrecursive datatype and @ti@ is a fixpoint of @ui@
 -> Int -- ^ @k@; an index
 -> ExpQ -- ^ @fold@ for @uk@
autofoldMutual uts k = do
    os <- mapM (autooutMutual uts) [0..n-1]
    h <- autohyloMutual (map fst uts) k
    return $ applistE h os
    where n = length uts

autofoldtypeMutual :: [(TypeQ,TypeQ)] -> Int -> TypeQ
autofoldtypeMutual uts k = do
    mtxs <- mapM (\(_,t) -> t >>= type2typex [] [] >>= applyFixed 0) uts
    let n = length uts
        ms = map fst mtxs
        txs = map snd mtxs
    t <- typex2type $ txs !! k
    uxs <- mapM (\(m,(u,_)) -> u >>= type2typex [] [] >>= applyFixed' m 0) (zip ms uts)
    let f i = do
            uxa <- applistTx (uxs !! i) (map (VarTx . a) [0..n-1])
            typex2type (ArrowTx uxa (VarTx $ a i))
        a i = mkName ("a"++show i)
        x i = mkName ("t"++show i)
    fs <- mapM f [0..n-1]
    return $ ForallT (map (PlainTV . x) [0..ms!!k-1] ++ map (PlainTV . a) [0..n-1]) [] (
        foldr1 (\t1 t2 -> AppT (AppT ArrowT t1) t2) (fs ++ [t, VarT $ a k]))

autofolddecMutual :: String -> [(TypeQ,TypeQ)] -> Int -> DecsQ
autofolddecMutual = gendec2 autofoldMutual autofoldtypeMutual

-- |
-- @autounfoldMutual ts@ provides an unfolding function for the mutually recursive types @ts@.
autounfoldMutual ::
    [(TypeQ,TypeQ)] -- ^ @[(u0,t0), .., (un,tn)]@; @ui@ is an nonrecursive datatype and @ti@ is a fixpoint of @ui@
 -> Int -- ^ @k@; an index
 -> ExpQ -- ^ @unfold@ for @uk@
autounfoldMutual uts k = do
    is <- mapM (autoinMutual uts) [0..n-1]
    h <- autohyloMutual (map fst uts) k
    u <- unique
    return $ LamE (map newFuncP [u..u+n-1]) (applistE h (map newFuncE [u..u+n-1]++is))
    where n = length uts

autounfoldtypeMutual :: [(TypeQ,TypeQ)] -> Int -> TypeQ
autounfoldtypeMutual uts k = do
    mtxs <- mapM (\(_,t) -> t >>= type2typex [] [] >>= applyFixed 0) uts
    let n = length uts
        ms = map fst mtxs
        txs = map snd mtxs
    t <- typex2type $ txs !! k
    uxs <- mapM (\(m,(u,_)) -> u >>= type2typex [] [] >>= applyFixed' m 0) (zip ms uts)
    let f i = do
            uxa <- applistTx (uxs !! i) (map (VarTx . a) [0..n-1])
            typex2type (ArrowTx (VarTx $ a i) uxa)
        a i = mkName ("a"++show i)
        x i = mkName ("t"++show i)
    fs <- mapM f [0..n-1]
    return $ ForallT (map (PlainTV . x) [0..ms!!k-1] ++ map (PlainTV . a) [0..n-1]) [] (
        foldr1 (\t1 t2 -> AppT (AppT ArrowT t1) t2) (fs ++ [VarT $ a k, t]))

autounfolddecMutual :: String -> [(TypeQ,TypeQ)] -> Int -> DecsQ
autounfolddecMutual = gendec2 autounfoldMutual autounfoldtypeMutual

