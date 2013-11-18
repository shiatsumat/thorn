{-# LANGUAGE TemplateHaskell #-}

-- |
-- The module Data.Thorn.Fold.
module Data.Thorn.Fold (
    unfixdata, unfixdataEx, autofold, autofoldtype, autofolddec, autounfold, autounfoldtype, autounfolddec
  , unfixdataMutual, unfixdataMutualEx, autofoldMutual, autofoldtypeMutual, autofolddecMutual, autounfoldMutual, autounfoldtypeMutual, autounfolddecMutual
  , autoin, autoout, autohylo
  , autoinMutual, autooutMutual, autohyloMutual
    ) where

import Data.Thorn.Type
import Data.Thorn.Functor
import Language.Haskell.TH
import Data.List
import Control.Applicative

-- |
-- @unfixdata t@ provides a declaration of a data whose fixpoint is the recursive type @t@.
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
 -> DecsQ -- ^ declaration of data
unfixdataEx (pretype,suftype) (predata,sufdata) (pretypeinfix,suftypeinfix) (predatainfix,sufdatainfix) t =
    unfixdataMutualEx (pretype,suftype) (predata,sufdata) (pretypeinfix,suftypeinfix) (predatainfix,sufdatainfix) [t]

autoin ::
    TypeQ -- ^ @u@, un-recursive datatype
 -> TypeQ -- ^ @t@, fixpoint of @u@
 -> ExpQ -- ^ function with a type @u a0 .. an t -> t a0 .. an@
autoin u t = do
    TupE [e] <- autoinMutual [(u,t)]
    return e

autoout ::
    TypeQ -- ^ @u@, un-recursive datatype
 -> TypeQ -- ^ @t@, fixpoint of @u@
 -> ExpQ -- ^ function with a type @t x0 .. xn -> u x0 .. xn t@
autoout u t = do
    TupE [e] <- autooutMutual [(u,t)]
    return e

autohylo ::
    TypeQ -- ^ @u@, un-recursive datatype
 -> ExpQ -- ^ function with a type @(a -> u x0 .. xn a) -> (u x0 .. xn b -> b) -> (a -> b)@
autohylo u = do
    TupE [e] <- autohyloMutual [u]
    return e

-- |
-- @autofold u t@ provides a folding function for the recursive type @t@.
autofold ::
    TypeQ -- ^ @u@, un-recursive datatype
 -> TypeQ -- ^ @t@, fixpoint of @u@
 -> ExpQ -- ^ function with a type @(u x0 .. xn a -> a) -> (t -> a)@
autofold u t = do
    TupE [e] <- autofoldMutual [(u,t)]
    return e

-- |
-- @autofoldtype u t@ provides the type of @$(autofoldtype u t)@.
autofoldtype :: TypeQ -> TypeQ -> TypeQ
autofoldtype u t = head <$> autofoldtypeMutual [(u,t)]

-- |
-- @autofolddec s u t@ provides a declaration of a folding function for the recursive type @t@ with the name @s@, with a type signature.
autofolddec :: String -> TypeQ -> TypeQ -> DecsQ
autofolddec = gendec2 autofold autofoldtype

-- |
-- @autounfold t@ provides an unfolding function for the recursive type @t@.
autounfold ::
    TypeQ -- ^ @u@, un-recursive datatype
 -> TypeQ -- ^ @t@, fixpoint of @u@
 -> ExpQ -- ^ function with a type @(a -> u x0 .. xn a) -> (a -> t)@
autounfold u t = do
    TupE [e] <- autounfoldMutual [(u,t)]
    return e

-- |
-- @autounfoldtype u t@ provides the type of @$(autounfoldtype u t)@.
autounfoldtype :: TypeQ -> TypeQ -> TypeQ
autounfoldtype u t = head <$> autounfoldtypeMutual [(u,t)]

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
unfixdataMutualEx (pretype,suftype) (predata,sufdata) (pretypeinfix,suftypeinfix) (predatainfix,sufdatainfix) ts = do
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
    where typenm nm
            | elem (head s) ['A'..'Z'] = mkName $ pretype ++ s ++ suftype
            | head s == '(' = mkName $ ":" ++ pretypeinfix ++ init (drop 2 s) ++ suftypeinfix
            | otherwise = mkName $ ":" ++ pretypeinfix ++ tail s ++ suftypeinfix
            where s = nameBase nm
          datanm nm
            | elem (head s) ['A'..'Z'] = mkName $ predata ++ s ++ sufdata
            | head s == '(' = mkName $ ":" ++ predatainfix ++ init (drop 2 s) ++ sufdatainfix
            | otherwise = mkName $ ":" ++ predatainfix ++ tail s ++ sufdatainfix
            where s = nameBase nm
          var i = PlainTV $ mkName ("t" ++ show i)
          self i = PlainTV $ mkName ("self" ++ show i)

-- |
-- Mutually recursive version of @autoin@.
autoinMutual ::
    [(TypeQ,TypeQ)] -- ^ @[(u0,t0), .., (un,tn)]@; @ui@ is an un-recursive datatype and @ti@ is a fixpoint of @ui@
 -> ExpQ -- ^ @(in0, .., inn)@; tuple of @in@
autoinMutual uts = do
    cxsus <- mapM (\(u,_) -> u >>= type2typex [] [] >>= applyFixed 0 >>= return . getcxs . snd) uts
    cxsts <- mapM (\(_,t) -> t >>= type2typex [] [] >>= applyFixed 0 >>= return . getcxs . snd) uts
    u1 <- unique
    u2 <- unique
    let go ((nmu,txsu),(nmt,_)) = Match (ConP nmu (map newVarP [u2..u2+length txsu-1])) (NormalB (applistE (ConE nmt) (map newVarE [u2..u2+length txsu-1]))) []
        ho (cxsu,cxst) = LamE [newVarP u1] (CaseE (newVarE u1) (map go (zip cxsu cxst)))
    return $ TupE (map ho (zip cxsus cxsts))
    where getcxs (DataTx _ _ cxs) = cxs
          getcxs _ = error "Thorn doesn't work well, sorry."

-- |
-- Mutually recursive version of @autoout@.
autooutMutual ::
    [(TypeQ,TypeQ)] -- ^ @[(u0,t0), .., (un,tn)]@; @ui@ is an un-recursive datatype and @ti@ is a fixpoint of @ui@
 -> ExpQ -- ^ @(out0, .., outn)@; tuple of @out@
autooutMutual uts = do
    cxsus <- mapM (\(u,_) -> u >>= type2typex [] [] >>= applyFixed 0 >>= \(_,DataTx _ _ cxs) -> return cxs) uts
    cxsts <- mapM (\(_,t) -> t >>= type2typex [] [] >>= applyFixed 0 >>= \(_,DataTx _ _ cxs) -> return cxs) uts
    u1 <- unique
    u2 <- unique
    let go ((nmu,txsu),(nmt,_)) = Match (ConP nmt (map newVarP [u2..u2+length txsu-1])) (NormalB (applistE (ConE nmu) (map newVarE [u2..u2+length txsu-1]))) []
        ho (cxsu,cxst) = LamE [newVarP u1] (CaseE (newVarE u1) (map go (zip cxsu cxst)))
    return $ TupE (map ho (zip cxsus cxsts))

-- |
-- Mutually recursive version of @autohylo@.
autohyloMutual ::
    [TypeQ] -- ^ @[u0, .., un]@; @ui@ is an un-recursive datatype
 -> ExpQ -- ^ @(hylo0, .., hylon)@; tuple of @hylo@
autohyloMutual us = do
    ns <- mapM (\u -> u >>= type2typex [] [] >>= applyFixed 0 >>= \(n,DataTx _ _ _) -> return n) us
    fms <- mapM autofmap us
    u1 <- unique
    u2 <- unique
    let l = length ns
        go (i,n,fm) = ValD (VarP (h i)) (NormalB $ LamE [VarP f, VarP g] (LetE [ValD (VarP r)
            (NormalB (LamE [VarP x] (AppE (VarE g) (applistE fm (replicate (n-1) (mkNameE "Prelude.id") ++ map (VarE . h) [0..l-1] ++ [AppE (VarE f) (VarE x)])))))
            []] (newVarE (u1+3)))) []
        f = newFunc u1
        g = newFunc (u1+1)
        r = newFunc u2
        x = newVar (u1+2)
        h i = mkName ("hylo"++show i)
    return $ LetE (map go (zip3 [0..l-1] ns fms)) (TupE $ map (VarE . h) [0..l-1])

-- |
-- @autofoldMutual ts@ provides a folding function for the mutually recursive types @ts@
autofoldMutual ::
    [(TypeQ,TypeQ)] -- ^ @[(u0,t0), .., (un,tn)]@; @ui@ is an un-recursive datatype and @ti@ is a fixpoint of @ui@
 -> ExpQ -- ^ @(fold0, .., foldn)@; tuple of @fold@
autofoldMutual uts = do
    TupE os <- autooutMutual uts
    LetE dhs _ <- autohyloMutual (map fst uts)
    return $ TupE (map (\(o,ValD _ (NormalB h) _) -> AppE h o) (zip os dhs))

autofoldtypeMutual :: [(TypeQ,TypeQ)] -> Q [Type]
autofoldtypeMutual uts = do
    ntxs <- mapM (\(_,t) -> t >>= type2typex [] [] >>= applyFixed 0) uts
    let l = length ntxs
        ns = map fst ntxs
        txs = map snd ntxs
    uxs <- mapM (\(n,(u,_)) -> u >>= type2typex [] [] >>= applyFixed' n 0) (zip ns uts)
    let go (k,n,ux,tx) = do
            uxa <- applistTx ux (map a [0..l-1])
            t <- typex2type (ArrowTx (ArrowTx uxa (a k)) (ArrowTx tx (a k)))
            return $ ForallT (map (\i -> PlainTV $ mkName ("t"++show i)) [0..n-1] ++ map a' [0..l-1]) [] t
        a i = VarTx $ mkName ("a"++show i)
        a' i = PlainTV $ mkName ("a"++show i)
    mapM go (zip4 [0..l-1] ns uxs txs)

autofolddecMutual :: [String] -> [(TypeQ,TypeQ)] -> DecsQ
autofolddecMutual = gendecs1 autofoldMutual autofoldtypeMutual

-- |
-- @autounfoldMutual ts@ provides an unfolding function for the mutually recursive types @ts@.
autounfoldMutual ::
    [(TypeQ,TypeQ)] -- ^ @[(u0,t0), .., (un,tn)]@; @ui@ is an un-recursive datatype and @ti@ is a fixpoint of @ui@
 -> ExpQ -- ^ @(unfold0, .., unfoldn)@; tuple of @unfold@
autounfoldMutual uts = do
    TupE is <- autoinMutual uts
    TupE hs <- autohyloMutual (map fst uts)
    u1 <- unique
    return $ TupE (map (\(i,h) -> LamE [newVarP u1] (AppE (AppE h (newVarE u1)) i)) (zip is hs))

autounfoldtypeMutual :: [(TypeQ,TypeQ)] -> Q [Type]
autounfoldtypeMutual uts = do
    ntxs <- mapM (\(_,t) -> t >>= type2typex [] [] >>= applyFixed 0) uts
    let l = length ntxs
        ns = map fst ntxs
        txs = map snd ntxs
    uxs <- mapM (\(n,(u,_)) -> u >>= type2typex [] [] >>= applyFixed' n 0) (zip ns uts)
    let go (k,n,ux,tx) = do
            uxa <- applistTx ux (map a [0..l-1])
            t <- typex2type (ArrowTx (ArrowTx (a k) uxa) (ArrowTx (a k) tx))
            return $ ForallT (map (\i -> PlainTV $ mkName ("t"++show i)) [0..n-1] ++ map a' [0..l-1]) [] t
        a i = VarTx $ mkName ("a"++show i)
        a' i = PlainTV $ mkName ("a"++show i)
    mapM go (zip4 [0..l-1] ns uxs txs)

autounfolddecMutual :: [String] -> [(TypeQ,TypeQ)] -> DecsQ
autounfolddecMutual = gendecs1 autounfoldMutual autounfoldtypeMutual

