{-# LANGUAGE TemplateHaskell #-}

-- |
-- The module Data.Thorn.Fold.
module Data.Thorn.Fold (
    unfixdata, unfixdataEx
  , autoin, autoout, autohylo, autofold, autounfold
  , unfixdataMutual, unfixdataMutualEx
  , autoinMutual, autooutMutual, autohyloMutual, autofoldMutual, autounfoldMutual
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
    (_,DataTx _ _ cxsu) <- applyFixed 0 =<< type2typex [] [] =<< u
    (_,DataTx _ _ cxst) <- applyFixed 0 =<< type2typex [] [] =<< t
    u1 <- unique
    u2 <- unique
    let go ((nmu,txsu),(nmt,_)) = Match (ConP nmu (map newVarP [u2..u2+length txsu-1])) (NormalB (applistE (ConE nmt) (map newVarE [u2..u2+length txsu-1]))) []
    return $ LamE [newVarP u1] (CaseE (newVarE u1) (map go (zip cxsu cxst)))

autoout ::
    TypeQ -- ^ @u@, un-recursive datatype
 -> TypeQ -- ^ @t@, fixpoint of @u@
 -> ExpQ -- ^ function with a type @t x0 .. xn -> u x0 .. xn t@
autoout u t = do
    (_,DataTx _ _ cxsu) <- applyFixed 0 =<< type2typex [] [] =<< u
    (_,DataTx _ _ cxst) <- applyFixed 0 =<< type2typex [] [] =<< t
    u1 <- unique
    u2 <- unique
    let go ((nmu,txsu),(nmt,_)) = Match (ConP nmt (map newVarP [u2..u2+length txsu-1])) (NormalB (applistE (ConE nmu) (map newVarE [u2..u2+length txsu-1]))) []
    return $ LamE [newVarP u1] (CaseE (newVarE u1) (map go (zip cxsu cxst)))

autohylo ::
    TypeQ -- ^ @u@, un-recursive datatype
 -> ExpQ -- ^ function with a type @(a -> u x0 .. xn a) -> (u x0 .. xn b -> b) -> (a -> b)@
autohylo u = do
    (n,DataTx _ _ _) <- applyFixed 0 =<< type2typex [] [] =<< u
    f <- autofmap u
    u1 <- unique
    return $ LamE [newVarP u1, newVarP (u1+1)] (LetE [ValD (newVarP (u1+3))
        (NormalB (LamE [newVarP (u1+2)] (AppE (newVarE (u1+1)) (applistE f (replicate (n-1) (mkNameE "Prelude.id") ++ [newVarE (u1+3)] ++ [AppE (newVarE u1) (newVarE (u1+2))])))))
        []] (newVarE (u1+3)))

-- |
-- @autofold u t@ provides a folding function for a recursive type @t@.
autofold ::
    TypeQ -- ^ @u@, un-recursive datatype
 -> TypeQ -- ^ @t@, fixpoint of @u@
 -> ExpQ -- ^ function with a type @(u x0 .. xn a -> a) -> (t -> a)@
autofold u t = do
    o <- autoout u t
    h <- autohylo u
    return $ AppE h o

-- |
-- @autounfold t@ provides an unfolding function for the recursive type @t@.
autounfold ::
    TypeQ -- ^ @u@, un-recursive datatype
 -> TypeQ -- ^ @t@, fixpoint of @u@
 -> ExpQ -- ^ function with a type @(a -> u x0 .. xn a) -> (a -> t)@
autounfold u t = do
    i <- autoin u t
    h <- autohylo u
    u1 <- unique
    return $ LamE [newVarP u1] (AppE (AppE h (newVarE u1)) i)

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
 -> [TypeQ] -- ^ data types
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
 -> ExpQ -- ^ @(out0, .., outn)@; tuple of @in@.
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
 -> ExpQ -- ^ @(out0, .., outn)@; tuple of @out@.
autooutMutual uts = do
    cxsus <- mapM (\(u,_) -> u >>= type2typex [] [] >>= applyFixed 0 >>= return . getcxs . snd) uts
    cxsts <- mapM (\(_,t) -> t >>= type2typex [] [] >>= applyFixed 0 >>= return . getcxs . snd) uts
    u1 <- unique
    u2 <- unique
    let go ((nmu,txsu),(nmt,_)) = Match (ConP nmt (map newVarP [u2..u2+length txsu-1])) (NormalB (applistE (ConE nmu) (map newVarE [u2..u2+length txsu-1]))) []
        ho (cxsu,cxst) = LamE [newVarP u1] (CaseE (newVarE u1) (map go (zip cxsu cxst)))
    return $ TupE (map ho (zip cxsus cxsts))
    where getcxs (DataTx _ _ cxs) = cxs
          getcxs _ = error "Thorn doesn't work well, sorry."

-- |
-- Mutually recursive version of @autohylo@.
autohyloMutual ::
    [(TypeQ,TypeQ)] -- ^ @[(u0,t0), .., (un,tn)]@; @ui@ is an un-recursive datatype and @ti@ is a fixpoint of @ui@
 -> ExpQ -- ^ @(hylo0, .., hylon)@; tuple of @hylo@.
autohyloMutual _ = fail "oh"

-- |
-- @autofoldMutual ts@ provides a folding function for the mutually recursive types @ts@.
autofoldMutual ::
    [(TypeQ,TypeQ)] -- ^ @[(u0,t0), .., (un,tn)]@; @ui@ is an un-recursive datatype and @ti@ is a fixpoint of @ui@
 -> ExpQ -- ^ @(fold0, .., foldn)@; tuple of @fold@.
autofoldMutual ts = do fail "oh"

-- |
-- @autounfoldMutual ts@ provides an unfolding function for the mutually recursive types @ts@.
autounfoldMutual ::
    [(TypeQ,TypeQ)] -- ^ @[(u0,t0), .., (un,tn)]@; @ui@ is an un-recursive datatype and @ti@ is a fixpoint of @ui@
 -> ExpQ -- ^ @(unfold0, .., unfoldn)@; tuple of @unfold@.
autounfoldMutual ts = do fail "oh"

