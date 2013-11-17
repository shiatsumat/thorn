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
unfixdataEx (pretype,suftype) (predata,sufdata) (pretypeinfix,suftypeinfix) (predatainfix,sufdatainfix) t = do
    (n, DataTx nm _ cxs) <- applyFixed 0 =<< type2typex [] [] =<< t
    let modifytx (DataTx nm' vmp cxs') = if nm == nm' then VarTx $ mkName ("self") else DataTx nm' (map (\(nm'',tx) -> (nm'',modifytx tx)) vmp) (map modifycx cxs')
        modifytx tx@(SeenDataTx nm' _) = if nm == nm' then VarTx $ mkName ("self") else modifytx tx
        modifytx (TupleTx txs) = TupleTx (map modifytx txs)
        modifytx (ArrowTx txa txb) = ArrowTx (modifytx txa) (modifytx txb)
        modifytx (ListTx tx) = ListTx (modifytx tx)
        modifytx tx = tx
        modifycx (nm',txs) = (nm',map modifytx txs)
        go (nm',txs) = do
              ts <- map ((,) NotStrict) <$> mapM (typex2type . modifytx) txs
              return $ NormalC (datanm nm') ts
    cns <- mapM go cxs
    return [DataD [] (typenm nm) (map var [0..n-1] ++ [self]) cns []]
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
          self = PlainTV $ mkName ("self")

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
    (n,DataTx nm _ cxs) <- applyFixed 0 =<< type2typex [] [] =<< u
    f <- autofmap u
    u <- unique
    return $ LamE [newVarP u, newVarP (u+1)] (LetE [ValD (newVarP (u+3))
        (NormalB (LamE [newVarP (u+2)] (AppE (newVarE (u+1)) (applistE f (replicate (n-1) (mkNameE "Prelude.id") ++ [newVarE (u+3)] ++ [AppE (newVarE u) (newVarE (u+2))])))))
        []] (newVarE (u+3)))

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
-- @unfixdataMutual ts@ is a mutual recursion version of @unfixdata t@.
unfixdataMutual :: [TypeQ] -> DecsQ
unfixdataMutual = unfixdataMutualEx ("Uf","") ("Uf","") ("&","") ("&","")

unfixdataMutualEx ::
    (String,String) -- ^ prefix and suffix of type constructor
 -> (String,String) -- ^ prefix and suffix of data constructor
 -> (String,String) -- ^ prefix and suffix of infix type constructor
 -> (String,String) -- ^ prefix and suffix of infix data constructor
 -> [TypeQ] -- ^ data types
 -> DecsQ -- ^ declarations of data
unfixdataMutualEx = undefined

autoinMutual :: [TypeQ] -> DecsQ
autoinMutual ts = fail "oh"

autooutMutual :: [TypeQ] -> DecsQ
autooutMutual ts = fail "oh"

autohyloMutual :: [TypeQ] -> DecsQ
autohyloMutual ts = fail "oh"

-- |
-- @autofoldMutual ts@ provides a folding function for the mutually recursive types @ts@.
autofoldMutual :: [TypeQ] -> ExpQ
autofoldMutual ts = do fail "oh"

-- |
-- @autounfoldMutual ts@ provides an unfolding function for the mutually recursive types @ts@.
autounfoldMutual :: [TypeQ] -> ExpQ
autounfoldMutual ts = do fail "oh"

