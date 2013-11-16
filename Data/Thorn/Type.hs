{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}

-- | The module Data.Thorn.Type.
module Data.Thorn.Type (
    mkNameE, mkNameCE, mkNameP
  , applistE, applistT
  , Typex(..)
  , Conx(..)
  , cxtxs
  , type2typex, typex2type, normalizetype
  , T0, T1, T2, T3, T4, T5, T6, T7, T8, T9
  ) where

import Language.Haskell.TH
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

mkNameE = VarE . mkName
mkNameCE = ConE . mkName
mkNameP = VarP . mkName

applistE e es = foldl (\e es -> AppE e es) e es
applistT t ts = foldl (\t ts -> AppT t ts) t ts

data Typex =
    VarTx Name
  | FuncTx (Typex -> TypexQ)
  | DataTx Name VarMap [Conx]
  | SeenDataTx Name VarMap
  | BasicTx Name
  | TupleTx [Typex]
  | ArrowTx Typex Typex
  | ListTx Typex
  | SpecialTx Int
  | FixedTx Int
type TypexQ = Q Typex

data Conx =
    NormalCx Name [Typex]
  | InfixCx Name Typex Typex
  deriving Eq

cxtxs :: Conx -> [Typex]
cxtxs (NormalCx _ txs) = txs
cxtxs (InfixCx _ txa txb) = [txa,txb]

type VarMap = [(Name,Typex)]
type Datas = [(Name,VarMap)]

instance Eq Typex where
    VarTx t == VarTx t' = t==t'
    DataTx nm vmp cons == DataTx nm' vmp' cons' = nm==nm'&&vmp==vmp'&&cons==cons'
    SeenDataTx nm vmp == SeenDataTx nm' vmp' = nm==nm'&&vmp==vmp'
    BasicTx nm == BasicTx nm' = nm==nm'
    TupleTx txs == TupleTx txs' = txs==txs'
    ArrowTx txa txb == ArrowTx txa' txb' = txa==txa'&&txb==txb'
    ListTx tx == ListTx tx' = tx==tx'
    SpecialTx n == SpecialTx n' = n==n'
    FixedTx n == FixedTx n' = n==n'
    _ == _ = False

instance Show Typex where
    show (DataTx _ _ _) = "DataTx"
    show (SeenDataTx _ _) = "SeenDataTx"
    show _ = "Foo"

type2typex :: VarMap -> Datas -> Type -> TypexQ
type2typex vmp dts (ForallT tvs _ t) = type2typex vmp' dts t
    where vmp' = filter (\(nm,_) -> notElem nm (map nameTV tvs)) vmp
type2typex vmp dts (AppT t u) = do
    FuncTx f <- type2typex vmp dts t
    ux <- type2typex vmp dts u
    f ux
type2typex vmp dts (SigT t _) = type2typex vmp dts t
type2typex vmp _ (VarT nm) = case (find (\(nm',_) -> nm==nm') vmp) of
    Nothing -> return $ VarTx nm
    Just (_,tx) -> return tx
type2typex vmp dts (ConT nm)
    | s == "()" = type2typex vmp dts (TupleT 0)
    | head s == '(' && dropWhile (==',') (tail s) == ")" = type2typex vmp dts (TupleT (length s - 1))
    | s == "(->)" = type2typex vmp dts ArrowT
    | s == "[]" = type2typex vmp dts ListT
    | elem s ["Int","Word","Float","Double","Char","Ptr","FunPtr"] = return $ BasicTx nm
    | otherwise = reify nm >>= go
    where s = nameBase nm
          go (TyConI (TySynD _ tvs u)) = ho (length tvs) []
            where ho 0 txs = type2typex (zip (map nameTV tvs) (reverse txs)) dts u
                  ho n txs = return $ FuncTx $ \tx -> ho (n-1) (tx:txs)
          go (TyConI (DataD _ nm tvs cons _)) = do
              b <- istypevariant nm
              if b then tofixed nm else ho (length tvs) []
            where ho 0 txs = fromData nm (zip (map nameTV tvs) (reverse txs)) dts cons
                  ho n txs = return $ FuncTx $ \tx -> ho (n-1) (tx:txs)
          go (TyConI (NewtypeD _ _ tvs con _)) = ho (length tvs) []
            where ho 0 txs = fromData nm (zip (map nameTV tvs) (reverse txs)) dts [con]
                  ho n txs = return $ FuncTx $ \tx -> ho (n-1) (tx:txs)
          go (PrimTyConI _ _ _) = fail "Thorn doesn't support such primitive types, sorry."
          go (FamilyI _ _) = fail "Thorn doesn't support type families, sorry."
          go _ = fail "Thorn doesn't work well, sorry."
type2typex _ _ (TupleT n) = go n []
    where go 0 txs = return $ TupleTx (reverse txs)
          go n txs = return $ FuncTx $ \tx -> go (n-1) (tx:txs)
type2typex _ _ ArrowT = return $ FuncTx $ \txa -> return $ FuncTx $ \txb -> return $ ArrowTx txa txb
type2typex _ _ ListT = return $ FuncTx $ \tx -> return $ ListTx tx
type2typex _ _ _ = fail "Thorn doesn't support such types, sorry."

fromData :: Name -> VarMap -> Datas -> [Con] -> TypexQ
fromData nm vmp dts cons = case find (\(nm',vmp')->nm==nm') dts of
        Just (_,vmp')
            | vmp == vmp' -> return $ SeenDataTx nm vmp
            | otherwise -> fail "Thorn doesn't support irregular types, sorry."
        Nothing -> DataTx nm vmp <$> mapM con2conx cons
    where dts' = (nm,vmp) : dts
          con2conx (NormalC nm sts) = NormalCx nm <$> mapM stype2typex sts
          con2conx (RecC nm vsts) = NormalCx nm <$> mapM vstype2typex vsts
          con2conx (InfixC sta nm stb) = InfixCx nm <$> stype2typex sta <*> stype2typex stb
          con2conx (ForallC _ _ _) = fail "Thorn doesn't support existential types, sorry."
          stype2typex (_,t) = type2typex vmp dts' t
          vstype2typex (_,_,t) = type2typex vmp dts' t

nameTV :: TyVarBndr -> Name
nameTV (PlainTV nm) = nm
nameTV (KindedTV nm _) = nm

typex2type :: Typex -> TypeQ
typex2type (VarTx nm) = return $ VarT nm
typex2type (FuncTx f) = do
    AppT t _ <- typex2type =<< f (SpecialTx 0)
    return t
typex2type (DataTx nm vmp _) = do
    ts <- mapM (typex2type . snd) vmp
    return $ applistT (ConT nm) ts
typex2type (SeenDataTx nm vmp) = do
    ts <- mapM (typex2type . snd) vmp
    return $ applistT (ConT nm) ts
typex2type (BasicTx nm) = return $ ConT nm
typex2type (TupleTx txs) = do
    ts <- mapM typex2type txs
    return $ applistT (TupleT (length txs)) ts
typex2type (ArrowTx txa txb) = do
    ta <- typex2type txa
    tb <- typex2type txb
    return $ applistT ArrowT [ta,tb]
typex2type (ListTx tx) = do
    t <- typex2type tx
    return $ AppT ListT t
typex2type (SpecialTx _) = return StarT
typex2type (FixedTx n) = return $ VarT (mkName $ "thorntypevariant" ++ show n)

normalizetype :: Type -> TypeQ
normalizetype t = typex2type =<< type2typex [] [] t

data T0
data T1
data T2
data T3
data T4
data T5
data T6
data T7
data T8
data T9

typevariants :: Q [Name]
typevariants = mapM (\n -> getnm <$> (reify . mkName $ 'T' : show n)) [0..9]
    where getnm (TyConI (DataD _ nm _ _ _)) = nm
          getnm _ = error "Thorn doesn't work well, sorry."

istypevariant :: Name -> Q Bool
istypevariant nm = do
    typevariants' <- typevariants
    return $ elem nm typevariants'

tofixed :: Name -> Q Typex
tofixed nm = do
    typevariants' <- typevariants
    return $ FixedTx (fromJust $ elemIndex nm typevariants')
