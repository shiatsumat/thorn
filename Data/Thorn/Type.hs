{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}

-- | The module Data.Thorn.Type.
module Data.Thorn.Type (
    Unique, unique
  , newVar, newSubvar, newFunc, newFmap
  , newVarP, newSubvarP, newFuncP, newFmapP
  , newVarE, newSubvarE, newFuncE, newFmapE
  , mkNameE, mkNameCE, mkNameP, mkNameTx
  , applistE, applistT, applistTx, appTx
  , Typex(..)
  , Conx
  , cxtxs
  , type2typex, typex2type, normalizetype
  , T0, T1, T2, T3, T4, T5, T6, T7, T8, T9
  , applySpecial, applyFixed, applyFixed'
  , gendec1, gendec2, gendecs1
  ) where

import Language.Haskell.TH
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import System.Random

instance MonadIO Q where
    liftIO = runIO

type Unique = Int

unique :: MonadIO m => m Unique
unique = liftIO $ getStdRandom (randomR (0,1000000000))

newVar, newSubvar, newFunc, newFmap :: Int -> Name
newVarP, newSubvarP, newFuncP, newFmapP :: Int -> Pat
newVarE, newSubvarE, newFuncE, newFmapE :: Int -> Exp
newVar n = mkName $ "var" ++ show n
newVarP = VarP . newVar
newVarE = VarE . newVar
newSubvar n = mkName $ "subvar" ++ show n
newSubvarP = VarP . newSubvar
newSubvarE = VarE . newSubvar
newFunc n = mkName $ "func" ++ show n
newFuncP = VarP . newFunc
newFuncE = VarE . newFunc
newFmap n = mkName $ "fmap" ++ show n
newFmapP = VarP . newFmap
newFmapE = VarE . newFmap

mkNameE, mkNameCE :: String -> Exp
mkNameP :: String -> Pat
mkNameTx :: String -> Typex
mkNameE = VarE . mkName
mkNameCE = ConE . mkName
mkNameP = VarP . mkName
mkNameTx = VarTx . mkName

applistE :: Exp -> [Exp] -> Exp
applistT :: Type -> [Type] -> Type
applistTx :: Typex -> [Typex] -> TypexQ
applistE e es = foldl (\e1 e2 -> AppE e1 e2) e es
applistT t ts = foldl (\t1 t2 -> AppT t1 t2) t ts
applistTx tx txs = foldM (\tx1 tx2 -> appTx tx1 tx2) tx txs

appTx :: Typex -> Typex -> TypexQ
appTx (FuncTx f) tx = f tx
appTx _ _ = fail "appTx : Thorn doesn't work well, sorry."

data Typex =
    VarTx Name
  | BasicTx Name
  | FixedTx Int
  | SpecialTx Int
  | NotTx
  | FuncTx (Typex -> TypexQ)
  | DataTx Name VarMap [Conx]
  | SeenDataTx Name VarMap
  | TupleTx [Typex]
  | ArrowTx Typex Typex
  | ListTx Typex
type TypexQ = Q Typex

type Conx = (Name,[Typex])

cxtxs :: Conx -> [Typex]
cxtxs = snd

type VarMap = [(Name,Typex)]
type Datas = [(Name,VarMap)]

instance Eq Typex where
    VarTx t == VarTx t' = t==t'
    BasicTx nm == BasicTx nm' = nm==nm'
    SpecialTx n == SpecialTx n' = n==n'
    FixedTx n == FixedTx n' = n==n'
    NotTx == NotTx = True
    DataTx nm vmp cons == DataTx nm' vmp' cons' = nm==nm'&&vmp==vmp'&&cons==cons'
    SeenDataTx nm vmp == SeenDataTx nm' vmp' = nm==nm'&&vmp==vmp'
    TupleTx txs == TupleTx txs' = txs==txs'
    ArrowTx txa txb == ArrowTx txa' txb' = txa==txa'&&txb==txb'
    ListTx tx == ListTx tx' = tx==tx'
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
          go (TyConI (DataD _ nm' tvs cons _)) = do
              b <- istypevariant nm'
              if b then tofixed nm' else ho (length tvs) []
            where ho 0 txs = fromData nm' (zip (map nameTV tvs) (reverse txs)) dts cons
                  ho n txs = return $ FuncTx $ \tx -> ho (n-1) (tx:txs)
          go (TyConI (NewtypeD _ _ tvs con _)) = ho (length tvs) []
            where ho 0 txs = fromData nm (zip (map nameTV tvs) (reverse txs)) dts [con]
                  ho n txs = return $ FuncTx $ \tx -> ho (n-1) (tx:txs)
          go (PrimTyConI _ _ _) = fail "type2typex : Thorn doesn't support such primitive types, sorry."
          go (FamilyI _ _) = fail "type2typex : Thorn doesn't support type families, sorry."
          go _ = fail "type2typex : Thorn doesn't work well, sorry."
type2typex _ _ (TupleT n) = go n []
    where go 0 txs = return $ TupleTx (reverse txs)
          go k txs = return $ FuncTx $ \tx -> go (k-1) (tx:txs)
type2typex _ _ ArrowT = return $ FuncTx $ \txa -> return $ FuncTx $ \txb -> return $ ArrowTx txa txb
type2typex _ _ ListT = return $ FuncTx $ \tx -> return $ ListTx tx
type2typex _ _ _ = fail "type2typex : Thorn doesn't support such types, sorry."

fromData :: Name -> VarMap -> Datas -> [Con] -> TypexQ
fromData nm vmp dts cons = case find (\(nm',_)->nm==nm') dts of
        Just (_,vmp')
            | vmp == vmp' -> return $ SeenDataTx nm vmp
            | otherwise -> fail "fromData : Thorn doesn't support irregular types, sorry."
        Nothing -> DataTx nm vmp <$> mapM con2conx cons
    where dts' = (nm,vmp) : dts
          con2conx (NormalC nm' sts) = (,) nm' <$> mapM stype2typex sts
          con2conx (RecC nm' vsts) = (,) nm' <$> mapM vstype2typex vsts
          con2conx (InfixC sta nm' stb) = do
              txa <- stype2typex sta
              txb <- stype2typex stb
              return (nm',[txa,txb])
          con2conx (ForallC _ _ _) = fail "fromData : Thorn doesn't support existential types, sorry."
          stype2typex (_,t) = type2typex vmp dts' t
          vstype2typex (_,_,t) = type2typex vmp dts' t

nameTV :: TyVarBndr -> Name
nameTV (PlainTV nm) = nm
nameTV (KindedTV nm _) = nm

typex2type :: Typex -> TypeQ
typex2type (VarTx nm) = return $ VarT nm
typex2type (SpecialTx _) = return StarT
typex2type (FixedTx n) = return $ VarT (mkName $ "t" ++ show n)
typex2type NotTx = return StarT
typex2type (FuncTx f) = do
    AppT t StarT <- typex2type =<< f NotTx
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
typevariants = mapM (\n -> getnm <$> (reify . mkName $ 'T' : show n)) ([0..9] :: [Int])
    where getnm (TyConI (DataD _ nm _ _ _)) = nm
          getnm _ = error "typevariants : Thorn doesn't work well, sorry."

istypevariant :: Name -> Q Bool
istypevariant nm = do
    typevariants' <- typevariants
    return $ elem nm typevariants'

tofixed :: Name -> Q Typex
tofixed nm = do
    typevariants' <- typevariants
    return $ FixedTx (fromJust $ elemIndex nm typevariants')

applySpecial :: Int -> Typex -> Q (Int,Typex)
applySpecial n (FuncTx f) = f (SpecialTx n) >>= applySpecial (n+1)
applySpecial n tx@(VarTx _) = return (n,tx)
applySpecial n tx@(BasicTx _) = return (n,tx)
applySpecial n tx@(FixedTx _) = return (n,tx)
applySpecial n tx@(SpecialTx _) = return (n,tx)
applySpecial n tx@NotTx = return (n,tx)
applySpecial n tx@(DataTx _ _ _) = return (n,tx)
applySpecial n tx@(SeenDataTx _ _) = return (n,tx)
applySpecial n tx@(TupleTx _) = return (n,tx)
applySpecial n tx@(ArrowTx _ _) = return (n,tx)
applySpecial n tx@(ListTx _) = return (n,tx)

applyFixed :: Int -> Typex -> Q (Int,Typex)
applyFixed n (FuncTx f) = f (FixedTx n) >>= applyFixed (n+1)
applyFixed n tx@(VarTx _) = return (n,tx)
applyFixed n tx@(BasicTx _) = return (n,tx)
applyFixed n tx@(FixedTx _) = return (n,tx)
applyFixed n tx@(SpecialTx _) = return (n,tx)
applyFixed n tx@NotTx = return (n,tx)
applyFixed n tx@(DataTx _ _ _) = return (n,tx)
applyFixed n tx@(SeenDataTx _ _) = return (n,tx)
applyFixed n tx@(TupleTx _) = return (n,tx)
applyFixed n tx@(ArrowTx _ _) = return (n,tx)
applyFixed n tx@(ListTx _) = return (n,tx)

applyFixed' :: Int -> Int -> Typex -> TypexQ
applyFixed' k n tx@(FuncTx f)
    | k==n = return tx
    | otherwise = f (FixedTx n) >>= applyFixed' k (n+1)
applyFixed' _ _ _ = fail "applyFixed' : Thorn doesn't work well, sorry."

gendec1 :: (a -> ExpQ) -> (a -> TypeQ) -> String -> a -> DecsQ
gendec1 f g s a = do
    e <- f a
    t <- g a
    return [SigD (mkName s) t, ValD (mkNameP s) (NormalB e) []]

gendec2 :: (a -> b -> ExpQ) -> (a -> b -> TypeQ) -> String -> a -> b -> DecsQ
gendec2 f g s a b = do
    e <- f a b
    t <- g a b
    return [SigD (mkName s) t, ValD (mkNameP s) (NormalB e) []]

gendecs1 :: (a -> ExpQ) -> (a -> Q [Type]) -> [String] -> a -> DecsQ
gendecs1 f g ss a = do
    TupE es <- f a
    ts <- g a
    return $ concatMap (\(s,e,t) -> [SigD (mkName s) t, ValD (mkNameP s) (NormalB e) []]) (zip3 ss es ts)

