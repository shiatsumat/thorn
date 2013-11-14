{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module Data.Thorn.Internal (
    newVar, newVarP, newVarE
  , newFunc, newFuncP, newFuncE
  , newFmap, newFmapP, newFmapE
  , mkNameE, mkNameCE, mkNameP
  , Typex(..)
  , Conx(..)
  , cxtxs
  , normalizeType
  , apps
  ) where

import Language.Haskell.TH
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

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

mkNameE = VarE . mkName
mkNameCE = ConE . mkName
mkNameP = VarP . mkName

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
    _ == _ = False

instance Show Typex where
    show (DataTx _ _ _) = "DataTx"
    show (SeenDataTx _ _) = "SeenDataTx"
    show _ = "Foo"

normalizeType :: VarMap -> Datas -> Type -> TypexQ
normalizeType vmp dts (ForallT tvs _ t) = normalizeType vmp' dts t
    where vmp' = filter (\(nm,_) -> notElem nm (map nameTV tvs)) vmp
normalizeType vmp dts (AppT t u) = do
    FuncTx f <- normalizeType vmp dts t
    ux <- normalizeType vmp dts u
    f ux
normalizeType vmp dts (SigT t _) = normalizeType vmp dts t
normalizeType vmp dts (VarT nm) = case (find (\(nm',_) -> nm==nm') vmp) of
    Nothing -> return $ VarTx nm
    Just (_,tx) -> return tx
normalizeType vmp dts (ConT nm)
    | s == "()" = normalizeType vmp dts (TupleT 0)
    | head s == '(' && dropWhile (==',') (tail s) == ")" = normalizeType vmp dts (TupleT (length s - 1))
    | s == "(->)" = normalizeType vmp dts ArrowT
    | s == "[]" = normalizeType vmp dts ListT
    | elem s ["Int","Word","Float","Double","Char","Ptr","FunPtr"] = return $ BasicTx nm
    | otherwise = reify nm >>= go
    where s = nameBase nm
          go (TyConI (TySynD _ tvs u)) = ho (length tvs) []
            where ho 0 txs = normalizeType (zip (map nameTV tvs) (reverse txs)) dts u
                  ho n txs = return $ FuncTx $ \tx -> ho (n-1) (tx:txs)
          go (TyConI (DataD _ _ tvs cons _)) = ho (length tvs) []
            where ho 0 txs = fromData nm (zip (map nameTV tvs) (reverse txs)) dts cons
                  ho n txs = return $ FuncTx $ \tx -> ho (n-1) (tx:txs)
          go (TyConI (NewtypeD _ _ tvs con _)) = ho (length tvs) []
            where ho 0 txs = fromData nm (zip (map nameTV tvs) (reverse txs)) dts [con]
                  ho n txs = return $ FuncTx $ \tx -> ho (n-1) (tx:txs)
          go (PrimTyConI _ _ _) = fail "Autofmap doesn't support such primitive types, sorry."
          go (FamilyI _ _) = fail "Autofmap doesn't support type families, sorry."
normalizeType vmp dts (TupleT n) = go n []
    where go 0 txs = return $ TupleTx (reverse txs)
          go n txs = return $ FuncTx $ \tx -> go (n-1) (tx:txs)
normalizeType vmp dts ArrowT = return $ FuncTx $ \txa -> return $ FuncTx $ \txb -> return $ ArrowTx txa txb
normalizeType vmp dts ListT = return $ FuncTx $ \tx -> return $ ListTx tx
normalizeType _ _ _ = fail "Autofmap doesn't support such types, sorry."

fromData :: Name -> VarMap -> Datas -> [Con] -> TypexQ
fromData nm vmp dts cons = case find (\(nm',vmp')->nm==nm') dts of
        Just (_,vmp')
            | vmp == vmp' -> return $ SeenDataTx nm vmp
            | otherwise -> fail "Autofmap doesn't support irregular types, sorry."
        Nothing -> DataTx nm vmp <$> mapM normalizeCon cons
    where dts' = (nm,vmp) : dts
          normalizeCon (NormalC nm sts) = NormalCx nm <$> mapM normalizeStrictType sts
          normalizeCon (RecC nm vsts) = NormalCx nm <$> mapM normalizeVarStrictType vsts
          normalizeCon (InfixC sta nm stb) = InfixCx nm <$> normalizeStrictType sta <*> normalizeStrictType stb
          normalizeStrictType (_,t) = normalizeType vmp dts' t
          normalizeVarStrictType (_,_,t) = normalizeType vmp dts' t

nameTV :: TyVarBndr -> Name
nameTV (PlainTV nm) = nm
nameTV (KindedTV nm _) = nm

apps e es = foldl (\e es -> AppE e es) e es

