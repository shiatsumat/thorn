{-# LANGUAGE TemplateHaskell #-}

-- |
-- The module Data.Thorn.Zipper.
module Data.Thorn.Zipper (
    Hop(..)
  , zipperdata
  ) where

import Data.Thorn.Internal
import Language.Haskell.TH

data Hop =
    MemberH Name Int
  | ListH Int
  | TupleH Int

zipperdata :: String -> TypeQ -> [Name] -> DecsQ
zipperdata zps t ds = do
    (n, tx@(DataTx nm _ _)) <- applyFixed 0 =<< type2typex [] [] =<< t
    cns <- mapM cx2cn (derivedata (nameBase nm) nm id tx)
    let tvs = map (\i -> PlainTV (mkName ("t" ++ show i))) [0..n-1]
        apptvs t' = applistT t' (map (\i -> VarT (mkName ("t" ++ show i))) [0..n-1])
        threadname = mkName (nameBase nm ++ "Thread")
        threaddec = DataD [] threadname tvs cns ds
        zipperdec = TySynD (mkName zps) tvs (applistT (TupleT 2) [apptvs (ConT nm), AppT ListT (apptvs (ConT threadname))])
    runIO (putStrLn (pprint [threaddec,zipperdec]))
    return [threaddec,zipperdec]
    where cx2cn :: Conx -> Q Con
          cx2cn (nm,txs) = do
              sts <- mapM (\tx' -> typex2type tx' >>= return . (,) NotStrict) txs
              return (NormalC nm sts)

derivedata :: String -> Name -> (Typex -> Typex) -> Typex -> [Conx]
derivedata _ _ _ (VarTx _) = []
derivedata _ _ _ (BasicTx _) = []
derivedata _ _ _ (FixedTx _) = []
derivedata _ _ _ (SpecialTx _) = []
derivedata s x f (DataTx _ _ cxs) = concatMap go [0..n-1]
    where n = length cxs
          go i = concat (foldl (ho nm txs) [] [0..m-1])
            where (nm,txs) = cxs !! i
                  m = length txs
          ho nm txs cxss i
            | null cxs' = cxss
            | otherwise = cxss ++ [cxs']
            where cxs' = derivedata (s ++ "ThornZipper" ++ nameBase nm ++ show (length cxss)) x (\tx -> f (TupleTx (txsa ++ [tx] ++ txsb))) (txs !! i)
                  txsa = take i txs
                  txsb = drop (i+1) txs
derivedata s x f (SeenDataTx nm _)
    | nameBase x == nameBase nm = [(mkName s, [f (TupleTx [])])]
    | otherwise = []
derivedata s x f (TupleTx txs) = concat (foldl go [] [0..n-1])
    where n = length txs
          go cxss i
            | null cxs = cxss
            | otherwise = cxss ++ [cxs]
            where cxs = derivedata (s ++ "ThornZipperTuple" ++ show (length cxss)) x (\tx -> f(TupleTx (txsa ++ [tx] ++ txsb))) (txs !! i)
                  txsa = take i txs
                  txsb = drop (i+1) txs
derivedata _ _ _ (ArrowTx _ _) = []
derivedata s x f (ListTx tx) = derivedata (s ++ "ThornZipperList") x (\tx' -> f(TupleTx [ListTx tx, tx', ListTx tx])) tx
derivedata _ _ _ _ = error "derivedata : Thorn doesn't work well, sorry."

