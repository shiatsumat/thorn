{-# LANGUAGE TemplateHaskell #-}

-- |
-- The module Data.Thorn.Zipper.
module Data.Thorn.Zipper (
    zipperdata
  , StepIn(..), Jump(..)
  ) where

import Data.Char
import Control.Applicative
import Language.Haskell.TH
import Data.Thorn.Internal

data StepIn =
    MemberI String Int
  | TupleI Int
  | ListI

data Jump =
    JumpIn [StepIn]
  | JumpOut
  | JumpLeft
  | JumpRight

indexed :: [a] -> [(a,Int)]
indexed as = zip as [0..length as-1]

encoded :: String -> String
encoded s = concatMap go s
    where go c
            | isSymbol c = "Symbol" ++ show (ord c)
            | otherwise = [c]

zipperdata :: String -> String -> TypeQ -> [Name] -> DecsQ
zipperdata zippername jumpname t ds = do
    (tvn, basetx@(DataTx basenm _ _)) <- applyFixed 0 =<< type2typex [] [] =<< t
    let cys = derivedata basenm [] id basetx
        cyl = length cys
        tvs = map (\i -> PlainTV (mkName ("t" ++ show i))) [0..tvn-1]
        alltvs = ForallT tvs []
        apptvs t' = applistT t' (map (\i -> VarT (mkName ("t" ++ show i))) [0..tvn-1])
        threadname = encoded zippername ++ "Thread"
        dataname i = threadname ++ show i
        cy2cn ((tx,_),i) = do
            st <- typex2type tx >>= return . (,) NotStrict
            return (NormalC (mkName (dataname i)) [st])
    cns <- mapM cy2cn (indexed cys)
    let threaddec = DataD [] (mkName threadname) tvs cns ds
        zipperdec = TySynD (mkName zippername) tvs (applistT (TupleT 2) [apptvs (ConT basenm), AppT ListT (apptvs (ConT (mkName threadname)))])
    let stepinsname = mkName "stepins"
        rcname = mkName "recursive"
        thname = mkName "thread"
        infuncmatch i = do
            ea <- infos2expA infos
            eb <- infos2expB infos
            return $ Match (ListP (map info2pat infos)) (NormalB (TupE [AppE eb (VarE rcname), applistE (ConE (mkName ":")) [AppE (ConE bignm) (AppE ea (VarE rcname)), VarE thname]])) []
            where (_,infos) = cys !! i
                  bignm = mkName (threadname ++ show i)
                  info2pat :: StepInfo -> Pat
                  info2pat (DataInfo nm _ k _) = ConP (mkName "Data.Thorn.MemberI") [LitP (StringL (nameBase nm)), LitP (IntegerL (toInteger k))]
                  info2pat (TupleInfo _ k _) = ConP (mkName "Data.Thorn.TupleI") [LitP (IntegerL (toInteger k))]
                  info2pat ListInfo = ConP (mkName "Data.Thorn.ListI") []
                  infos2expA :: [StepInfo] -> ExpQ
                  infos2expA (DataInfo nm n _ j : rest) = do
                      u <- return 100
                      let govar k
                                | k-u==j = AppE <$> infos2expA rest <*> return (newVarE k)
                                | otherwise = return $ newVarE k
                      vs <- mapM govar [u..u+n-1]
                      return $ LamE [ConP nm (map newVarP [u..u+n-1])] (TupE vs)
                  infos2expA (TupleInfo n _ j : rest) = do
                      u <- return 0
                      let govar k
                                | k-u==j = AppE <$> infos2expA rest <*> return (newVarE k)
                                | otherwise = return $ newVarE k
                      vs <- mapM govar [u..u+n-1]
                      return $ LamE [TupP (map newVarP [u..u+n-1])] (TupE vs)
                  infos2expA (ListInfo : rest) = do
                      u <- return 0
                      e <- infos2expA rest
                      return $ LamE [newVarP u] (TupE [ListE [], AppE e (AppE (mkNameE "Prelude.head") (newVarE u)), AppE (mkNameE "Prelude.tail") (newVarE u)])
                  infos2expA [] = return $ AppE (mkNameE "Prelude.const") (TupE [])
                  infos2expB :: [StepInfo] -> ExpQ
                  infos2expB (DataInfo nm n _ j : rest) = do
                      u <- return 100
                      e <- infos2expB rest
                      return $ LamE [ConP nm (replicate j WildP ++ [newVarP u] ++ replicate (n-j-1) WildP)] (AppE e (newVarE u))
                  infos2expB (TupleInfo n _ j : rest) = do
                      u <- return 0
                      e <- infos2expB rest
                      return $ LamE [TupP (replicate j WildP ++ [newVarP u] ++ replicate (n-j-1) WildP)] (AppE e (newVarE u))
                  infos2expB (ListInfo : rest) = do
                      u <- return 0
                      e <- infos2expB rest
                      return $ LamE [newVarP u] (AppE e (AppE (mkNameE "Prelude.head") (newVarE u)))
                  infos2expB [] = return $ mkNameE "Prelude.id"
    infunc <- CaseE (VarE stepinsname) <$> (mapM infuncmatch [0..cyl-1])
    --outfunc <- undefined
    --leftfunc <- undefined
    --rightfunc <- undefined
    let inclause = Clause [ConP (mkName "Data.Thorn.JumpIn") [VarP stepinsname], TupP [VarP rcname,VarP thname]] (NormalB infunc) []
        --outclause = Clause [ConP (mkName "Data.Thorn.JumpOut") [], TupP [VarP rcname,VarP thname]] (NormalB outfunc) []
        --leftclause = Clause [ConP (mkName "Data.Thorn.JumpLeft") [], TupP [VarP rcname,VarP thname]] (NormalB leftfunc) []
        --rightclause = Clause [ConP (mkName "Data.Thorn.JumpRight") [], TupP [VarP rcname,VarP thname]] (NormalB rightfunc) []
        jumpdec = FunD (mkName jumpname) [inclause{-,outclause,leftclause,rightclause-}]
        jumpsig = SigD (mkName jumpname) (alltvs (applistT ArrowT [ConT (mkName "Data.Thorn.Jump"), applistT ArrowT (replicate 2 (apptvs (ConT (mkName zippername))))]))
    runIO (putStrLn (pprint [threaddec,zipperdec,jumpsig,jumpdec]))
    return [threaddec,zipperdec,jumpsig,jumpdec]

data StepInfo =
    DataInfo Name Int Int Int
  | TupleInfo Int Int Int
  | ListInfo
type ConY = (Typex, [StepInfo])

derivedata :: Name -> [StepInfo] -> (Typex -> Typex) -> Typex -> [ConY]
derivedata _ _ _ (VarTx _) = []
derivedata _ _ _ (BasicTx _) = []
derivedata _ _ _ (FixedTx _) = []
derivedata _ _ _ (SpecialTx _) = []
derivedata base info f (DataTx _ _ cxs) = concatMap go [0..n-1]
    where n = length cxs
          go i = concat (foldl ho [] [0..m-1])
            where (nm,txs) = cxs !! i
                  m = length txs
                  ho cxss j
                    | null cys = cxss
                    | otherwise = cxss ++ [cys]
                    where cys = derivedata base (info ++ [DataInfo nm m (length cxss) j]) (\tx -> f (TupleTx (txsa ++ [tx] ++ txsb))) (txs !! j)
                          txsa = take j txs
                          txsb = drop (j+1) txs
derivedata base info f (SeenDataTx nm _)
    | nameBase base == nameBase nm = [(f (TupleTx []), info)]
    | otherwise = []
derivedata base info f (TupleTx txs) = concat (foldl go [] [0..n-1])
    where n = length txs
          go cxss i
            | null cys = cxss
            | otherwise = cxss ++ [cys]
            where cys = derivedata base (info ++ [TupleInfo n (length cxss) i]) (\tx -> f (TupleTx (txsa ++ [tx] ++ txsb))) (txs !! i)
                  txsa = take i txs
                  txsb = drop (i+1) txs
derivedata _ _ _ (ArrowTx _ _) = []
derivedata base info f (ListTx tx) = derivedata base (info ++ [ListInfo]) (\tx' -> f (TupleTx [ListTx tx, tx', ListTx tx])) tx
derivedata _ _ _ _ = error "derivedata : Thorn doesn't work well, sorry."

