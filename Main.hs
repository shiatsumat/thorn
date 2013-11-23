{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Main (module Main) where

import Data.Thorn

data Rose x = x :-< (Forest x) deriving Show
data Forest x = F [Rose x] deriving Show

unfixdataMutual [([t|Rose|],"UfRose",modifynameUf,[''Show]), ([t|Forest|],"UfForest",modifynameUf,[''Show])]
-- data UfRose x rose forest = x :&-< forest deriving Show
-- data UfForest x rose forest = UfF [rose] deriving Show

autofolddecMutual "foldrose" [([t|UfRose|],[t|Rose|]),([t|UfForest|],[t|Forest|])] 0
-- foldrose :: (UfRose x a b -> a) -> (UfForest x a b -> b) -> Rose x -> a
-- foldrose = ...
autounfolddecMutual "unfoldrose" [([t|UfRose|],[t|Rose|]),([t|UfForest|],[t|Forest|])] 0
-- unfoldrose :: (a -> UfRose x a b) -> (b -> UfForest x a b) -> Rose x -> a
-- unfoldrose = ...

rose :: Rose Int
rose = unfoldrose gorose goforest 0 -- 0 :-< F [1 :-< F [3 :-< F [],4 :-< F []],2 :-< F [5 :-< F [],6 :-< F []]]
    where gorose :: Int -> UfRose Int Int Int
          gorose n
            | n > 2 = n :&-< (-1)
            | otherwise = n :&-< n
          goforest :: Int -> UfForest Int Int Int
          goforest (-1) = UfF []
          goforest n = UfF [n*2+1,n*2+2]

showrose :: Show x => Rose x -> String
showrose = unlines . foldrose gorose goforest
    where gorose :: Show x => UfRose x [String] [String] -> [String]
          gorose (x :&-< ls) = [show x] ++ ls
          goforest :: UfForest x [String] [String] -> [String]
          goforest (UfF []) = []
          goforest (UfF lss) = concatMap hang (init lss) ++ hang' (last lss)
          hang ls = ["|"] ++ ["+--" ++ head ls] ++ map ("|  "++) (tail ls)
          hang' ls = ["|"] ++ ["+--" ++ head ls] ++ map ("   "++) (tail ls)

shownrose :: String
shownrose = showrose rose
-- 0
-- |
-- +--1
-- |  |
-- |  +--3
-- |  |
-- |  +--4
-- |
-- +--2
--    |
--    +--5
--    |
--    +--6

main = putStr shownrose

