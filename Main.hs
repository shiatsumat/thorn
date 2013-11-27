{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Main (module Main) where

import Data.Thorn

data Rose x = Rose x [Rose x] deriving Show

zipperdata "ZipperRose" "jumprose" [t|Rose|] [''Show]

main = putStr ""

