{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Types where

import Data.Array
import Data.Array.Base (amap)
import Data.List.Split (chunksOf)
import Data.Ratio (approxRational)
import Data.Tuple (swap)

type Scalar = Rational

type Matrix = Array (Int, Int) Scalar

type Vector = Array (Int, Int) Scalar

checkSameSize :: Matrix -> Matrix -> Bool
checkSameSize a b = bounds a == bounds b

checkMultSize :: Matrix -> Matrix -> Bool
checkMultSize a b = (lj, uj) == (li', ui')
 where
  ((_, lj), (_, uj)) = bounds a
  ((li', _), (ui', _)) = bounds b

-- got some help from https://www.haskell.org/tutorial/arrays.html
genMatAdd :: (Scalar -> Scalar -> Scalar) -> Matrix -> Matrix -> Matrix
genMatAdd plus a b =
  if checkSameSize a b
    then
      array
        (bounds a)
        [ ((i, j), (a ! (i, j)) `plus` (b ! (i, j)))
        | i <- range (li, ui)
        , j <- range (lj, uj)
        ]
    else error "SizeCompareError (ui, uj) ((snd . bounds) b)"
 where
  ((li, lj), (ui, uj)) = bounds a

genMatMult :: (Scalar -> Scalar -> Scalar) -> Matrix -> Matrix -> Matrix
genMatMult star a b =
  if checkMultSize a b
    then
      accumArray
        (+)
        0
        resultBounds
        [ ((i, j), (a ! (i, k)) `star` (b ! (k, j)))
        | i <- range (li, ui)
        , j <- range (lj', uj')
        , k <- range (lj, uj)
        ]
    else error "MultSizeError ((snd . bounds) a) ((snd . bounds) b)"
 where
  ((li, lj), (ui, uj)) = bounds a
  ((_, lj'), (_, uj')) = bounds b
  resultBounds = ((li, lj'), (ui, uj'))

matAdd :: Matrix -> Matrix -> Matrix
matAdd = genMatAdd (+)

matMult :: Matrix -> Matrix -> Matrix
matMult = genMatMult (*)

scalMatMult :: Scalar -> Matrix -> Matrix
scalMatMult s = amap (* s)

matFromList :: (Int, Int) -> [Scalar] -> Matrix
matFromList b = listArray ((1, 1), b)

prettyMatrix :: Matrix -> String
prettyMatrix = concatMap ((' ' :) . show)

transpose :: Matrix -> Matrix
transpose m = ixmap newBounds (\(i, j) -> (j, i)) m
 where
  newBounds = (fst (bounds m), (swap . snd . bounds) m)

dotProd :: Vector -> Vector -> Scalar
dotProd v1 v2 = sum $ zipWith (*) v1' v2'
 where
  v1' = elems v1
  v2' = elems v2

norm :: Vector -> Scalar
norm = flip approxRational (0.0000001 :: Float) . sqrt . fromRational . sum . map (^ (2 :: Int)) . elems

normalize :: Vector -> Vector
normalize v = scalMatMult (1 / norm v) v

printGrid :: (Show a) => Array (Int, Int) a -> IO ()
printGrid a = putStrLn str
 where
  size = (snd . snd . bounds) a
  chunks = chunksOf size (elems a)
  lnes = map (joinWith "  " . map show) chunks
  str = "[  " ++ joinWith "\n|  " lnes ++ "  ]"

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [a] = a
joinWith j (a : as) = a ++ j ++ joinWith j as

identity :: Int -> Matrix
identity n = array ((1, 1), (n, n)) [if i == j then ((i, j), 1) else ((i, j), 0) | i <- [1 .. n], j <- [1 .. n]]