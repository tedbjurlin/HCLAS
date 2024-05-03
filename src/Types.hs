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

identity :: Int -> Matrix
identity n = array ((1, 1), (n, n)) [if i == j then ((i, j), 1) else ((i, j), 0) | i <- [1 .. n], j <- [1 .. n]]

type ExpressionBlock = [Expression]

data Expression where
  Assignment :: String -> Expression -> Expression
  Variable :: String -> Expression
  ValueExpression :: Value -> Expression
  FunctionExpression :: Function -> ExpressionBlock -> Expression
  BinaryOperation :: BinOp -> Expression -> Expression -> Expression
  Negation :: Expression -> Expression
  deriving (Show)

data Value
  = S Scalar
  | M Matrix
  | V Vector
  | VL [Vector]
  | B Bool
  deriving (Show, Eq)

data Function
  = INVERSE
  | RREF
  | EF
  | SPAN
  | DETERMINANT
  | PROJECT
  | DIM
  | RANK
  | NULLITY
  | IS_CONSISTENT
  | COL
  | ROW
  | NUL
  | SPANS
  | IS_BASIS
  | QR
  | AUGMENT
  | TRANSPOSE
  | ORTHO_BASIS
  | IN_SPAN
  | IS_INDEPENDENT
  | EIGENSPACE
  | IS_EIGENVALUE
  | IS_EIGENVECTOR
  deriving (Show, Enum)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)