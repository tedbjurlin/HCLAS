module Algorithms where

import Data.Array (array, assocs, bounds, elems, ixmap, listArray, (!))
import Data.Array.Base (amap)
import Data.Ix
import Data.List (find, sortBy)
import Data.List.Split
import Data.Ratio (approxRational, (%))
import Types

-- efHelper :: Int -> Int -> Matrix -> Matrix
-- efHelper r c a =
--   if c > n || r > m
--     then a
--     else
--  where
--   (m, n) = (snd . bounds) a
--   as = chunksOf n (elems a)
--   sl = sortBy (compRows r c) as

compRows :: Int -> Int -> [Scalar] -> [Scalar] -> Ordering
compRows = undefined

swapRows :: (Ix i) => i -> i -> (i, i) -> (i, i)
swapRows r1 r2 (i, j)
  | i == r1 = (r2, j)
  | i == r2 = (r1, j)
  | otherwise = (i, j)

rowSwap :: Matrix -> Int -> Int -> Matrix
rowSwap m r1 r2 = ixmap (bounds m) (swapRows r1 r2) m

rowScale :: Matrix -> Int -> Scalar -> Matrix
rowScale m r s = array (bounds m) (map f ms)
 where
  ms = assocs m
  f ((i, j), e) =
    if i == r
      then ((i, j), e * s)
      else ((i, j), e)

rowReplace :: Matrix -> Int -> Int -> Scalar -> Matrix
rowReplace m r1 r2 s = array (bounds m) (map f ms)
 where
  ms = assocs m
  f ((i, j), e) =
    if i == r1
      then ((i, j), e * s * (m ! (r2, j)))
      else ((i, j), e)

qrAlgo :: Matrix -> Matrix
qrAlgo a = case find (isUpperTriangular (1 % 100000)) $ iterate qrStep a of
  (Just m) -> amap (flip approxRational (0.00001 :: Float) . fromRational) m
  Nothing -> error "Impossible termination of infinite list"

isUpperTriangular :: Scalar -> Matrix -> Bool
isUpperTriangular epsilon a = all (\(_, s) -> s <= epsilon) (filter (\((i, j), _) -> i > j) as)
 where
  as = assocs a

qrStep :: Matrix -> Matrix
qrStep a = matMult r q
 where
  q = gramSchmidt a
  r = consUpTriag q a

consUpTriag :: Matrix -> Matrix -> Matrix
consUpTriag q a = joinVectors $ zipWith (consColumn qs) [1 ..] as
 where
  as = columns a
  qs = columns q

consColumn :: [Vector] -> Int -> Vector -> Vector
consColumn qs i a = listArray ((1, 1), (1, s)) $ take s (take i (map (`dotProd` a) qs) ++ repeat 0)
 where
  s = (snd . snd . bounds) a

gramSchmidt :: Matrix -> Matrix
gramSchmidt a = (joinVectors . map normalize . zipWith ($) projs) as
 where
  as = columns a
  projs = gsScan as

gsScan :: [Vector] -> [Vector -> Vector]
gsScan = scanl gsComb id

gsComb :: (Vector -> Vector) -> Vector -> (Vector -> Vector)
gsComb a vprev v = genMatAdd (-) (a v) (gsProj (a vprev) v)

gsProj :: Vector -> Vector -> Vector
gsProj u v = scalMatMult (dotProd v u / dotProd u u) u

columns :: Matrix -> [Vector]
columns a = map (listArray ((1, 1), (1, j))) (chunksOf j (elems a'))
 where
  (_, (_, j)) = bounds a'
  a' = transpose a

joinVectors :: [Vector] -> Matrix
joinVectors vs = transpose $ listArray ((1, 1), (length vs, s)) (concatMap elems vs)
 where
  s = (snd . snd . bounds . head) vs