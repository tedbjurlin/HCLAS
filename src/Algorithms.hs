module Algorithms where

import Data.Array (array, assocs, bounds, elems, ixmap, listArray, (!))
import Data.Array.Base (amap)
import Data.Ix
import Data.List (find, sortBy)
import Data.List.Split
import Data.Ratio (approxRational, (%))
import Types

ef :: Matrix -> (Maybe Scalar, Matrix)
ef = efHelper 1 1 1

efHelper :: Scalar -> Int -> Int -> Matrix -> (Maybe Scalar, Matrix)
efHelper det r c a
  | c > n || r >= m =
      if m == n
        then (Just $ det * diag, a)
        else (Nothing, a)
  | sl ! (r + 1, c) == 0 = efHelper det r (c + 1) a
  | otherwise = efHelper det' (r + 1) (c + 1) m'
 where
  diag = product [a ! (i, i) | i <- [1 .. m]]
  (m, n) = (snd . bounds) a
  (det', sl) = sortMat det r c a
  m' = zeroRowsEF r c (r + 1) sl

zeroRowsEF :: Int -> Int -> Int -> Matrix -> Matrix
zeroRowsEF r c i m =
  if i > s || m ! (i, c) == 0
    then m
    else zeroRowsEF r c (i + 1) replaced
 where
  replaced = rowReplace m i r (-k)
  k = m ! (i, c) / m ! (r, c)
  s = (fst . snd . bounds) m

sortMat :: Scalar -> Int -> Int -> Matrix -> (Scalar, Matrix)
sortMat det r c m
  | r > (fst . snd . bounds) m = (det, m)
  | ml == r = sortMat det (r + 1) c m
  | otherwise = sortMat (-det) (r + 1) c swap
 where
  swap = rowSwap m r ml
  ml = maxLoc (r + 1) c r (m ! (r, c)) m

maxLoc :: Int -> Int -> Int -> Scalar -> Matrix -> Int
maxLoc r c l s m
  | r > (fst . snd . bounds) m = l
  | abs (m ! (r, c)) > abs s = maxLoc (r + 1) c r (abs (m ! (r, c))) m
  | otherwise = maxLoc (r + 1) c l s m

compRows :: Int -> Int -> (Int, [Scalar]) -> (Int, [Scalar]) -> Ordering
compRows r c (r1, es1) (r2, es2) =
  if r1 <= r || r2 <= r
    then compare r1 r2
    else compare (es1 !! (c - 1)) (es2 !! (c - 1))

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
      then ((i, j), e + s * (m ! (r2, j)))
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

eigenSpace :: Matrix -> Scalar -> [Vector]
eigenSpace a s = nullSpace $ genMatAdd (-) a $ scalMatMult s $ identity n
 where
  n = (snd . snd . bounds) a

isEigenVector :: Vector -> Matrix -> Bool
isEigenVector v a = not $ isLinearlyIndependent [v, matMult a v]

isEigenValue :: Matrix -> Scalar -> Bool
isEigenValue m = isLinearlyIndependent . eigenSpace m

nullSpace :: Matrix -> [Vector]
nullSpace = undefined

-- possibly relies on swapped columns and rows in original code. Vectorlists of row-vectors, not column-vectors
isLinearlyIndependent :: [Vector] -> Bool
isLinearlyIndependent [] = undefined

-- isLinearlyIndependent vs = (length vs <= dim vs) && not (isZeroVector r)
--  where
--   dim = fst . snd . bounds . head
--   r = getRow (snd . ef) (joinVectors vs)

isZeroVector :: Vector -> Bool
isZeroVector = undefined