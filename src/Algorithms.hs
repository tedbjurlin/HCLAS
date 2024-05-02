module Algorithms where

import Data.Array (array, assocs, bounds, elems, ixmap, listArray, (!))
import Data.Array.Base (amap)
import Data.Ix (Ix)
import Data.List (find)
import qualified Data.List as L
import Data.List.Split (chunksOf)
import Data.Ratio (approxRational, (%))
import Types

ef :: Matrix -> (Maybe Scalar, Matrix)
ef = efHelper 1 1 1

efHelper :: Scalar -> Int -> Int -> Matrix -> (Maybe Scalar, Matrix)
efHelper det r c a
  | c > n || r > m =
      if m == n
        then (Just $ det * diag, a)
        else (Nothing, a)
  | a ! (r, c) == 0 = efHelper det r (c + 1) a
  | (r > m) && sl' ! (r + 1, c) == 0 = efHelper det'' (r + 1) (c + 1) sl'
  | otherwise = efHelper det'' (r + 1) (c + 1) m'
 where
  diag = product [a ! (i, i) | i <- [1 .. m]]
  (m, n) = (snd . bounds) a
  (det', sl) = sortMat det r c a
  k = 1 / (sl ! (r, c))
  (det'', sl') = (det' * k, rowScale sl r k)
  m' = zeroRowsEF r c (r + 1) sl'

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

rref :: Matrix -> (Maybe Scalar, Matrix)
rref a = (det, rrefHelper 1 1 efForm)
 where
  (det, efForm) = ef a

rrefHelper :: Int -> Int -> Matrix -> Matrix
rrefHelper i j a
  | i > r || j > c = a
  | a ! (i, j) == 0 = rrefHelper i (j + 1) a
  | otherwise = rrefHelper (i + 1) (j + 1) z
 where
  z = zeroRowsRREF i j (i - 1) a
  (r, c) = (snd . bounds) a

zeroRowsRREF :: Int -> Int -> Int -> Matrix -> Matrix
zeroRowsRREF r c i m =
  if i < 1
    then m
    else zeroRowsRREF r c (i - 1) replaced
 where
  replaced = rowReplace m i r (-k)
  k = m ! (i, c)

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
consUpTriag q a = joinColumns $ zipWith (consColumn qs) [1 ..] as
 where
  as = columns a
  qs = columns q

consColumn :: [Vector] -> Int -> Vector -> Vector
consColumn qs i a = listArray ((1, 1), (1, s)) $ take s (take i (map (`dotProd` a) qs) ++ repeat 0)
 where
  s = (snd . snd . bounds) a

gramSchmidt :: Matrix -> Matrix
gramSchmidt a = (joinColumns . map normalize . zipWith ($) projs) as
 where
  as = columns a
  projs = gsScan as

gsScan :: [Vector] -> [Vector -> Vector]
gsScan = scanl gsComb id

gsComb :: (Vector -> Vector) -> Vector -> (Vector -> Vector)
gsComb a vprev v = genMatAdd (-) (a v) (gsProj (a vprev) v)

gsProj :: Vector -> Vector -> Vector
gsProj u v = scalMatMult (dotProd v u / dotProd u u) u

augment :: Matrix -> Matrix -> Matrix
augment a b = joinColumns (columns a ++ columns b)

splitMat :: Int -> Matrix -> (Matrix, Matrix)
splitMat i m = (fArray (concat (L.transpose l1)), bArray (concat (L.transpose l2)))
 where
  (l1, l2) = splitAt i (chunksOf r (elems (transpose m)))
  fArray = listArray (l, (r, i))
  bArray = listArray (l, (r, c - i))
  (l, (r, c)) = bounds m

columns :: Matrix -> [Vector]
columns a = map (listArray ((1, 1), (1, j))) (chunksOf j (elems a'))
 where
  (_, (_, j)) = bounds a'
  a' = transpose a

joinColumns :: [Vector] -> Matrix
joinColumns vs = transpose $ listArray ((1, 1), (length vs, s)) (concatMap elems vs)
 where
  s = (snd . snd . bounds . head) vs

rows :: Matrix -> [Vector]
rows a = map (listArray ((1, 1), (1, j))) (chunksOf j (elems a))
 where
  (_, (_, j)) = bounds a

joinRows :: [Vector] -> Matrix
joinRows vs = listArray ((1, 1), (length vs, s)) (concatMap elems vs)
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
nullSpace a = map snd $ takeWhile (isZeroVector . (rows tEf !!) . fst) (zip (iterate (flip (-) 1) (r - 1)) (reverse $ rows inv))
 where
  (_, (r, c)) = bounds a
  (tEf, inv) = splitMat c efRes
  (_, efRes) = ef (augment (transpose a) (identity c))

isLinearlyIndependent :: [Vector] -> Bool
isLinearlyIndependent [] = False
isLinearlyIndependent vs = (length vs <= dim vs) && not (isZeroVector r)
 where
  dim = snd . snd . bounds . head
  r = (rows . snd . ef) (joinColumns vs) !! (length vs - 1)

isZeroVector :: Vector -> Bool
isZeroVector = all (== 0) . elems

inverse :: Matrix -> Matrix
inverse m
  | r /= c = error "Cannot take the inverse of a non-square matrix"
  | det == Just 0 = error "Matrix is not invertible"
  | otherwise = (snd . splitMat c) rrefForm
 where
  (_, (r, c)) = bounds m
  comb = augment m (identity r)
  (det, _) = ef m
  (_, rrefForm) = rref comb

pivotPos :: Vector -> Int
pivotPos v = case find ((/= 0) . snd) (zip [1 ..] es) of
  Just (i, _) -> i
  Nothing -> length es + 1
 where
  es = elems v

rank :: Matrix -> Int
rank a = case find ((> r) . snd) (zip [0 ..] ps) of
  Just (i, _) -> i
  Nothing -> r
 where
  (_, (r, _)) = bounds a
  (_, efForm) = ef a
  rs = rows efForm
  ps = map pivotPos rs

nullity :: Matrix -> Int
nullity a = rs - r
 where
  (_, (rs, _)) = bounds a
  r = rank a

isConsistent :: Matrix -> Bool
isConsistent a = case find (== r) ps of
  Just _ -> False
  Nothing -> True
 where
  (_, (r, _)) = bounds a
  (_, efForm) = ef a
  ps = map pivotPos (rows efForm)

columnSpace :: Matrix -> [Vector]
columnSpace a = map ((columns a !!) . flip (-) 1) ps
 where
  (_, (r, _)) = bounds a
  ps = takeWhile (<= r) (map pivotPos (rows (snd $ ef a)))

rowSpace :: Matrix -> [Vector]
rowSpace a = map fst $ takeWhile (not . isZeroVector . snd) (zip (rows a) (rows efForm))
 where
  (_, efForm) = ef a

independentSubset :: [Vector] -> [Vector]
independentSubset [] = []
independentSubset vs = columnSpace (joinColumns vs)

withinSpan :: [Vector] -> Vector -> Bool
withinSpan vs v
  | null vs = False
  | (dim . head) vs /= dim v = error "Vector must have the same dimension as the vector space"
  | otherwise = isConsistent efResult
 where
  dim = snd . snd . bounds
  m = augment (joinColumns vs) v
  efResult = snd (ef m)

spans :: [Vector] -> [Vector] -> Bool
spans a b
  | null a = False
  | null b = True
  | (dim . head) a /= (dim . head) b = error "Vector spaces must have the same dimension"
  | otherwise = length (independentSubset a) == length (independentSubset b)
 where
  dim = snd . snd . bounds

isBasis :: [Vector] -> Bool
isBasis vs = not (null vs) && ((length vs == dim vs) && isLinearlyIndependent vs)
 where
  dim = snd . snd . bounds . head